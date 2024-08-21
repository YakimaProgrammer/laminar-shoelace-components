package com.raquo.laminar.shoelace.sl

import org.scalajs.dom
import scala.scalajs.js
import scala.reflect.ClassTag
import com.raquo.laminar.keys.{EventProcessor, EventProp}

class EnhancedEventProp[Ev <: dom.Event, ScalaVal, JsVal: ClassTag](name: String, mapper: JsVal => ScalaVal, selector: js.Dynamic => js.Dynamic) extends EventProp[Ev](name) {
  protected val asEventProcessor = EventProcessor.empty(this)

  def mapToValue: EventProcessor[Ev, ScalaVal] = {
    asEventProcessor.mapRaw((ev, _) => {
        selector(ev.asInstanceOf[js.Dynamic])
        .asInstanceOf[js.UndefOr[Any]]
        .toOption
        // As an implementation note, this matches against the isInstance "subclasses" of JsVal, rather than everything that can implicitly convert to JsVal. For example, a Byte will not match against an Int, but a Array[String] will match against a `String | Array[String]`
        .collect { case v if implicitly[ClassTag[JsVal]].runtimeClass.isInstance(v) =>
                     mapper(v.asInstanceOf[JsVal])
        }
    })
  }
}
object EnhancedEventProp {
  def stringProp[Ev <: dom.Event](name: String) = new EnhancedEventProp[Ev, String, String](name, identity[String], _.selectDynamic("target").selectDynamic("value"))
  def valueProp[Ev <: dom.Event, ScalaVal, JsVal: ClassTag](name: String, mapper: JsVal => ScalaVal) = new EnhancedEventProp[Ev, ScalaVal, JsVal](name, mapper, _.selectDynamic("target").selectDynamic("value"))
}
