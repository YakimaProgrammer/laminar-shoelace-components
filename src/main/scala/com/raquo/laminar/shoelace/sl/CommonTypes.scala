package com.raquo.laminar.shoelace.sl

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.codecs.*
import com.raquo.laminar.defs.styles.{traits as s, units as u}
import com.raquo.laminar.keys
import com.raquo.laminar.keys.DerivedStyleProp
import com.raquo.laminar.modifiers.KeySetter
import com.raquo.laminar.modifiers.KeySetter.StyleSetter
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import com.raquo.laminar.keys.EventProcessor
import scala.reflect.ClassTag

class EnhancedEventProp[Ev <: dom.Event, ScalaVal, JsVal: ClassTag](name: String, f: JsVal => ScalaVal) extends EventProp[Ev](name) {
  protected val asEventProcessor = EventProcessor.empty(this)

  def mapToValue: EventProcessor[Ev, ScalaVal] = {
    asEventProcessor.mapRaw((ev, _) => {
      ev
        .target
        .asInstanceOf[js.Dynamic]
        .selectDynamic("value")
        .asInstanceOf[js.UndefOr[Any]]
        .toOption
        .collect { case v if implicitly[ClassTag[JsVal]].runtimeClass.isInstance(v) =>
                     f(v.asInstanceOf[JsVal])
        }
    })
  }

  private def getClassTag[T: ClassTag](value: T): ClassTag[T] = {
    implicitly[ClassTag[T]]
  }
}

trait CommonTypes {
  protected lazy val StringSeperatedArrayCodec = (sep: String) => new Codec[Array[String], js.Array[String] | String] {
    def encode(scalaValue: IterableOnce[String]): js.Array[String] | String = scalaValue.toJSArray
    def encode(scalaValue: Array[String]) = encode(scalaValue.iterator)
    def decode(domValue: js.Array[String] | String): Array[String] = domValue match {
      case arr: js.Array[String] => arr.toArray
      case str: String => str.split(sep)
    }
  }

  // #TODO[API] I should make use of Laminar helpers like lengthAutoStyle in StyleProps.scala,
  //  but they're defined together with the listings in the same traits, and I don't want
  //  to expose all those props. Split them out. Need minor breaking changes in Laminar.

  protected type DSP[V] = DerivedStyleProp[V]

  protected type SS = StyleSetter

  //// #TODO[Perf] I'm not convinced that these cache maps actually have a net benefit.
  ////  - I don't really think that we need reference equality among keys on different components
  ////  - We still refer to attrs as `lazy val`, this doesn't help with that
  ////  - Whether we save any RAM or not depends on which attrs on which components you use
  //
  //private val boolAttrs = js.Dictionary[HtmlAttr[Boolean]]()
  //
  //private val intAttrs = js.Dictionary[HtmlAttr[Int]]()
  //
  //private val stringAttrs = js.Dictionary[HtmlAttr[String]]()

  protected def mappingEventProp[Ev <: dom.Event, ScalaVal, JsVal: ClassTag](name: String, f: JsVal => ScalaVal): EnhancedEventProp[Ev, ScalaVal, JsVal] = EnhancedEventProp(name, f)
  protected def eventProp[Ev <: dom.Event](name: String): EventProp[Ev] = L.eventProp(name)


  protected def stringProp(name: String): HtmlProp[String, _] = L.htmlProp(name, StringAsIsCodec)

  protected def intProp(name: String): HtmlProp[Int, _] = L.htmlProp(name, IntAsIsCodec)

  protected def doubleProp(name: String): HtmlProp[Double, _] = L.htmlProp(name, DoubleAsIsCodec)

  protected def boolProp(name: String): HtmlProp[Boolean, _] = L.htmlProp(name, BooleanAsIsCodec)

  protected def asIsProp[V](name: String): HtmlProp[V, _] = L.htmlProp(name, AsIsCodec[V]())

  /** Maps a JavaScript `string | string[]` to an Array[String] in Scala. For use with components that have a value that is seperated by some charecter (for example, sl-select) */
  protected def stringSeperatedArrayProp(sep: String)(name: String) = L.htmlProp(name, StringSeperatedArrayCodec(sep))

  protected def boolAttr(name: String): HtmlAttr[Boolean] = {
    L.htmlAttr(name, BooleanAsAttrPresenceCodec)
  }

  protected def intAttr(name: String): HtmlAttr[Int] = {
    L.htmlAttr(name, IntAsStringCodec)
  }

  protected def stringAttr(name: String): HtmlAttr[String] = {
    //stringAttrs.getOrElseUpdate(name, L.htmlAttr(name, StringAsIsCodec))
    L.htmlAttr(name, StringAsIsCodec)
  }

  protected def lengthStyle(name: String): StyleProp[String] with u.Length[DSP, Int] = {
    new StyleProp[String](name) with u.Length[DSP, Int]
  }

  protected def colorStyle(name: String): StyleProp[String] with s.Color with u.Color[SS, DSP] = {
    new StyleProp[String](name) with s.Color with u.Color[SS, DSP]
  }

  protected def timeStyle(key: String): StyleProp[String] with u.Time[DSP] = {
    new StyleProp[String](key) with u.Time[DSP]
  }

  protected def lineStyle(key: String): StyleProp[String] with s.Line = {
    new StyleProp[String](key) with s.Line
  }

  protected def doubleStyle(key: String): StyleProp[Double] = {
    new StyleProp[Double](key)
  }

  protected def intStyle(key: String): StyleProp[Int] = {
    new StyleProp[Int](key)
  }
}
