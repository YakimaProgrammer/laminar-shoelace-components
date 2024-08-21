package com.raquo.laminar.shoelace.sl

import com.raquo.laminar.keys.{EventProp, HtmlAttr, StyleProp}
import com.raquo.laminar.api.L
import com.raquo.laminar.defs.styles.{traits as s, units as u}
import com.raquo.laminar.nodes.Slot
import com.raquo.laminar.codecs.*
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.|
import scala.scalajs.js.annotation.JSImport

// This file is generated at compile-time by ShoelaceGenerator.scala

/**
  * Compare visual differences between similar photos with a sliding panel.
  * 
  * [[https://github.com/raquo/laminar-shoelace-components/blob/master/src/main/scala/com/raquo/laminar/shoelace/sl/ImageComparer.scala ImageComparer.scala source code]]
  * 
  * [[https://shoelace.style/components/image-comparer Shoelace ImageComparer docs]]
  */
object ImageComparer extends WebComponent("sl-image-comparer") {

  @JSImport("@shoelace-style/shoelace/dist/components/image-comparer/image-comparer.js", JSImport.Namespace)
  @js.native object RawImport extends js.Object

  type Self = ImageComparer.type

  type Ref = ImageComparerComponent with dom.HTMLElement


  // -- Events --


  // -- Attributes --

  /** The position of the divider as a percentage. */
  lazy val position: HtmlAttr[Int] = intAttr("position")


  // -- Props --


  // -- Slots --

  object slots {

    /** The before image, an `<img>` or `<svg>` element. */
    lazy val before: Slot = Slot("before")

    /** The after image, an `<img>` or `<svg>` element. */
    lazy val after: Slot = Slot("after")

    /** The icon used inside the handle. */
    lazy val handle: Slot = Slot("handle")
  }


  // -- CSS Vars --

  /** The width of the dividing line. */
  lazy val dividerWidth: StyleProp[String] with u.Length[DSP, Int] = lengthStyle("--divider-width")

  /** The size of the compare handle. */
  lazy val handleSize: StyleProp[String] with u.Length[DSP, Int] = lengthStyle("--handle-size")


  // -- CSS Parts --

  /** For documentation only. You need to style these from a CSS stylesheet. */
  object cssParts {

    /** The component's base wrapper. */
    lazy val base: String = "base"

    /** The container that wraps the before image. */
    lazy val before: String = "before"

    /** The container that wraps the after image. */
    lazy val after: String = "after"

    /** The divider that separates the images. */
    lazy val divider: String = "divider"

    /** The handle that the user drags to expose the after image. */
    lazy val handle: String = "handle"
  }


  // -- Element type -- 

  @js.native trait ImageComparerComponent extends js.Object { this: dom.HTMLElement => 

    /** The position of the divider as a percentage. */
    var position: Int
  }
}
