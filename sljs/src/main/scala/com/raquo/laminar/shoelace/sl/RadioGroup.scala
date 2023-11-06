package com.raquo.laminar.shoelace.sl

import com.raquo.laminar.keys.{EventProp, HtmlAttr}
import com.raquo.laminar.api.L.*
import com.raquo.laminar.defs.styles.{traits as s, units as u}
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

// This file is generated at compile-time by ShoelaceGenerator.scala

/** [[https://shoelace.style/components/radio-group Shoelace RadioGroup docs]] */
object RadioGroup extends WebComponent("sl-radio-group") {

  @JSImport("@shoelace-style/shoelace/dist/components/radio-group/radio-group.js")
  @js.native object RawImport extends js.Object

  type Ref = dom.HTMLElement


  // -- Events --

  /** Emitted when the radio group's selected value changes. */
  lazy val onChange: EventProp[dom.Event] = eventProp("sl-change")

  /** Emitted when the radio group receives user input. */
  lazy val onInput: EventProp[dom.Event] = eventProp("sl-input")

  /** Emitted when the form control has been checked for validity and its constraints aren't satisfied. */
  lazy val onInvalid: EventProp[dom.Event] = eventProp("sl-invalid")


  // -- Props --


  // -- Attributes --

  /**
    * The radio group's label. Required for proper accessibility. If you need to display HTML, use the `label` slot
    * instead.
    */
  lazy val label: HtmlAttr[String] = stringAttr("label")

  /** The radio groups's help text. If you need to display HTML, use the `help-text` slot instead. */
  lazy val helpText: HtmlAttr[String] = stringAttr("help-text")

  /** The name of the radio group, submitted as a name/value pair with form data. */
  lazy val name: HtmlAttr[String] = stringAttr("name")

  /** The current value of the radio group, submitted as a name/value pair with form data. */
  lazy val defaultValue: HtmlAttr[String] = stringAttr("value")

  /** The radio group's size. This size will be applied to all child radios and radio buttons. */
  lazy val size: CommonKeys.size.type = CommonKeys.size

  /**
    * By default, form controls are associated with the nearest containing `<form>` element. This attribute allows you
    * to place the form control outside of a form and associate it with the form that has this `id`. The form must be in
    * the same document or shadow root for this to work.
    */
  lazy val formId: HtmlAttr[String] = stringAttr("form")

  /** Ensures a child radio is checked before allowing the containing form to submit. */
  lazy val required: HtmlAttr[Boolean] = boolAttr("required")


  // -- Slots --

  object slots {

    /** The default slot where `<sl-radio>` or `<sl-radio-button>` elements are placed. Note: You can just say `_ => element` instead of `_.slots.default(element)` */
    lazy val default: Slot = Slot("")

    /** The radio group's label. Required for proper accessibility. Alternatively, you can use the `label` attribute. */
    lazy val label: Slot = Slot("label")
  }


  // -- CSS Vars --

  /** This component has no CSS vars / custom properties. */
  @inline def noCssVars: Unit = ()


  // -- CSS Parts --

  /** For documentation only. You need to style these from a CSS stylesheet. */
  object cssParts {

    /** The form control that wraps the label, input, and help text. */
    lazy val formControl: String = "form-control"

    /** The label's wrapper. */
    lazy val formControlLabel: String = "form-control-label"

    /** The input's wrapper. */
    lazy val formControlInput: String = "form-control-input"

    /** The help text's wrapper. */
    lazy val formControlHelpText: String = "form-control-help-text"

    /** The button group that wraps radio buttons. */
    lazy val buttonGroup: String = "button-group"

    /** The button group's `base` part. */
    lazy val buttonGroup__base: String = "button-group__base"
  }


}
