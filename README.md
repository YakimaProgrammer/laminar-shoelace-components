![](https://laminar.dev/img/brand/laminar-logo-100px-rounded.png)

# Laminar Bindings for Shoelace.js Web Components

[![Chat on https://discord.gg/JTrUxhq7sj](https://img.shields.io/badge/chat-on%20discord-7289da.svg)](https://discord.gg/JTrUxhq7sj)
[![Maven Central](https://img.shields.io/maven-central/v/com.raquo/laminar-shoelace_sjs1_3.svg)](https://search.maven.org/artifact/com.raquo/laminar-shoelace_sjs1_3)


## What

[Shoelace.js](https://shoelace.style/) is a very nice library of modern [Web Components](https://developer.mozilla.org/en-US/docs/Web/API/Web_Components).

Web Components like this  can be used from any UI library (React, Vue, etc.), with some integration work.

This project lets you use Shoelace.js components from [Laminar](https://laminar.dev/), my UI library for Scala.js.

On the build side, this project is a parser, a translator, and a generator. It parses Shoelace's `custom-elements.json` manifest file, translates it into a Scala.js-friendly data structure, and outputs Laminar code for all Shoelace components. Because the generator is customizable (and the translator is the hard part anyway), this can bring Shoelace support to other Scala.js UI libraries for very cheap (now that we have it). We could also try parsing other Web Component libraries, although that would take more work.


## Status

WIP. For now, you can play with `0.1.0` if you really want to, but remember it's an early preview. You can see it in action in [Laminar demo](https://demo.laminar.dev/app/integrations/web-components/shoelace).

Top TODO-s (two of many):

- output instance methods (if you want to help, this may be a good place to start)
- support complex/non-standard attrs and props


## Project Structure

### Generator Overview

The bindings offered here were produced using semi-automatic code generation. It wasn't easy, but there are a few significant upsides to it:

1. High quality output: pretty much indistinguishable from manually-built web component bindings, with concise yet precise types that feel right at home in Scala.js and Laminar.

2. It should be pretty easy to generate Shoelace component bindings for other Scala.js UI libraries

3. It should be possible, not sure how easy, to generate similar bindings for other component libraries such as Material UI.

4. It should be very easy to maintain the Scala.js bindings going forward, as we can just re-run the generator whenever the upstream component libraries update.

The project isn't yet fully set up to fulfill this vision. Some of it will need to be moved to Scala DOM Types and perhaps generalized some more, to enable reusability.

* `custom-elements.json` – [Custom Elements Manifest](https://github.com/webcomponents/custom-elements-manifest) generated by Shoelace.js. It lists and describes the types of their elements, attributes, properties, methods, etc. This file is found neither in this project nor in the shoelace project. You need to run `npm install` to obtain it.  

* `project/CustomElementsManifest.scala` – Scala data structure describing Shoelace's `custom-elements.json` file. We deserialize the JSON file into this structure using uPickle.

* `project/ShoelaceTranslator.scala` – Converts `CustomElementsManifest` above into a `WebComponentsDef` (see `project/WebComponentsDef.scala`), which is our own data structure that contains all the data that we need to generate Scala code. It's similar in structure to `CustomElementsManifest`, but with many things parsed, cleaned up, fixed up, and formatted to better suit our purpose.

* `ShoelaceTranslator` encodes our knowledge of the Shoelace.js library, knowledge about encoding JS types in Scala types, and various decisions relating to edge cases. At the moment, those are inseparable from each other, as I didn't have any non-Shoelace components to work with.

* This `ShoelaceTranslator` is supposed to be Laminar-agnostic (not sure how much I succeeded, let me know), and should be compatible with any library that gets its definitions from [Scala DOM Types](https://github.com/raquo/scala-dom-types), and even that isn't really a hard requirement.

* Finally, `project/ShoelaceGenerator.scala` is responsible for formatting and outputting the code. Currently it prints out only Laminar code, but I think it can be made customizeable enough to support any Scala.js UI library.

* Aside from the component files, the generator produces `EventTypes.scala` – Shoelace's custom event types. The data to produce it is missing from `custom-manifest.json`, I defined it manually in `ShoelaceTranslator` by manually translating their typescript types – it's not too much.

* The generated code assumes the existence of the following manually created support files: `CommonKeys.scala`, `CommonTypes.scala`, `WebComponent.scala`, `ControlledInput.scala`, `Slot.scala`. They provide some of the components' public API, private helper methods, and provide manual implementations of some common / special Shoelace attributes.

---

To summarize, I think in the best case, pretty much all of this can live in a shared repo (Scala DOM Types?), from which people could generate Shoelace bindings for their Scala.js UI libraries. Well, worst case, if `ShoelaceGenerator` is not flexible enough, you might need to output Scala code yourself. But that's the easy part, having this generator as a reference.

As for supporting other Web Component libraries like Material UI, I assume all of them publish `custom-element.json`, so technically our pipeline should work on them too. However, `ShoelaceGenerator` is not very robust yet. I'm not sure how many Shoelace-specific assumptions are baked into it. There definitely _is_ some weirdness in how Shoelace populates its `custom-elements.json`, and I'm not sure that other libraries' weirdness will match up. 

Discussion about generalizing Web Components support in the Scala.js ecosystem is here: [SDT#97](https://github.com/raquo/scala-dom-types/issues/97).


## Author

Nikita Gazarov – [@raquo](https://twitter.com/raquo)


## License

This project is provided under the MIT license.
