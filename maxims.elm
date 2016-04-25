import Graphics.Element exposing (..)
import Time exposing (..)
import Array exposing (..)
import Maybe exposing (..)
import Text exposing (Text, bold, monospace, fromString)
import Color
import Window
import Mouse
import Keyboard


-- Hard data

maxims = fromList ["All programmers are API designers", "APIs can be among your greatest assets or liabilities", "Public APIs, like diamonds, are forever", "APIs should be easy to use and hard to misuse", "APIs should be self-documenting", "When designing an API, first gather requirements - with a healthy degree of skepticism", "Structure requirements as use-cases", "Early drafts of APIs should be short", "Code the use-cases against your API before you implement it", "Maintain the code for uses-cases as the API evolves", "Example code should be exemplary", "You can't please everyone so aim to displease everyone equally", "Expect API-design mistakes due to failures of imagination", "API design is not a solitary activity", "Avoid fixed limits on input sizes", "If it's hard to find good names, go back to the drawing board", "Names matter", "When in doubt, leave it out", "Minimizing conceptual weight is more important than class- or method-count.", "Keep APIs free of implementations details", "Minimize mutability", "Documentation matters", "Consider the performance consequences of API design decisions", "APIs must coexist peacefully with the platform, so do what is customary", "Minimize accessibility; when in doubt, make it private", "Subclass only if you can say with a straight face that every instance of the subclass is an instance of the superclass", "Design and document for inheritance or else prohibit it", "Don't make the client do anything the library could do", "Obey the principle of least astonishment", "Fail fast", "Provide programmatic access to all data available in string form", "Overload with care", "Use the right data type for the job", "Use consistent parameter ordering across methods", "Avoid long parameter lists", "Avoid return values that demand exceptional processing", "Throw exceptions only to indicate exceptional conditions", "Throw unchecked exceptions unless clients can realistically recover from the failure", "API design is an art, not a science"]
details = fromList ["Good programs are modular, and intermodular boundaries define APIs. Good modules get reused.", "Good APIs create long-term customers; bad ones create long-term support nightmares.", "You have one chance to get it right so give it your best.", "It should be easy to do simple things; possible to do complex things; and impossible, or at least difficult, to do wrong things.", "It should rarely require documentation to read code written to a good API. In fact, it should rarely require documentation to write it.", "People often provide solutions; it's your job to ferret out the underlying problems and find the best solutions.", "They are the yardstick against which you'll measure your API.", "Typically one page with class and method signatures and one-line descriptions. This makes it easy to restructure the API when you don't get it right the first time.", "Even before you specify it properly. This will save you from implementing, or even specifying, a fundamentally broken API.", "Not only will this protect you from rude surprises, but the resulting code will become the examples for the API, the basis for tutorials and tests.", "If an API is used widely, its examples will be the archetypes for thousands of programs. Any mistakes will come back to haunt you a thousand fold.", "Most APIs are overconstrained.", "You can't reasonably hope to imagine everything that everyone will do with an API, or how it will interact with every other part of a system.", "Show your design to as many people as you can, and take their feedback seriously. Possibilities that elude your imagination may be clear to others.", "They limit usefulness and hasten obsolescence.", "Don't be afraid to split or merge an API, or embed it in a more general setting. If names start falling into place, you're on the right track.", "Strive for intelligibility, consistency, and symmetry. Every API is a little language, and people must learn to read and write it. If you get an API right, code will read like prose.", "If there is a fundamental theorem of API design, this is it. It applies equally to functionality, classes, methods, and parameters. Every facet of an API should be as small as possible, but no smaller. You can always add things later, but you can't take them away.", "", "They confuse users and inhibit the flexibility to evolve. It isn't always obvious what's an implementation detail: Be wary of overspecification.", "Immutable objects are simple, thread-safe, and freely sharable.", "No matter how good an API, it won't get used without good documentation. Document every exported API element: every class, method, field, and parameter.", "But don't warp an API to achieve performance gains. Luckily, good APIs typically lend themselves to fast implementations.", "It is almost always wrong to 'transliterate' an API from one platform to another.", "This simplifies APIs and reduces coupling.", "Exposed classes should never subclass just to reuse implementation code.", "This documentation takes the form of self-use patterns: how methods in a class use one another. Without it, safe subclassing is impossible.", "Violating this rule leads to boilerplate code in the client, which is annoying and error-prone.", "Every method should do the least surprising thing it could, given its name. If a method doesn't do what users think it will, bugs will result.", "The sooner you report a bug, the less damage it will do. Compile-time is best. If you must fail at run-time, do it as soon as possible.", "Otherwise, programmers will be forced to parse strings, which is painful. Worse, the string forms will turn into de facto APIs.", "If the behaviors of two methods differ, it's better to give them different names.", "For example, don't use string if there is a more appropriate type.", "Otherwise, programmers will get it backwards.", "Especially those with multiple consecutive parameters of the same type.", "Clients will forget to write the special-case code, leading to bugs. For example, return zero-length arrays or collections rather than nulls.", "Otherwise, clients will be forced to use exceptions for normal flow control, leading to programs that are hard to read, buggy, or slow.", "", "Strive for beauty, and trust your gut. Do not adhere slavishly to the above heuristics, but violate them only infrequently and with good reason."]
size = min (length maxims) (length details)

sepStr = """

o      o      o

"""


main : Signal Element
main =
  Signal.map2 view Window.dimensions position


-- Signals

arrows =
  Signal.map .x Keyboard.arrows
-- signals 1 on each click and each right arrow pressed and every 30 seconds
scheduled =
  Signal.map (always 1) (every (30 * second))
clicks =
  Signal.map (always 1) Mouse.clicks
position =
  Signal.mergeMany [arrows, clicks, scheduled]
    |> Signal.foldp (+) 0


-- View building

view : (Int,Int) -> Int -> Element
view (w,h) textId =
  let
    footer = "Maxims property of Joshua Bloch"
      |> Text.fromString
      |> Text.height 12
      |> Text.color (Color.rgb 160 160 160)
      |> centered
    
    footerH = heightOf footer

    content = List.map (\fct -> fct textId) [ maxim, sep, detail ]
      |> List.map centered
      |> List.map (width (w // 5 * 4))
      |> flow down
      |> container w (h - footerH) middle

  in
    flow down [content, container w footerH midRight footer]



getText style texts textId =
  get (textId % size) texts
    |> withDefault "No text for you"
    |> Text.fromString
    |> style

sep : Int -> Text
sep =
  always (fromString sepStr)

maxim : Int -> Text
maxim =
  getText maximStyle maxims


detail : Int -> Text
detail =
  getText detailStyle details


maximStyle : Text -> Text
maximStyle e =
  e |> bold |> monospace |> Text.height 60


detailStyle : Text -> Text
detailStyle e =
  e |> Text.height 30