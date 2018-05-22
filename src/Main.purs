module Main where

import Prelude

import Control.Monad.Aff (Aff, delay, forkAff, joinFiber, try)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (randomInt)
import Control.Monad.Reader.Trans (lift)
import Data.Array (length, range, (!!))
import Data.Either (Either(..), either)
import Data.Foreign (MultipleErrors)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst, snd)
import Dispatcher (DispatchEffFn(..))
import Dispatcher.React (ReactProps(..), createComponent, getState, modifyState)
import Network.HTTP.Affjax (AJAX, get)
import React (ReactClass, createClassStateless, createElement)
import ReactNative.API (REGISTER, registerComponent)
import ReactNative.Components.Button (button')
import ReactNative.Components.Image (image)
import ReactNative.Components.ListView (listView', listViewDataSource, rowRenderer)
import ReactNative.Components.Text (text, text', text_)
import ReactNative.Components.TextInput (textInput')
import ReactNative.Components.Touchable (touchableHighlight', touchableOpacity')
import ReactNative.Components.TouchableNativeFeedback (touchableNativeFeedback')
import ReactNative.Components.View (view)
import ReactNative.PropTypes (center, uriSrc)
import ReactNative.PropTypes.Color (black, blue, gray, powderblue, red, rgb, skyblue, steelblue, white)
import ReactNative.Styles (backgroundColor, borderColor, borderWidth, contain, flex, height, maxWidth, padding, paddingBottom, paddingTop, resizeMode, staticStyles, width)
import ReactNative.Styles.Flex (alignItems, column, flexDirection, flexEnd, justifyContent, row, spaceBetween, stretch)
import ReactNative.Styles.Text (color, fontSize, textAlign)
import Simple.JSON (read)
-- import Simple.JSON (read)


-- Example: Text component toggling visivility on touch with some delay
data X = X
myBlinkingComponent :: ReactClass Unit
myBlinkingComponent = createComponent {isVisible: true} render eval
  where
    render this@{isVisible} (ReactProps p) (DispatchEffFn d) =
      text' {style: blinkingTextStyle, onPress: d $ const X} (msg isVisible)

    msg v = if v then "Hello World!" else ""

    blinkingTextStyle = staticStyles
                        [ flex 1
                        , (backgroundColor $ rgb 255 1 1)]

    eval X = do
      {isVisible} <- getState
      fib <- lift $ forkAff do
        delay (Milliseconds 200.0)
        pure (not isVisible)

      newVE <- lift $ try (joinFiber fib)
      modifyState _{isVisible=either (const false) id newVE}
      pure unit


-- Example: Flex Dimensions Basics - based on official react-native tutorial
flexDimensionsBasics :: ReactClass Unit
flexDimensionsBasics = createClassStateless $ const $
    view sheet.container
    [ view sheet.upper []
    , view sheet.mid []
    , view sheet.bottom [] ]
    where
      sheet = {
        container: staticStyles [
          flex 1
        ],
        upper: staticStyles [
          flex 1,
          backgroundColor powderblue
        ],
        mid: staticStyles [
          flex 2,
          backgroundColor skyblue
        ],
        bottom: staticStyles [
          flex 3,
          backgroundColor steelblue
        ]
      }


-- Example: Flex Justify Content - based on official react-native tutorial
flexDimensionsJustifyContent :: ReactClass Unit
flexDimensionsJustifyContent = createClassStateless $ const $
    view sheet.container
    [ view sheet.upper []
    , view sheet.mid []
    , view sheet.bottom [] ]
  where
    sheet = {
      -- Try setting 'justifyContent' to 'center'
      -- Try setting 'flexDirection' to 'row'
      container: staticStyles [
        flex 1,
        flexDirection column,
        justifyContent spaceBetween
      ],
      upper: staticStyles [
        width 50,
        height 50,
        backgroundColor powderblue
      ],
      mid: staticStyles [
        width 50,
        height 50,
        backgroundColor skyblue
      ],
      bottom: staticStyles [
        width 50,
        height 50,
        backgroundColor steelblue
      ]
}

-- Example: Align Items - based on official react-native tutorial
alignItemsBasics :: ReactClass Unit
alignItemsBasics = createClassStateless $ const $
    view sheet.container
    [ view sheet.upper []
    , view sheet.mid []
    , view sheet.bottom [] ]
  where
    sheet = {
      container: staticStyles [
        flex 1,
        flexDirection column,
        justifyContent center,
        alignItems center
      ],
      upper: staticStyles [
        width 50,
        height 50,
        backgroundColor powderblue
      ],
      mid: staticStyles [
        width 50,
        height 50,
        backgroundColor skyblue
      ],
      bottom: staticStyles [
        width 50,
        height 50,
        backgroundColor steelblue
      ]
    }

-- end of layout stuff
-- full list of props is here https://facebook.github.io/react-native/docs/layout-props.html

-- handling text input - based on original react-native tutorial

data Translate = Translate String

handlingTextExample :: ReactClass Unit
handlingTextExample = createComponent initialState render eval
  where
    initialState = {translated: ""}

    render this@{translated} (ReactProps p) (DispatchEffFn d) =
      view sheet.container
      [
        textInput' {
           placeholder: "Type here to translate!",
           onChangeText: d $ Translate,
           style: sheet.textInput
           },
        text sheet.text translated
      ]

    eval (Translate txt) = do
      let transl = trim >>> split (Pattern " ") >>> map (const "🍕") >>> joinWith " "
      modifyState _{translated=transl txt}
      lift affUnit

    sheet = {
      container: staticStyles [
        height 300,
        borderWidth 1,
        borderColor black
        ],
      textInput: staticStyles [
        borderWidth 1,
        borderColor blue,
        padding 15,
        height 100
        ],
      text: staticStyles [
        flex 1,
        borderWidth 1,
        borderColor red,
        padding 15,
        height 200,
        fontSize 42
        ]
      }


affUnit :: forall eff. Aff eff Unit
affUnit = pure unit

--------------------------------
-- Handling Touches - from official tutorial

data Msg = Msg String

buttonExample :: ReactClass Unit
buttonExample = createComponent initialState render eval
  where
    initialState = { msg : "" }

    render {msg} (ReactProps p) (DispatchEffFn d) =
      view sheet.container
      [ text sheet.label msg
      , button' {
          title: "button1",
          onPress : d $ const (Msg "First pressed!"),
          color: powderblue
      }
      , button' {
          title: "button2",
          onPress : d $ const (Msg "Second pressed!"),
          color: skyblue
      }
      ]

    eval (Msg txt) = do
      modifyState _{msg = txt}
      lift $ delay (Milliseconds 2000.0)
      modifyState _{msg = " "}

    sheet = {
      container: staticStyles [
        flex 1,
        flexDirection column,
        justifyContent center,
        alignItems center
      ],
      button1: staticStyles [
        width 50,
        height 50,
        backgroundColor powderblue
      ],
      label: staticStyles [
        fontSize 33
      ]
    }


touchablesExample :: ReactClass Unit
touchablesExample = createComponent initialState render eval
  where
    initialState = { msg : "" }

    render {msg} (ReactProps p) (DispatchEffFn d) =
      view sheet.container
      [ text sheet.label msg
      , touchableHighlight'
          { onPress : d $ const (Msg "highlight pressed")
          , underlayColor: powderblue}
          (text_ "touchableHighlight")
      , touchableOpacity'
          {onPress : d $ const (Msg "opacity pressed")}
          (text_ "touchableOpacity")
      , touchableNativeFeedback'
          {onPress : d $ const (Msg "native feedback pressed")}
          (text_ "touchableNativeFeedback")
      ]

    eval (Msg txt) = do
      modifyState _{msg = txt}
      lift $ delay (Milliseconds 2000.0)
      modifyState _{msg = " "}

    sheet = {
      container: staticStyles [
        flex 1,
        flexDirection column,
        justifyContent center,
        alignItems center
      ],
      button1: staticStyles [
        width 50,
        height 50,
        backgroundColor powderblue
      ],
      label: staticStyles [
        fontSize 33
      ]
    }

---------- Using a ListView

-- flatListExample :: ReactClass Unit
flatListExample :: ReactClass Unit
flatListExample = createComponent unit render unit
  where
    render this (ReactProps p) = view sheet.container [
        listView'
          { dataSource: items
          , renderRow: rowRenderer renderRow}
        ]

    renderRow {key} = text_ key

    items = listViewDataSource $ map (\x -> {key: (show x <> ". 🍕")}) (range 1 100)

    sheet = { container: staticStyles [] }


---------- Interacting with json api

type XkcdComicJson =
  { month :: String
  , num :: Number
  , link :: String
  , year :: String
  , news :: String
  , safe_title :: String
  , transcript :: String
  , alt :: String
  , img :: String
  , title :: String
  , day :: String
}

type XkcdComicJson' r =
  { month :: String
  , num :: Number
  , link :: String
  , year :: String
  , news :: String
  , safe_title :: String
  , transcript :: String
  , alt :: String
  , img :: String
  , title :: String
  , day :: String
  | r }


data XkcdLoader = LoadRandomXkcd

-- flatListExample :: ReactClass Unit
randomXkcdComic :: ReactClass Unit
randomXkcdComic = createComponent { comicJson: Nothing } render eval
  where
    render { comicJson } (ReactProps p) (DispatchEffFn d) =
      view sheet.container
      [
        view sheet.comicSelectContainer
        [
          button' {
              title: "Random XKCD",
              onPress : d $ const LoadRandomXkcd,
              color: skyblue
          }

        ]
      ,  view sheet.comicDisplayContainer
        [
          case comicJson of
            Nothing -> text_ "Comic not loaded"
            Just cmcJson -> image sheet.comicImage $ uriSrc cmcJson.img
            -- Just cmcJson -> view sheet.container
            --   [ text_ cmcJson.transcript
            --   , image sheet.comicImage $ uriSrc cmcJson.img
            --   ]
        ]
      ]

    eval LoadRandomXkcd = do
      num <- lift $ liftEff $ randomInt 0 2000
      newState <- eitherToMaybe <$> (lift $ fetchComic num)
      modifyState _{comicJson=newState}


    fetchComic :: forall eff. Int -> Aff ( ajax::AJAX | eff ) (Either MultipleErrors XkcdComicJson)
    fetchComic num = (_.response >>> read) <$> get (reqUrl num)

    reqUrl num = "https://xkcd.com/" <> show num <> "/info.0.json"

    eitherToMaybe = case _ of
      Right x -> Just x
      Left _ -> Nothing



    sheet =
      { container: staticStyles
        [ flex 1
        , flexDirection column
        , alignItems stretch ]
      , comicSelectContainer: staticStyles []
      , comicDisplayContainer: staticStyles
        [ flex 1
        , justifyContent center
        , flexDirection column
        , alignItems stretch ]
      , comicImage: staticStyles
        [ flex 1
        , resizeMode contain ]
      }


----------------------------------
--  navigation

type Examples = Array (Tuple String (ReactClass Unit))
data SelectExampleIx = SelectExampleIx Int

navigationHeader :: ReactClass { examples :: Examples }
navigationHeader = createComponent {activeExampleIx: 0} render eval
  where

    incExample i p = if i+1 >= (length p.examples) then 0 else i+1
    decExample i p = if i-1 < 0 then (length p.examples - 1) else i-1

    render {activeExampleIx} (ReactProps p) (DispatchEffFn d) =
      view sheet.container
      [
        -- navigation header
        view sheet.header
        [
          -- previous button
          view sheet.button
          [ button'
            { title: "previous"
            , onPress : d $ const (SelectExampleIx (decExample activeExampleIx p))
            , color: skyblue }
          ]
          -- header title and example number
        , view sheet.headerTitleContainer
          [
            text sheet.exampleCounter $
              show (activeExampleIx + 1) <> "/" <> show (length p.examples)
          , text sheet.exampleTitle $
              case p.examples !! activeExampleIx of
                Just ex → fst ex
                Nothing → "Couldn't load example"

          ]
          -- next button
        , view sheet.button [ button'
          { title: "next"
          , onPress : d $ const (SelectExampleIx (incExample activeExampleIx p))
          , color: skyblue } ]

        ]
      -- container for examples
      , view sheet.exampleContainer
        [ let mex = p.examples !! activeExampleIx in case mex of
            Just ex → createElement (snd ex) unit []
            Nothing → text_ "Couldn't load example"
        ]
      ]

    eval (SelectExampleIx x) = do
        modifyState _{activeExampleIx = x}
        lift affUnit

    sheet =
      { container : staticStyles
        [ paddingTop 20
        , flex 1
        , backgroundColor gray ]
      , exampleContainer : staticStyles
        [ flex 1
        , backgroundColor white]
      , header : staticStyles
        [ height 70
        , flexDirection row
        , justifyContent spaceBetween ]
      , headerTitleContainer : staticStyles
        [ flex 1
        , flexDirection column
        , paddingBottom 10 ]
      , button : staticStyles
        [ flex 1
        , justifyContent flexEnd
        , flexDirection column ]
      , exampleTitle : staticStyles
        [ flex 2
        , color white
        , textAlign center
        , fontSize 18]
      , exampleCounter : staticStyles
        [ flex 1
        , textAlign center
        , color white
        , fontSize 12]
      }


app :: ReactClass Unit
app = createClassStateless $ const $
      createElement navigationHeader
        {examples :
         [ Tuple "blinking component" myBlinkingComponent
         , Tuple "flex dimensions basics" flexDimensionsBasics
         , Tuple "flex dimensions justify" flexDimensionsJustifyContent
         , Tuple "handling text" handlingTextExample
         , Tuple "buttons" buttonExample
         , Tuple "touchables" touchablesExample
         , Tuple "flatlist" flatListExample
         , Tuple "fetch random xkcd" randomXkcdComic ]} []

main :: forall eff. Eff ( register :: REGISTER | eff) Unit
main = registerComponent "purescriptReactnativeTutorialExamples" app
