port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Events exposing (on, preventDefaultOn, stopPropagationOn)
import Json.Decode as Json
import Process
import Task

port setPointerCapture : ( String, Int ) -> Cmd msg
port copyToClipboard : String -> Cmd msg
port acknowledgeClipboardSuccess : (() -> msg) -> Sub msg
port acknowledgeClipboardFailure : (String -> msg) -> Sub msg


type alias PointerEvent =
    { pointerId : Int
    , offsetX : Float
    , offsetY : Float
    }


type ScrollDirection
    = ScrollDown
    | ScrollUp


type alias Rgb =
    { red : Int
    , green : Int
    , blue : Int
    }
    
    
type alias Hsl =
    { hue : Float
    , saturation : Float
    , lightness : Float
    }


type alias Vector2 a =
    ( a, a )
    
    
type PickerElement
    = ColorBox
    | LightnessSlider


type ClipboardTarget
    = HSLOutput
    | RGBOutput


type ClipboardStatus
    = ClipboardIdle
    | ClipboardAwaiting ClipboardTarget
    | ClipboardSuccess ClipboardTarget
    | ClipboardFailure ClipboardTarget String


type alias Model =
    { pickerPosition : Vector2 Float
    , lightnessPosition : Float
    , dragging : Maybe PickerElement
    , hueValue : String
    , satValue : String
    , lightValue : String
    , redValue : String
    , greenValue : String
    , blueValue : String
    , clipboardStatus : ClipboardStatus
    }
    
    
type alias Flags =
    ()


initialModel : Model
initialModel =
    let
        initialRgb =
            hslToRgb
                { hue = 0
                , saturation = 1
                , lightness = 0.5
                }
    in
        { pickerPosition = ( 0, 1 )
        , lightnessPosition = 0.5
        , dragging = Nothing
        , hueValue = "0"
        , satValue = "100"
        , lightValue = "50"
        , redValue = String.fromInt initialRgb.red
        , greenValue = String.fromInt initialRgb.green
        , blueValue = String.fromInt initialRgb.blue
        , clipboardStatus = ClipboardIdle
        }
    

init : Flags -> ( Model, Cmd Msg )
init () = 
    ( initialModel, Cmd.none )


type Msg
    = Grab PickerElement PointerEvent
    | Release
    | Drag (Vector2 Float)
    | ChangeHue String
    | ChangeSaturation String
    | ChangeLightness String
    | ChangeRed String
    | ChangeGreen String
    | ChangeBlue String
    | ScrollLightnessSlider ScrollDirection
    | CopyToClipboard ClipboardTarget
    | AcknowledgeClipboardSuccess
    | AcknowledgeClipboardFailure String
    | ClearClipboardStatusInfo
    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Grab grabbedElement thePointer ->
            (
                { model | dragging = Just grabbedElement }
            ,
                setPointerCapture ( pickerId grabbedElement, thePointer.pointerId )
            )
            
        Release ->
            ( { model | dragging = Nothing }, Cmd.none )

        Drag mousePosition ->
            ( updatePositions mousePosition model, Cmd.none )
                
        ChangeHue newHueValue ->
            let
                model0 =
                    { model | hueValue = newHueValue }
            in
            case String.toInt newHueValue of
                Nothing ->
                    ( model0, Cmd.none )
                    
                Just newHue ->
                    (
                        { model0
                        | pickerPosition =
                            ( toFloat (newHue |> modBy 360) / 360, Tuple.second model.pickerPosition )
                        }
                        |> adjustRgb
                    ,
                        Cmd.none
                    )
                    
        ChangeSaturation newsatValue ->
            let
                model0 =
                    { model | satValue = newsatValue }
            in
            case String.toInt newsatValue of
                Nothing ->
                    ( model0, Cmd.none )
                    
                Just newSaturation ->
                    (
                        { model0
                        | pickerPosition =
                            ( Tuple.first model.pickerPosition, clamp 0 1 (toFloat newSaturation / 100) )
                        }
                        |> adjustRgb
                    , Cmd.none
                    )
                    
        ChangeLightness newlightValue ->
            let
                model0 =
                    { model | lightValue = newlightValue }
            in
            case String.toInt newlightValue of
                Nothing ->
                    ( model0, Cmd.none )
                    
                Just newLightness ->
                    ( adjustRgb { model0 | lightnessPosition = clamp 0 1 (toFloat newLightness / 100) }, Cmd.none )
                    
        ChangeRed newRedValue ->
            ( adjustHsl { model | redValue = newRedValue }, Cmd.none )

        ChangeGreen newGreenValue ->
            ( adjustHsl { model | greenValue = newGreenValue }, Cmd.none )

        ChangeBlue newBlueValue ->
            ( adjustHsl { model | blueValue = newBlueValue }, Cmd.none )

        ScrollLightnessSlider scrollDirection ->
            case model.dragging of
                Nothing ->
                    (
                        { model
                        | lightnessPosition =
                            clamp 0 1
                                (model.lightnessPosition
                                + case scrollDirection of
                                    ScrollUp ->
                                        scrollSensitivity
                                    
                                    ScrollDown ->
                                        -scrollSensitivity)
                        }
                    ,
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CopyToClipboard clipboardTarget ->
            ( { model | clipboardStatus = ClipboardAwaiting clipboardTarget }, copyToClipboard (clipboardId clipboardTarget) )

        AcknowledgeClipboardSuccess ->
            case model.clipboardStatus of
                ClipboardAwaiting target ->
                    ( { model | clipboardStatus = ClipboardSuccess target }, clearClipboardStatus )
                
                _ ->
                    ( model, Cmd.none )

        AcknowledgeClipboardFailure errorMessage ->
            case model.clipboardStatus of
                ClipboardAwaiting target ->
                    ( { model | clipboardStatus = ClipboardFailure target errorMessage }, clearClipboardStatus )

                _ ->
                    ( model, Cmd.none )

        ClearClipboardStatusInfo ->
            ( { model | clipboardStatus = ClipboardIdle }, Cmd.none )


clearClipboardStatus : Cmd Msg
clearClipboardStatus =
    Process.sleep clipboardStatusTimeoutMs
    |> Task.perform (\ _ -> ClearClipboardStatusInfo)


clipboardStatusTimeoutMs : Float
clipboardStatusTimeoutMs =
    1000


scrollSensitivity : Float
scrollSensitivity =
    0.1


pickerId : PickerElement -> String
pickerId pickerElement =
    case pickerElement of
        ColorBox ->
            "color-box"
        
        LightnessSlider ->
            "lightness-slider"


clipboardId : ClipboardTarget -> String
clipboardId clipboardTarget =
    case clipboardTarget of
        HSLOutput ->
            "hsl-css-output"

        RGBOutput ->
            "rgb-css-output"


updatePositions : Vector2 Float -> Model -> Model
updatePositions (( x, y ) as mousePosition) model =
    case model.dragging of
        Nothing ->
            model

        Just ColorBox ->
            adjustRgb
                { model
                | pickerPosition = mousePosition
                , hueValue = degreeValue x
                , satValue = percentageValue y
                }

        Just LightnessSlider ->
            adjustRgb
                { model
                | lightnessPosition = y
                , lightValue = percentageValue y
                }
                
                
adjustRgb : Model -> Model
adjustRgb model =
    let
        ( hue, saturation ) =
            model.pickerPosition
            
        lightness =
            model.lightnessPosition
            
        { red, green, blue } =
            hslToRgb
                { hue = hue
                , saturation = saturation
                , lightness = lightness
                }
                
    in
        { model
        | redValue = String.fromInt red
        , greenValue = String.fromInt green
        , blueValue = String.fromInt blue
        }
        
        
adjustHsl : Model -> Model
adjustHsl model =
    let
        mHsl =
            Maybe.map3 Rgb
                (String.toInt model.redValue)
                (String.toInt model.greenValue)
                (String.toInt model.blueValue)
            |> Maybe.map rgbToHsl
    in
    case mHsl of
        Nothing ->
            model

        Just newHsl ->
            { model
            | pickerPosition = ( newHsl.hue, newHsl.saturation )
            , lightnessPosition = newHsl.lightness
            , hueValue = degreeValue newHsl.hue
            , satValue = percentageValue newHsl.saturation
            , lightValue = percentageValue newHsl.lightness
            }
    

-- Let's not hardcode this eventually
colorBoxSize : Int
colorBoxSize =
    360


degreeValue : Float -> String
degreeValue degrees =
    String.fromInt (round (360 * degrees))


percentageValue : Float -> String
percentageValue percentage =
    String.fromInt (round (100 * percentage))
    
    
-- conversion algorithms taken from https://en.wikipedia.org/wiki/HSL_and_HSV#Color_conversion_formulae

hslToRgb : Hsl -> Rgb
hslToRgb { hue, saturation, lightness } =
    let
        alpha =
            saturation * Basics.min lightness (1 - lightness)
        
        f n =
            let
                k =
                    n + hue / 12
                    |> floatModByInt 12
            in
                lightness - alpha * Basics.max -1 (Basics.min (k - 3) (Basics.min (9 - k) 1))
    in
        { red = round (256 * f 0)
        , green = round (256 * f 8)
        , blue = round (256 * f 4)
        }
        
        
rgbToHsl : Rgb -> Hsl
rgbToHsl rgb =
    let
        r =
            toFloat rgb.red / 256
            
        g =
            toFloat rgb.green / 256
            
        b =
            toFloat rgb.blue / 256
            
        value =
            r
            |> Basics.max g
            |> Basics.max b

        xMin =
            r
            |> Basics.min g
            |> Basics.min b
            
        chroma =
            value - xMin
            
        lightness =
            value - (chroma / 2)
            
        h =
            if chroma == 0 then
                0
            else if value == r then
                (g - b) / chroma
                |> floatModByInt 6
            else if value == g then
                ((b - r) / chroma) + 2
            else
                ((r - g) / chroma) + 4
                
        hue =
            h / 6
            
        saturation =
            if lightness == 0 || lightness == 1 then
                0
            else
                chroma / (1 - abs (2 * value - chroma - 1))
    in
        { hue = hue
        , saturation = saturation
        , lightness = lightness
        }


floatModByInt : Int -> Float -> Float
floatModByInt n x =
    x - toFloat (n * floor (x / toFloat n))


view : Model -> Html Msg
view model =
    main_ [id "app"]
        [ Html.form [id "color-picker"]
            [ div [id "graphical-color-inputs"]
                [ div
                    [ id (pickerId ColorBox)
                    , onPointerDown (Grab ColorBox)
                    , onPointerMove Drag
                    , onPointerUp Release
                    , classList [( "grabbing", model.dragging == Just ColorBox )]
                    ]
                    [ canvas
                        [ id "color-box-canvas"
                        , width colorBoxSize
                        , height colorBoxSize
                        ]
                        []
                    , div
                        [ id "color-box-target"
                        , style "left" ("calc(" ++ percent (Tuple.first model.pickerPosition) ++ " - 9px)")
                        , style "bottom" ("calc(" ++ percent (Tuple.second model.pickerPosition) ++ " - 9px)")
                        ]
                        []
                    ]
                , div
                    [ id (pickerId LightnessSlider)
                    , onPointerMove Drag
                    , onPointerUp Release
                    , classList [( "grabbing", model.dragging == Just LightnessSlider )]
                    ]
                    [ div
                        [ id "lightness-slider-colors"
                        , lightnessGradient model.pickerPosition
                        , onPointerDown (Grab LightnessSlider)
                        , on "wheel" (Json.map ScrollLightnessSlider wheelScrollDirection)
                        , classList [( "grabbing", model.dragging == Just LightnessSlider )]
                        ]
                        []
                    , div
                        [ id "lightness-slider-indicator"
                        , onPointerDown (Grab LightnessSlider)
                        , style "bottom" ("calc(" ++ percent model.lightnessPosition ++ " - 18px)")
                        , classList [( "grabbing", model.dragging == Just LightnessSlider )]
                        ]
                        []
                    ]
                ]
            , div
                [ id "control-panel" ]
                [ output
                    [ id "color-preview"
                    , let
                          ( h, s ) =
                              model.pickerPosition
                          l =
                              model.lightnessPosition
                      in
                          style "background-color" (hsl h s l)
                    ]
                    []
                , div [id "color-inputs"]
                    [ label [for "hue"] [text "Hue"]
                    , input
                        [ id "hue"
                        , name "hue"
                            , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "360"
                        , value model.hueValue
                        , onInput ChangeHue
                        ]
                        []
                    , label [for "red"] [text "Red"]
                    , input
                        [ id "red"
                        , name "red"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "256"
                        , value model.redValue
                        , onInput ChangeRed
                        ]
                        []
                    , label [for "saturation"] [text "Saturation"]
                    , input
                        [ id "saturation"
                        , name "saturation"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "100"
                        , value model.satValue
                        , onInput ChangeSaturation
                        ]
                        []
                    , label [for "green"] [text "Green"]
                    , input
                        [ id "green"
                        , name "green"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "256"
                        , value model.greenValue
                        , onInput ChangeGreen
                        ]
                        []
                    , label [for "lightness"] [text "Lightness"]
                    , input
                        [ id "lightness"
                        , name "lightness"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "100"
                        , value model.lightValue
                        , onInput ChangeLightness
                        ]
                        []
                    , label [for "blue"] [text "Blue"]
                    , input
                        [ id "blue"
                        , name "blue"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "256"
                        , value model.blueValue
                        , onInput ChangeBlue
                        ]
                        []
                    ]
                ]
            , div [id "color-outputs"]
                [ cssOutput HSLOutput ("hsl(" ++ model.hueValue ++ "deg " ++ model.satValue ++ "% " ++ model.lightValue ++ "%)") model
                , cssOutput RGBOutput ("rgb(" ++ model.redValue ++ " " ++ model.greenValue ++ " " ++ model.blueValue ++ ")") model
                ]
            ]
        ]


cssOutput : ClipboardTarget -> String -> Model -> Html Msg
cssOutput thisClipboardTarget content model =
    output
        [ id (clipboardId thisClipboardTarget)
        , classList
            [ ( "css-output", True )
            , ( "success", isClipboardSuccess thisClipboardTarget model.clipboardStatus )
            , ( "failure", isClipboardFailure thisClipboardTarget model.clipboardStatus )
            ]
        , onClick (CopyToClipboard thisClipboardTarget)
        ]
        [text content]


isClipboardSuccess : ClipboardTarget -> ClipboardStatus -> Bool
isClipboardSuccess thisClipboardTarget clipboardStatus =
    case clipboardStatus of
        ClipboardSuccess successfulTarget ->
            thisClipboardTarget == successfulTarget

        _ ->
            False


isClipboardFailure : ClipboardTarget -> ClipboardStatus -> Bool
isClipboardFailure thisClipboardTarget clipboardStatus =
    case clipboardStatus of
        ClipboardFailure failedTarget _ ->
            thisClipboardTarget == failedTarget

        _ ->
            False


wheelScrollDirection : Json.Decoder ScrollDirection
wheelScrollDirection =
    Json.map
        (\ deltaY ->
            if deltaY < 0 then
                ScrollUp
            else
                ScrollDown
        )
        (Json.field "deltaY" Json.float)


onPointerDown : (PointerEvent -> msg) -> Html.Attribute msg
onPointerDown toMsg =
    on "pointerdown" (Json.map toMsg pointer)


onPointerMove : (Vector2 Float -> msg) -> Html.Attribute msg
onPointerMove toMsg =
    on "pointermove" (Json.map toMsg mouseOffsets)


onPointerUp : msg -> Html.Attribute msg
onPointerUp msg =
    on "pointerup" (Json.succeed msg)


pointer : Json.Decoder PointerEvent
pointer =
    Json.map2
        (\ pointerId ( offsetX, offsetY ) ->
            { pointerId = pointerId
            , offsetX = offsetX
            , offsetY = offsetY
            }
        )
        (Json.field "pointerId" Json.int)
        mouseOffsets
        
        
lightnessGradient : Vector2 Float -> Html.Attribute Msg
lightnessGradient ( h, s ) =
    style "background-image"
        <| "linear-gradient(in hsl,"
            ++ hsl h s 1.0 ++ ","
            ++ hsl h s 0.5 ++ ","
            ++ hsl h s 0.0 ++ ")"
                
                
deg : Float -> String
deg d =
    String.fromFloat (360 * d) ++ "deg"
    
    
percent : Float -> String
percent p =
    String.fromFloat (100 * p) ++ "%"
                
                
hsl : Float -> Float -> Float -> String
hsl h s l =
    let
        hslValues =
            [ deg h
            , percent s
            , percent l
            ]
    in
        "hsl(" ++ String.join " " hslValues ++ ")"


mouseOffsets : Json.Decoder (Vector2 Float)
mouseOffsets =
    Json.map2 adjustMouseCoordinates
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)


adjustMouseCoordinates : Float -> Float -> (Vector2 Float)
adjustMouseCoordinates x y =
    ( proportionOffset x, 1 - proportionOffset y )


proportionOffset : Float -> Float
proportionOffset x =
    clamp 0 1 (x / toFloat colorBoxSize)
        
        
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ acknowledgeClipboardSuccess (\ _ -> AcknowledgeClipboardSuccess)
        , acknowledgeClipboardFailure AcknowledgeClipboardFailure
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
