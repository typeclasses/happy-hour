{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main (main) where

-- happy-hour
import Graphics.HappyHour (writeBarGraphSvgFile)

-- async
import Control.Concurrent.Async (mapConcurrently_)

-- base
import Data.Foldable (fold)

-- bytestring
import qualified Data.ByteString.Lazy

-- text
import qualified Data.Text
import qualified Data.Text.Lazy.Encoding

-- blaze-html
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- blaze-markup
import qualified Text.Blaze.Renderer.Utf8

-- clay
import qualified Clay as C
import qualified Clay.Render
import Clay (Css, (?), (|>))

main :: IO ()
main =
  mapConcurrently_ id
    [ writeHtmlFile "docs/index.html" indexHtml
    , writeCssFile "docs/style.css" style
    , writeBarGraphSvgFile "docs/figures/one-two-three.svg"
        [("One", 1), ("Two", 2), ("Three", 3)]
    , writeBarGraphSvgFile "docs/figures/negative.svg"
        [("One", 1), ("Negative two", (-2)), ("Three", 3)]
    , writeBarGraphSvgFile "docs/figures/many.svg"
        (zip (map pure (enumFromTo 'a' 'z')) (enumFrom 1))
    , writeBarGraphSvgFile "docs/figures/many-many.svg"
        [(show i, round (sin @Double (fromInteger i) * 100)) | i <- enumFromTo 1 100]
    ]

indexHtml :: Html
indexHtml =
    (H.html ! A.lang "en")
    (fold
      [ H.head
        (fold
          [ H.meta ! A.charset "utf-8"
          , H.title "Happy Hour"
          , H.meta ! A.content "@typeclasses" ! A.name "twitter:site"
          , H.link ! A.href "style.css" ! A.rel "stylesheet"
          ])
      , H.body
        (H.main
          (fold
            [ H.h1 "It's Happy Hour"
            , H.p "Time to hit the bar (chart)."
            , H.p ((H.a ! A.href "https://github.com/typeclasses/happy-hour/") "github.com/typeclasses/happy-hour")
            , H.h2 "Easy as one, two, three"
            , H.pre (H.code (H.text (Data.Text.unlines
                [ "writeBarGraphSvgFile \"one-two-three.svg\""
                , "    [(\"One\", 1), (\"Two\", 2), (\"Three\", 3)]"
                ])))
            , H.img ! A.src "figures/one-two-three.svg" ! styleAttr (C.height (C.px 200))
            , H.h2 "Going negative"
            , H.pre (H.code (H.text (Data.Text.unlines
                [ "writeBarGraphSvgFile \"negative.svg\""
                , "    [(\"One\", 1), (\"Negative two\", (-2)), (\"Three\", 3)]"
                ])))
            , H.img ! A.src "figures/negative.svg" ! styleAttr (C.height (C.px 200))
            , H.h2 "Many bars"
            , H.pre (H.code (H.text (Data.Text.unlines
                [ "writeBarGraphSvgFile \"many.svg\""
                , "    (zip (map pure (enumFromTo 'a' 'z')) (enumFrom 1))"
                ])))
            , H.img ! A.src "figures/many.svg" ! styleAttr (C.height (C.px 250))
            , H.h2 "So, so many bars"
            , H.pre (H.code (H.text (Data.Text.unlines
                [ "writeBarGraphSvgFile \"many-many.svg\""
                , "    [ (show i, round (sin @Double (fromInteger i) * 100))"
                , "    | i <- enumFromTo 1 100 ]"
                ])))
            , H.img ! A.src "figures/many-many.svg"
            ])
        )
      ])

  where
    styleAttr :: Css -> H.Attribute
    styleAttr = A.style . H.toValue . C.renderWith config []
      where
        config =
          C.compact { Clay.Render.lbrace = mempty
                    , Clay.Render.rbrace = mempty }

style :: Css
style =
    fold
      [ C.body ? fold
          [ C.paddingTop (C.px 50)
          , C.paddingBottom (C.px 150)
          ]
      , C.main_ ? fold
          [ C.maxWidth (C.px 800)
          , C.display C.block
          , centerBlock
          ]
      , C.img ? fold
          [ C.maxWidth (C.pct 100)
          , C.boxShadow (pure faintShadow)
          ]
      , C.pre ? fold
          [ borderRadius (C.px 10)
          , C.fontSize (C.px 16)
          , C.boxShadow (pure faintShadow)
          ]
      , C.h2 ? fold
          [ C.marginTop (C.px 100)
          ]
      , codeBlockScrolling
      , codeBlockSpacing
      , colors
      ]

  where
    colors =
        fold
          [ C.body ? C.backgroundColor (C.rgb 0xf2 0xdd 0xb4)
          , C.pre ? C.backgroundColor (C.rgb 0x48 0x2d 0x48) <> C.color C.white
          , (C.body <> C.a) ? C.color C.black
          ]

    codeBlockScrolling =
        fold
          [ C.pre ? fold
              [ C.overflowX C.auto
              , C.code ? C.display C.inlineBlock
              ]
          ]

    codeBlockSpacing =
        fold
          [ C.pre ? fold
              [ paddingVertical (C.px 20)
              , C.code ? fold
                  [ paddingHorizontal (C.px 30)
                  , C.lineHeight (C.unitless 1.6)
                  ]
              ]
          ]

    centerBlock = fold [ C.marginLeft C.auto, C.marginRight C.auto ]
    borderRadius x = C.borderRadius x x x x;
    paddingVertical x = C.paddingTop x <> C.paddingBottom x;
    paddingHorizontal x = C.paddingLeft x <> C.paddingRight x;
    faintShadow = C.bsColor C.black (C.shadowWithSpread (C.px 0) (C.px 0) (C.px 20) (C.px (-10)))

writeHtmlFile :: FilePath -> Html -> IO ()
writeHtmlFile fp =
    Data.ByteString.Lazy.writeFile fp .
    Text.Blaze.Renderer.Utf8.renderMarkup

writeCssFile :: FilePath -> Css -> IO ()
writeCssFile fp =
    Data.ByteString.Lazy.writeFile fp .
    Data.Text.Lazy.Encoding.encodeUtf8 .
    C.render
