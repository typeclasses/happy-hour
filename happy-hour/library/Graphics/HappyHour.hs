{-# OPTIONS_GHC -Wall #-}

module Graphics.HappyHour (writeBarGraphSvgFile) where

-- base
import Data.List (genericLength)

-- Chart
import Graphics.Rendering.Chart.Easy hiding (bars)

-- Chart-diagrams
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile, FileOptions, fo_size)

-- | Create an SVG file containing a bar graph.

writeBarGraphSvgFile
    :: FilePath
          -- ^ Where the file will be written. The containing directory must already exist. If there already exists a file at this path, it will be overwritten. If there exists a directory at this path, the action will fail.
    -> [(String, Int)]
          -- ^ The data to visualize. Each list entry represents a bar on the plot, ordered from left to right. The @String@ is the label on the X axis, and the @Int@ is the height of the bar.
    -> IO ()

writeBarGraphSvgFile path bars =
  do
    let layout = applyBarPlot bars def :: Layout PlotIndex Int
    let r = toRenderable layout :: Renderable ()
    let fo = applyFileOptions bars def :: FileOptions
    _ <- renderableToFile fo path r
    return ()

type Bars = [(String, Int)]

type Endo a = a -> a

foldEndo :: [a -> a] -> (a -> a)
foldEndo = foldr (.) id

applyFileOptions :: Bars -> Endo FileOptions
applyFileOptions bars =
    set fo_size (maximum ([l x | (x, _) <- bars]) * l bars * 50, 600)
  where
    l = genericLength

-- | Add a single bar plot to a layout, configure its X axis labels to match the data, and style the layout to look nice with a bar plot.
applyBarPlot :: Bars -> Endo (Layout PlotIndex Int)
applyBarPlot bars =
  foldEndo
    [ applyLayoutStyle
    , applyLabels bars
    , set layout_plots [plotBars (( applyBarStyle . applyValues bars ) def)]
    ]

-- | Set the data values on a bar plot.
applyValues :: Bars -> Endo (PlotBars PlotIndex Int)
applyValues bars =
    set plot_bars_values (addIndexes [ [y] | (_, y) <- bars ])

-- | Set up the X axis of a layout in preparation for adding the bar plot.
applyLabels :: Integral x => Bars -> Endo (Layout x y)
applyLabels bars =
    set (layout_x_axis . laxis_generate) (autoIndexAxis [ x | (x, _) <- bars ])

-- | Set some layout styles that look nice.
applyLayoutStyle :: Endo (Layout x y)
applyLayoutStyle =
  foldEndo
    [ set layout_margin 40
    , setAxisLineWidth 5
    , over yGridStyle (set line_width 5 . set line_dashes [40, 20])
    , set (layout_y_axis . laxis_style . axis_label_gap) 25
    , set (layout_title_style . font_size) 80
    , setAxisFontSize 40
    ]

-- | Lens for the grid style on the Y axis
yGridStyle :: Lens' (Layout x y) LineStyle
yGridStyle = layout_y_axis . laxis_style . axis_grid_style

-- | Lens for the width of an axis
axisLineWidth :: Lens' (LayoutAxis x) Double
axisLineWidth = laxis_style . axis_line_style . line_width

-- | Lens for the label font size of an axis
axisLabelSize :: Lens' (LayoutAxis x) Double
axisLabelSize = laxis_style . axis_label_style . font_size

-- | Set the line width on both axes.
setAxisLineWidth :: Double -> Endo (Layout x y)
setAxisLineWidth v =
  foldEndo [ set l v | l <- [ layout_x_axis . axisLineWidth
                            , layout_y_axis . axisLineWidth ] ]

-- | Set the label size on both axes.
setAxisFontSize :: Double -> Endo (Layout x y)
setAxisFontSize v =
  foldEndo [ set l v | l <- [ layout_x_axis . axisLabelSize
                            , layout_y_axis . axisLabelSize ] ]

-- | Set some bar styles that look nice.
applyBarStyle :: Endo (PlotBars x y)
applyBarStyle =
  foldEndo
    [ set plot_bars_spacing (BarsFixGap 80 20)
    , set plot_bars_item_styles [(FillStyleSolid (opaque steelblue), Nothing)]
    ]
