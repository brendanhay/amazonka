{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types.Visual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Visual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BarChartVisual
import Amazonka.QuickSight.Types.BoxPlotVisual
import Amazonka.QuickSight.Types.ComboChartVisual
import Amazonka.QuickSight.Types.CustomContentVisual
import Amazonka.QuickSight.Types.EmptyVisual
import Amazonka.QuickSight.Types.FilledMapVisual
import Amazonka.QuickSight.Types.FunnelChartVisual
import Amazonka.QuickSight.Types.GaugeChartVisual
import Amazonka.QuickSight.Types.GeospatialMapVisual
import Amazonka.QuickSight.Types.HeatMapVisual
import Amazonka.QuickSight.Types.HistogramVisual
import Amazonka.QuickSight.Types.InsightVisual
import Amazonka.QuickSight.Types.KPIVisual
import Amazonka.QuickSight.Types.LineChartVisual
import Amazonka.QuickSight.Types.PieChartVisual
import Amazonka.QuickSight.Types.PivotTableVisual
import Amazonka.QuickSight.Types.SankeyDiagramVisual
import Amazonka.QuickSight.Types.ScatterPlotVisual
import Amazonka.QuickSight.Types.TableVisual
import Amazonka.QuickSight.Types.TreeMapVisual
import Amazonka.QuickSight.Types.WaterfallVisual
import Amazonka.QuickSight.Types.WordCloudVisual

-- | A visual displayed on a sheet in an analysis, dashboard, or template.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newVisual' smart constructor.
data Visual = Visual'
  { -- | A bar chart.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/bar-charts.html Using bar charts>
    -- in the /Amazon QuickSight User Guide/.
    barChartVisual :: Prelude.Maybe BarChartVisual,
    -- | A box plot.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/box-plots.html Using box plots>
    -- in the /Amazon QuickSight User Guide/.
    boxPlotVisual :: Prelude.Maybe BoxPlotVisual,
    -- | A combo chart.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/combo-charts.html Using combo charts>
    -- in the /Amazon QuickSight User Guide/.
    comboChartVisual :: Prelude.Maybe ComboChartVisual,
    -- | A visual that contains custom content.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/custom-visual-content.html Using custom visual content>
    -- in the /Amazon QuickSight User Guide/.
    customContentVisual :: Prelude.Maybe CustomContentVisual,
    -- | An empty visual.
    emptyVisual :: Prelude.Maybe EmptyVisual,
    -- | A filled map.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/filled-maps.html Creating filled maps>
    -- in the /Amazon QuickSight User Guide/.
    filledMapVisual :: Prelude.Maybe FilledMapVisual,
    -- | A funnel chart.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/funnel-visual-content.html Using funnel charts>
    -- in the /Amazon QuickSight User Guide/.
    funnelChartVisual :: Prelude.Maybe FunnelChartVisual,
    -- | A gauge chart.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/gauge-chart.html Using gauge charts>
    -- in the /Amazon QuickSight User Guide/.
    gaugeChartVisual :: Prelude.Maybe GaugeChartVisual,
    -- | A geospatial map or a points on map visual.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/point-maps.html Creating point maps>
    -- in the /Amazon QuickSight User Guide/.
    geospatialMapVisual :: Prelude.Maybe GeospatialMapVisual,
    -- | A heat map.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/heat-map.html Using heat maps>
    -- in the /Amazon QuickSight User Guide/.
    heatMapVisual :: Prelude.Maybe HeatMapVisual,
    -- | A histogram.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/histogram-charts.html Using histograms>
    -- in the /Amazon QuickSight User Guide/.
    histogramVisual :: Prelude.Maybe HistogramVisual,
    -- | An insight visual.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/computational-insights.html Working with insights>
    -- in the /Amazon QuickSight User Guide/.
    insightVisual :: Prelude.Maybe InsightVisual,
    -- | A key performance indicator (KPI).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/kpi.html Using KPIs>
    -- in the /Amazon QuickSight User Guide/.
    kPIVisual :: Prelude.Maybe KPIVisual,
    -- | A line chart.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/line-charts.html Using line charts>
    -- in the /Amazon QuickSight User Guide/.
    lineChartVisual :: Prelude.Maybe LineChartVisual,
    -- | A pie or donut chart.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/pie-chart.html Using pie charts>
    -- in the /Amazon QuickSight User Guide/.
    pieChartVisual :: Prelude.Maybe PieChartVisual,
    -- | A pivot table.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/pivot-table.html Using pivot tables>
    -- in the /Amazon QuickSight User Guide/.
    pivotTableVisual :: Prelude.Maybe PivotTableVisual,
    -- | A sankey diagram.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/sankey-diagram.html Using Sankey diagrams>
    -- in the /Amazon QuickSight User Guide/.
    sankeyDiagramVisual :: Prelude.Maybe SankeyDiagramVisual,
    -- | A scatter plot.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/scatter-plot.html Using scatter plots>
    -- in the /Amazon QuickSight User Guide/.
    scatterPlotVisual :: Prelude.Maybe ScatterPlotVisual,
    -- | A table visual.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/tabular.html Using tables as visuals>
    -- in the /Amazon QuickSight User Guide/.
    tableVisual :: Prelude.Maybe TableVisual,
    -- | A tree map.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/tree-map.html Using tree maps>
    -- in the /Amazon QuickSight User Guide/.
    treeMapVisual :: Prelude.Maybe TreeMapVisual,
    -- | A waterfall chart.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/waterfall-chart.html Using waterfall charts>
    -- in the /Amazon QuickSight User Guide/.
    waterfallVisual :: Prelude.Maybe WaterfallVisual,
    -- | A word cloud.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/word-cloud.html Using word clouds>
    -- in the /Amazon QuickSight User Guide/.
    wordCloudVisual :: Prelude.Maybe WordCloudVisual
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Visual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'barChartVisual', 'visual_barChartVisual' - A bar chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/bar-charts.html Using bar charts>
-- in the /Amazon QuickSight User Guide/.
--
-- 'boxPlotVisual', 'visual_boxPlotVisual' - A box plot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/box-plots.html Using box plots>
-- in the /Amazon QuickSight User Guide/.
--
-- 'comboChartVisual', 'visual_comboChartVisual' - A combo chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/combo-charts.html Using combo charts>
-- in the /Amazon QuickSight User Guide/.
--
-- 'customContentVisual', 'visual_customContentVisual' - A visual that contains custom content.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/custom-visual-content.html Using custom visual content>
-- in the /Amazon QuickSight User Guide/.
--
-- 'emptyVisual', 'visual_emptyVisual' - An empty visual.
--
-- 'filledMapVisual', 'visual_filledMapVisual' - A filled map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/filled-maps.html Creating filled maps>
-- in the /Amazon QuickSight User Guide/.
--
-- 'funnelChartVisual', 'visual_funnelChartVisual' - A funnel chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/funnel-visual-content.html Using funnel charts>
-- in the /Amazon QuickSight User Guide/.
--
-- 'gaugeChartVisual', 'visual_gaugeChartVisual' - A gauge chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/gauge-chart.html Using gauge charts>
-- in the /Amazon QuickSight User Guide/.
--
-- 'geospatialMapVisual', 'visual_geospatialMapVisual' - A geospatial map or a points on map visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/point-maps.html Creating point maps>
-- in the /Amazon QuickSight User Guide/.
--
-- 'heatMapVisual', 'visual_heatMapVisual' - A heat map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/heat-map.html Using heat maps>
-- in the /Amazon QuickSight User Guide/.
--
-- 'histogramVisual', 'visual_histogramVisual' - A histogram.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/histogram-charts.html Using histograms>
-- in the /Amazon QuickSight User Guide/.
--
-- 'insightVisual', 'visual_insightVisual' - An insight visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/computational-insights.html Working with insights>
-- in the /Amazon QuickSight User Guide/.
--
-- 'kPIVisual', 'visual_kPIVisual' - A key performance indicator (KPI).
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/kpi.html Using KPIs>
-- in the /Amazon QuickSight User Guide/.
--
-- 'lineChartVisual', 'visual_lineChartVisual' - A line chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/line-charts.html Using line charts>
-- in the /Amazon QuickSight User Guide/.
--
-- 'pieChartVisual', 'visual_pieChartVisual' - A pie or donut chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/pie-chart.html Using pie charts>
-- in the /Amazon QuickSight User Guide/.
--
-- 'pivotTableVisual', 'visual_pivotTableVisual' - A pivot table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/pivot-table.html Using pivot tables>
-- in the /Amazon QuickSight User Guide/.
--
-- 'sankeyDiagramVisual', 'visual_sankeyDiagramVisual' - A sankey diagram.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/sankey-diagram.html Using Sankey diagrams>
-- in the /Amazon QuickSight User Guide/.
--
-- 'scatterPlotVisual', 'visual_scatterPlotVisual' - A scatter plot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/scatter-plot.html Using scatter plots>
-- in the /Amazon QuickSight User Guide/.
--
-- 'tableVisual', 'visual_tableVisual' - A table visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/tabular.html Using tables as visuals>
-- in the /Amazon QuickSight User Guide/.
--
-- 'treeMapVisual', 'visual_treeMapVisual' - A tree map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/tree-map.html Using tree maps>
-- in the /Amazon QuickSight User Guide/.
--
-- 'waterfallVisual', 'visual_waterfallVisual' - A waterfall chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/waterfall-chart.html Using waterfall charts>
-- in the /Amazon QuickSight User Guide/.
--
-- 'wordCloudVisual', 'visual_wordCloudVisual' - A word cloud.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/word-cloud.html Using word clouds>
-- in the /Amazon QuickSight User Guide/.
newVisual ::
  Visual
newVisual =
  Visual'
    { barChartVisual = Prelude.Nothing,
      boxPlotVisual = Prelude.Nothing,
      comboChartVisual = Prelude.Nothing,
      customContentVisual = Prelude.Nothing,
      emptyVisual = Prelude.Nothing,
      filledMapVisual = Prelude.Nothing,
      funnelChartVisual = Prelude.Nothing,
      gaugeChartVisual = Prelude.Nothing,
      geospatialMapVisual = Prelude.Nothing,
      heatMapVisual = Prelude.Nothing,
      histogramVisual = Prelude.Nothing,
      insightVisual = Prelude.Nothing,
      kPIVisual = Prelude.Nothing,
      lineChartVisual = Prelude.Nothing,
      pieChartVisual = Prelude.Nothing,
      pivotTableVisual = Prelude.Nothing,
      sankeyDiagramVisual = Prelude.Nothing,
      scatterPlotVisual = Prelude.Nothing,
      tableVisual = Prelude.Nothing,
      treeMapVisual = Prelude.Nothing,
      waterfallVisual = Prelude.Nothing,
      wordCloudVisual = Prelude.Nothing
    }

-- | A bar chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/bar-charts.html Using bar charts>
-- in the /Amazon QuickSight User Guide/.
visual_barChartVisual :: Lens.Lens' Visual (Prelude.Maybe BarChartVisual)
visual_barChartVisual = Lens.lens (\Visual' {barChartVisual} -> barChartVisual) (\s@Visual' {} a -> s {barChartVisual = a} :: Visual)

-- | A box plot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/box-plots.html Using box plots>
-- in the /Amazon QuickSight User Guide/.
visual_boxPlotVisual :: Lens.Lens' Visual (Prelude.Maybe BoxPlotVisual)
visual_boxPlotVisual = Lens.lens (\Visual' {boxPlotVisual} -> boxPlotVisual) (\s@Visual' {} a -> s {boxPlotVisual = a} :: Visual)

-- | A combo chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/combo-charts.html Using combo charts>
-- in the /Amazon QuickSight User Guide/.
visual_comboChartVisual :: Lens.Lens' Visual (Prelude.Maybe ComboChartVisual)
visual_comboChartVisual = Lens.lens (\Visual' {comboChartVisual} -> comboChartVisual) (\s@Visual' {} a -> s {comboChartVisual = a} :: Visual)

-- | A visual that contains custom content.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/custom-visual-content.html Using custom visual content>
-- in the /Amazon QuickSight User Guide/.
visual_customContentVisual :: Lens.Lens' Visual (Prelude.Maybe CustomContentVisual)
visual_customContentVisual = Lens.lens (\Visual' {customContentVisual} -> customContentVisual) (\s@Visual' {} a -> s {customContentVisual = a} :: Visual)

-- | An empty visual.
visual_emptyVisual :: Lens.Lens' Visual (Prelude.Maybe EmptyVisual)
visual_emptyVisual = Lens.lens (\Visual' {emptyVisual} -> emptyVisual) (\s@Visual' {} a -> s {emptyVisual = a} :: Visual)

-- | A filled map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/filled-maps.html Creating filled maps>
-- in the /Amazon QuickSight User Guide/.
visual_filledMapVisual :: Lens.Lens' Visual (Prelude.Maybe FilledMapVisual)
visual_filledMapVisual = Lens.lens (\Visual' {filledMapVisual} -> filledMapVisual) (\s@Visual' {} a -> s {filledMapVisual = a} :: Visual)

-- | A funnel chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/funnel-visual-content.html Using funnel charts>
-- in the /Amazon QuickSight User Guide/.
visual_funnelChartVisual :: Lens.Lens' Visual (Prelude.Maybe FunnelChartVisual)
visual_funnelChartVisual = Lens.lens (\Visual' {funnelChartVisual} -> funnelChartVisual) (\s@Visual' {} a -> s {funnelChartVisual = a} :: Visual)

-- | A gauge chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/gauge-chart.html Using gauge charts>
-- in the /Amazon QuickSight User Guide/.
visual_gaugeChartVisual :: Lens.Lens' Visual (Prelude.Maybe GaugeChartVisual)
visual_gaugeChartVisual = Lens.lens (\Visual' {gaugeChartVisual} -> gaugeChartVisual) (\s@Visual' {} a -> s {gaugeChartVisual = a} :: Visual)

-- | A geospatial map or a points on map visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/point-maps.html Creating point maps>
-- in the /Amazon QuickSight User Guide/.
visual_geospatialMapVisual :: Lens.Lens' Visual (Prelude.Maybe GeospatialMapVisual)
visual_geospatialMapVisual = Lens.lens (\Visual' {geospatialMapVisual} -> geospatialMapVisual) (\s@Visual' {} a -> s {geospatialMapVisual = a} :: Visual)

-- | A heat map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/heat-map.html Using heat maps>
-- in the /Amazon QuickSight User Guide/.
visual_heatMapVisual :: Lens.Lens' Visual (Prelude.Maybe HeatMapVisual)
visual_heatMapVisual = Lens.lens (\Visual' {heatMapVisual} -> heatMapVisual) (\s@Visual' {} a -> s {heatMapVisual = a} :: Visual)

-- | A histogram.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/histogram-charts.html Using histograms>
-- in the /Amazon QuickSight User Guide/.
visual_histogramVisual :: Lens.Lens' Visual (Prelude.Maybe HistogramVisual)
visual_histogramVisual = Lens.lens (\Visual' {histogramVisual} -> histogramVisual) (\s@Visual' {} a -> s {histogramVisual = a} :: Visual)

-- | An insight visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/computational-insights.html Working with insights>
-- in the /Amazon QuickSight User Guide/.
visual_insightVisual :: Lens.Lens' Visual (Prelude.Maybe InsightVisual)
visual_insightVisual = Lens.lens (\Visual' {insightVisual} -> insightVisual) (\s@Visual' {} a -> s {insightVisual = a} :: Visual)

-- | A key performance indicator (KPI).
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/kpi.html Using KPIs>
-- in the /Amazon QuickSight User Guide/.
visual_kPIVisual :: Lens.Lens' Visual (Prelude.Maybe KPIVisual)
visual_kPIVisual = Lens.lens (\Visual' {kPIVisual} -> kPIVisual) (\s@Visual' {} a -> s {kPIVisual = a} :: Visual)

-- | A line chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/line-charts.html Using line charts>
-- in the /Amazon QuickSight User Guide/.
visual_lineChartVisual :: Lens.Lens' Visual (Prelude.Maybe LineChartVisual)
visual_lineChartVisual = Lens.lens (\Visual' {lineChartVisual} -> lineChartVisual) (\s@Visual' {} a -> s {lineChartVisual = a} :: Visual)

-- | A pie or donut chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/pie-chart.html Using pie charts>
-- in the /Amazon QuickSight User Guide/.
visual_pieChartVisual :: Lens.Lens' Visual (Prelude.Maybe PieChartVisual)
visual_pieChartVisual = Lens.lens (\Visual' {pieChartVisual} -> pieChartVisual) (\s@Visual' {} a -> s {pieChartVisual = a} :: Visual)

-- | A pivot table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/pivot-table.html Using pivot tables>
-- in the /Amazon QuickSight User Guide/.
visual_pivotTableVisual :: Lens.Lens' Visual (Prelude.Maybe PivotTableVisual)
visual_pivotTableVisual = Lens.lens (\Visual' {pivotTableVisual} -> pivotTableVisual) (\s@Visual' {} a -> s {pivotTableVisual = a} :: Visual)

-- | A sankey diagram.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/sankey-diagram.html Using Sankey diagrams>
-- in the /Amazon QuickSight User Guide/.
visual_sankeyDiagramVisual :: Lens.Lens' Visual (Prelude.Maybe SankeyDiagramVisual)
visual_sankeyDiagramVisual = Lens.lens (\Visual' {sankeyDiagramVisual} -> sankeyDiagramVisual) (\s@Visual' {} a -> s {sankeyDiagramVisual = a} :: Visual)

-- | A scatter plot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/scatter-plot.html Using scatter plots>
-- in the /Amazon QuickSight User Guide/.
visual_scatterPlotVisual :: Lens.Lens' Visual (Prelude.Maybe ScatterPlotVisual)
visual_scatterPlotVisual = Lens.lens (\Visual' {scatterPlotVisual} -> scatterPlotVisual) (\s@Visual' {} a -> s {scatterPlotVisual = a} :: Visual)

-- | A table visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/tabular.html Using tables as visuals>
-- in the /Amazon QuickSight User Guide/.
visual_tableVisual :: Lens.Lens' Visual (Prelude.Maybe TableVisual)
visual_tableVisual = Lens.lens (\Visual' {tableVisual} -> tableVisual) (\s@Visual' {} a -> s {tableVisual = a} :: Visual)

-- | A tree map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/tree-map.html Using tree maps>
-- in the /Amazon QuickSight User Guide/.
visual_treeMapVisual :: Lens.Lens' Visual (Prelude.Maybe TreeMapVisual)
visual_treeMapVisual = Lens.lens (\Visual' {treeMapVisual} -> treeMapVisual) (\s@Visual' {} a -> s {treeMapVisual = a} :: Visual)

-- | A waterfall chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/waterfall-chart.html Using waterfall charts>
-- in the /Amazon QuickSight User Guide/.
visual_waterfallVisual :: Lens.Lens' Visual (Prelude.Maybe WaterfallVisual)
visual_waterfallVisual = Lens.lens (\Visual' {waterfallVisual} -> waterfallVisual) (\s@Visual' {} a -> s {waterfallVisual = a} :: Visual)

-- | A word cloud.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/word-cloud.html Using word clouds>
-- in the /Amazon QuickSight User Guide/.
visual_wordCloudVisual :: Lens.Lens' Visual (Prelude.Maybe WordCloudVisual)
visual_wordCloudVisual = Lens.lens (\Visual' {wordCloudVisual} -> wordCloudVisual) (\s@Visual' {} a -> s {wordCloudVisual = a} :: Visual)

instance Data.FromJSON Visual where
  parseJSON =
    Data.withObject
      "Visual"
      ( \x ->
          Visual'
            Prelude.<$> (x Data..:? "BarChartVisual")
            Prelude.<*> (x Data..:? "BoxPlotVisual")
            Prelude.<*> (x Data..:? "ComboChartVisual")
            Prelude.<*> (x Data..:? "CustomContentVisual")
            Prelude.<*> (x Data..:? "EmptyVisual")
            Prelude.<*> (x Data..:? "FilledMapVisual")
            Prelude.<*> (x Data..:? "FunnelChartVisual")
            Prelude.<*> (x Data..:? "GaugeChartVisual")
            Prelude.<*> (x Data..:? "GeospatialMapVisual")
            Prelude.<*> (x Data..:? "HeatMapVisual")
            Prelude.<*> (x Data..:? "HistogramVisual")
            Prelude.<*> (x Data..:? "InsightVisual")
            Prelude.<*> (x Data..:? "KPIVisual")
            Prelude.<*> (x Data..:? "LineChartVisual")
            Prelude.<*> (x Data..:? "PieChartVisual")
            Prelude.<*> (x Data..:? "PivotTableVisual")
            Prelude.<*> (x Data..:? "SankeyDiagramVisual")
            Prelude.<*> (x Data..:? "ScatterPlotVisual")
            Prelude.<*> (x Data..:? "TableVisual")
            Prelude.<*> (x Data..:? "TreeMapVisual")
            Prelude.<*> (x Data..:? "WaterfallVisual")
            Prelude.<*> (x Data..:? "WordCloudVisual")
      )

instance Prelude.Hashable Visual where
  hashWithSalt _salt Visual' {..} =
    _salt
      `Prelude.hashWithSalt` barChartVisual
      `Prelude.hashWithSalt` boxPlotVisual
      `Prelude.hashWithSalt` comboChartVisual
      `Prelude.hashWithSalt` customContentVisual
      `Prelude.hashWithSalt` emptyVisual
      `Prelude.hashWithSalt` filledMapVisual
      `Prelude.hashWithSalt` funnelChartVisual
      `Prelude.hashWithSalt` gaugeChartVisual
      `Prelude.hashWithSalt` geospatialMapVisual
      `Prelude.hashWithSalt` heatMapVisual
      `Prelude.hashWithSalt` histogramVisual
      `Prelude.hashWithSalt` insightVisual
      `Prelude.hashWithSalt` kPIVisual
      `Prelude.hashWithSalt` lineChartVisual
      `Prelude.hashWithSalt` pieChartVisual
      `Prelude.hashWithSalt` pivotTableVisual
      `Prelude.hashWithSalt` sankeyDiagramVisual
      `Prelude.hashWithSalt` scatterPlotVisual
      `Prelude.hashWithSalt` tableVisual
      `Prelude.hashWithSalt` treeMapVisual
      `Prelude.hashWithSalt` waterfallVisual
      `Prelude.hashWithSalt` wordCloudVisual

instance Prelude.NFData Visual where
  rnf Visual' {..} =
    Prelude.rnf barChartVisual `Prelude.seq`
      Prelude.rnf boxPlotVisual `Prelude.seq`
        Prelude.rnf comboChartVisual `Prelude.seq`
          Prelude.rnf customContentVisual `Prelude.seq`
            Prelude.rnf emptyVisual `Prelude.seq`
              Prelude.rnf filledMapVisual `Prelude.seq`
                Prelude.rnf funnelChartVisual `Prelude.seq`
                  Prelude.rnf gaugeChartVisual `Prelude.seq`
                    Prelude.rnf geospatialMapVisual `Prelude.seq`
                      Prelude.rnf heatMapVisual `Prelude.seq`
                        Prelude.rnf histogramVisual `Prelude.seq`
                          Prelude.rnf insightVisual `Prelude.seq`
                            Prelude.rnf kPIVisual `Prelude.seq`
                              Prelude.rnf lineChartVisual `Prelude.seq`
                                Prelude.rnf pieChartVisual `Prelude.seq`
                                  Prelude.rnf pivotTableVisual `Prelude.seq`
                                    Prelude.rnf sankeyDiagramVisual `Prelude.seq`
                                      Prelude.rnf scatterPlotVisual `Prelude.seq`
                                        Prelude.rnf tableVisual `Prelude.seq`
                                          Prelude.rnf treeMapVisual `Prelude.seq`
                                            Prelude.rnf waterfallVisual `Prelude.seq`
                                              Prelude.rnf
                                                wordCloudVisual

instance Data.ToJSON Visual where
  toJSON Visual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BarChartVisual" Data..=)
              Prelude.<$> barChartVisual,
            ("BoxPlotVisual" Data..=) Prelude.<$> boxPlotVisual,
            ("ComboChartVisual" Data..=)
              Prelude.<$> comboChartVisual,
            ("CustomContentVisual" Data..=)
              Prelude.<$> customContentVisual,
            ("EmptyVisual" Data..=) Prelude.<$> emptyVisual,
            ("FilledMapVisual" Data..=)
              Prelude.<$> filledMapVisual,
            ("FunnelChartVisual" Data..=)
              Prelude.<$> funnelChartVisual,
            ("GaugeChartVisual" Data..=)
              Prelude.<$> gaugeChartVisual,
            ("GeospatialMapVisual" Data..=)
              Prelude.<$> geospatialMapVisual,
            ("HeatMapVisual" Data..=) Prelude.<$> heatMapVisual,
            ("HistogramVisual" Data..=)
              Prelude.<$> histogramVisual,
            ("InsightVisual" Data..=) Prelude.<$> insightVisual,
            ("KPIVisual" Data..=) Prelude.<$> kPIVisual,
            ("LineChartVisual" Data..=)
              Prelude.<$> lineChartVisual,
            ("PieChartVisual" Data..=)
              Prelude.<$> pieChartVisual,
            ("PivotTableVisual" Data..=)
              Prelude.<$> pivotTableVisual,
            ("SankeyDiagramVisual" Data..=)
              Prelude.<$> sankeyDiagramVisual,
            ("ScatterPlotVisual" Data..=)
              Prelude.<$> scatterPlotVisual,
            ("TableVisual" Data..=) Prelude.<$> tableVisual,
            ("TreeMapVisual" Data..=) Prelude.<$> treeMapVisual,
            ("WaterfallVisual" Data..=)
              Prelude.<$> waterfallVisual,
            ("WordCloudVisual" Data..=)
              Prelude.<$> wordCloudVisual
          ]
      )
