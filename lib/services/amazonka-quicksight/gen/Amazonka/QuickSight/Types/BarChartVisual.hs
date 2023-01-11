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
-- Module      : Amazonka.QuickSight.Types.BarChartVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BarChartVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BarChartConfiguration
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A bar chart.
--
-- The @BarChartVisual@ structure describes a visual that is a member of
-- the bar chart family. The following charts can be described using this
-- structure:
--
-- -   Horizontal bar chart
--
-- -   Vertical bar chart
--
-- -   Horizontal stacked bar chart
--
-- -   Vertical stacked bar chart
--
-- -   Horizontal stacked 100% bar chart
--
-- -   Vertical stacked 100% bar chart
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/bar-charts.html Using bar charts>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newBarChartVisual' smart constructor.
data BarChartVisual = BarChartVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe BarChartConfiguration,
    -- | The column hierarchy that is used during drill-downs and drill-ups.
    columnHierarchies :: Prelude.Maybe [ColumnHierarchy],
    -- | The subtitle that is displayed on the visual.
    subtitle :: Prelude.Maybe VisualSubtitleLabelOptions,
    -- | The title that is displayed on the visual.
    title :: Prelude.Maybe VisualTitleLabelOptions,
    -- | The unique identifier of a visual. This identifier must be unique within
    -- the context of a dashboard, template, or analysis. Two dashboards,
    -- analyses, or templates can have visuals with the same identifiers.
    visualId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BarChartVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'barChartVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'barChartVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'barChartVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'barChartVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'barChartVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'barChartVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newBarChartVisual ::
  -- | 'visualId'
  Prelude.Text ->
  BarChartVisual
newBarChartVisual pVisualId_ =
  BarChartVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
barChartVisual_actions :: Lens.Lens' BarChartVisual (Prelude.Maybe [VisualCustomAction])
barChartVisual_actions = Lens.lens (\BarChartVisual' {actions} -> actions) (\s@BarChartVisual' {} a -> s {actions = a} :: BarChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
barChartVisual_chartConfiguration :: Lens.Lens' BarChartVisual (Prelude.Maybe BarChartConfiguration)
barChartVisual_chartConfiguration = Lens.lens (\BarChartVisual' {chartConfiguration} -> chartConfiguration) (\s@BarChartVisual' {} a -> s {chartConfiguration = a} :: BarChartVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
barChartVisual_columnHierarchies :: Lens.Lens' BarChartVisual (Prelude.Maybe [ColumnHierarchy])
barChartVisual_columnHierarchies = Lens.lens (\BarChartVisual' {columnHierarchies} -> columnHierarchies) (\s@BarChartVisual' {} a -> s {columnHierarchies = a} :: BarChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
barChartVisual_subtitle :: Lens.Lens' BarChartVisual (Prelude.Maybe VisualSubtitleLabelOptions)
barChartVisual_subtitle = Lens.lens (\BarChartVisual' {subtitle} -> subtitle) (\s@BarChartVisual' {} a -> s {subtitle = a} :: BarChartVisual)

-- | The title that is displayed on the visual.
barChartVisual_title :: Lens.Lens' BarChartVisual (Prelude.Maybe VisualTitleLabelOptions)
barChartVisual_title = Lens.lens (\BarChartVisual' {title} -> title) (\s@BarChartVisual' {} a -> s {title = a} :: BarChartVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
barChartVisual_visualId :: Lens.Lens' BarChartVisual Prelude.Text
barChartVisual_visualId = Lens.lens (\BarChartVisual' {visualId} -> visualId) (\s@BarChartVisual' {} a -> s {visualId = a} :: BarChartVisual)

instance Data.FromJSON BarChartVisual where
  parseJSON =
    Data.withObject
      "BarChartVisual"
      ( \x ->
          BarChartVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable BarChartVisual where
  hashWithSalt _salt BarChartVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData BarChartVisual where
  rnf BarChartVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON BarChartVisual where
  toJSON BarChartVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("ChartConfiguration" Data..=)
              Prelude.<$> chartConfiguration,
            ("ColumnHierarchies" Data..=)
              Prelude.<$> columnHierarchies,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId)
          ]
      )
