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
-- Module      : Amazonka.QuickSight.Types.LineChartVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.LineChartConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A line chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/line-charts.html Using line charts>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newLineChartVisual' smart constructor.
data LineChartVisual = LineChartVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a line chart.
    chartConfiguration :: Prelude.Maybe LineChartConfiguration,
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
-- Create a value of 'LineChartVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'lineChartVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'lineChartVisual_chartConfiguration' - The configuration of a line chart.
--
-- 'columnHierarchies', 'lineChartVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'lineChartVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'lineChartVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'lineChartVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newLineChartVisual ::
  -- | 'visualId'
  Prelude.Text ->
  LineChartVisual
newLineChartVisual pVisualId_ =
  LineChartVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
lineChartVisual_actions :: Lens.Lens' LineChartVisual (Prelude.Maybe [VisualCustomAction])
lineChartVisual_actions = Lens.lens (\LineChartVisual' {actions} -> actions) (\s@LineChartVisual' {} a -> s {actions = a} :: LineChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a line chart.
lineChartVisual_chartConfiguration :: Lens.Lens' LineChartVisual (Prelude.Maybe LineChartConfiguration)
lineChartVisual_chartConfiguration = Lens.lens (\LineChartVisual' {chartConfiguration} -> chartConfiguration) (\s@LineChartVisual' {} a -> s {chartConfiguration = a} :: LineChartVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
lineChartVisual_columnHierarchies :: Lens.Lens' LineChartVisual (Prelude.Maybe [ColumnHierarchy])
lineChartVisual_columnHierarchies = Lens.lens (\LineChartVisual' {columnHierarchies} -> columnHierarchies) (\s@LineChartVisual' {} a -> s {columnHierarchies = a} :: LineChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
lineChartVisual_subtitle :: Lens.Lens' LineChartVisual (Prelude.Maybe VisualSubtitleLabelOptions)
lineChartVisual_subtitle = Lens.lens (\LineChartVisual' {subtitle} -> subtitle) (\s@LineChartVisual' {} a -> s {subtitle = a} :: LineChartVisual)

-- | The title that is displayed on the visual.
lineChartVisual_title :: Lens.Lens' LineChartVisual (Prelude.Maybe VisualTitleLabelOptions)
lineChartVisual_title = Lens.lens (\LineChartVisual' {title} -> title) (\s@LineChartVisual' {} a -> s {title = a} :: LineChartVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
lineChartVisual_visualId :: Lens.Lens' LineChartVisual Prelude.Text
lineChartVisual_visualId = Lens.lens (\LineChartVisual' {visualId} -> visualId) (\s@LineChartVisual' {} a -> s {visualId = a} :: LineChartVisual)

instance Data.FromJSON LineChartVisual where
  parseJSON =
    Data.withObject
      "LineChartVisual"
      ( \x ->
          LineChartVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable LineChartVisual where
  hashWithSalt _salt LineChartVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData LineChartVisual where
  rnf LineChartVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON LineChartVisual where
  toJSON LineChartVisual' {..} =
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
