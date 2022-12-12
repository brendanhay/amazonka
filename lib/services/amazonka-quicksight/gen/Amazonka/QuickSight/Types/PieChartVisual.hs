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
-- Module      : Amazonka.QuickSight.Types.PieChartVisual
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PieChartVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.PieChartConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A pie or donut chart.
--
-- The @PieChartVisual@ structure describes a visual that is a member of
-- the pie chart family.
--
-- The following charts can be described by using this structure:
--
-- -   Pie charts
--
-- -   Donut charts
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/pie-chart.html Using pie charts>
-- in the /Amazon QuickSight User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/donut-chart.html Using donut charts>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newPieChartVisual' smart constructor.
data PieChartVisual = PieChartVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a pie chart.
    chartConfiguration :: Prelude.Maybe PieChartConfiguration,
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
-- Create a value of 'PieChartVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'pieChartVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'pieChartVisual_chartConfiguration' - The configuration of a pie chart.
--
-- 'columnHierarchies', 'pieChartVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'pieChartVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'pieChartVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'pieChartVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newPieChartVisual ::
  -- | 'visualId'
  Prelude.Text ->
  PieChartVisual
newPieChartVisual pVisualId_ =
  PieChartVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
pieChartVisual_actions :: Lens.Lens' PieChartVisual (Prelude.Maybe [VisualCustomAction])
pieChartVisual_actions = Lens.lens (\PieChartVisual' {actions} -> actions) (\s@PieChartVisual' {} a -> s {actions = a} :: PieChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a pie chart.
pieChartVisual_chartConfiguration :: Lens.Lens' PieChartVisual (Prelude.Maybe PieChartConfiguration)
pieChartVisual_chartConfiguration = Lens.lens (\PieChartVisual' {chartConfiguration} -> chartConfiguration) (\s@PieChartVisual' {} a -> s {chartConfiguration = a} :: PieChartVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
pieChartVisual_columnHierarchies :: Lens.Lens' PieChartVisual (Prelude.Maybe [ColumnHierarchy])
pieChartVisual_columnHierarchies = Lens.lens (\PieChartVisual' {columnHierarchies} -> columnHierarchies) (\s@PieChartVisual' {} a -> s {columnHierarchies = a} :: PieChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
pieChartVisual_subtitle :: Lens.Lens' PieChartVisual (Prelude.Maybe VisualSubtitleLabelOptions)
pieChartVisual_subtitle = Lens.lens (\PieChartVisual' {subtitle} -> subtitle) (\s@PieChartVisual' {} a -> s {subtitle = a} :: PieChartVisual)

-- | The title that is displayed on the visual.
pieChartVisual_title :: Lens.Lens' PieChartVisual (Prelude.Maybe VisualTitleLabelOptions)
pieChartVisual_title = Lens.lens (\PieChartVisual' {title} -> title) (\s@PieChartVisual' {} a -> s {title = a} :: PieChartVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
pieChartVisual_visualId :: Lens.Lens' PieChartVisual Prelude.Text
pieChartVisual_visualId = Lens.lens (\PieChartVisual' {visualId} -> visualId) (\s@PieChartVisual' {} a -> s {visualId = a} :: PieChartVisual)

instance Data.FromJSON PieChartVisual where
  parseJSON =
    Data.withObject
      "PieChartVisual"
      ( \x ->
          PieChartVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable PieChartVisual where
  hashWithSalt _salt PieChartVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData PieChartVisual where
  rnf PieChartVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON PieChartVisual where
  toJSON PieChartVisual' {..} =
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
