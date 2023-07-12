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
-- Module      : Amazonka.QuickSight.Types.ComboChartVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComboChartVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.ComboChartConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A combo chart.
--
-- The @ComboChartVisual@ includes stacked bar combo charts and clustered
-- bar combo charts
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/combo-charts.html Using combo charts>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newComboChartVisual' smart constructor.
data ComboChartVisual = ComboChartVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe ComboChartConfiguration,
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
-- Create a value of 'ComboChartVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'comboChartVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'comboChartVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'comboChartVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'comboChartVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'comboChartVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'comboChartVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newComboChartVisual ::
  -- | 'visualId'
  Prelude.Text ->
  ComboChartVisual
newComboChartVisual pVisualId_ =
  ComboChartVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
comboChartVisual_actions :: Lens.Lens' ComboChartVisual (Prelude.Maybe [VisualCustomAction])
comboChartVisual_actions = Lens.lens (\ComboChartVisual' {actions} -> actions) (\s@ComboChartVisual' {} a -> s {actions = a} :: ComboChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
comboChartVisual_chartConfiguration :: Lens.Lens' ComboChartVisual (Prelude.Maybe ComboChartConfiguration)
comboChartVisual_chartConfiguration = Lens.lens (\ComboChartVisual' {chartConfiguration} -> chartConfiguration) (\s@ComboChartVisual' {} a -> s {chartConfiguration = a} :: ComboChartVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
comboChartVisual_columnHierarchies :: Lens.Lens' ComboChartVisual (Prelude.Maybe [ColumnHierarchy])
comboChartVisual_columnHierarchies = Lens.lens (\ComboChartVisual' {columnHierarchies} -> columnHierarchies) (\s@ComboChartVisual' {} a -> s {columnHierarchies = a} :: ComboChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
comboChartVisual_subtitle :: Lens.Lens' ComboChartVisual (Prelude.Maybe VisualSubtitleLabelOptions)
comboChartVisual_subtitle = Lens.lens (\ComboChartVisual' {subtitle} -> subtitle) (\s@ComboChartVisual' {} a -> s {subtitle = a} :: ComboChartVisual)

-- | The title that is displayed on the visual.
comboChartVisual_title :: Lens.Lens' ComboChartVisual (Prelude.Maybe VisualTitleLabelOptions)
comboChartVisual_title = Lens.lens (\ComboChartVisual' {title} -> title) (\s@ComboChartVisual' {} a -> s {title = a} :: ComboChartVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
comboChartVisual_visualId :: Lens.Lens' ComboChartVisual Prelude.Text
comboChartVisual_visualId = Lens.lens (\ComboChartVisual' {visualId} -> visualId) (\s@ComboChartVisual' {} a -> s {visualId = a} :: ComboChartVisual)

instance Data.FromJSON ComboChartVisual where
  parseJSON =
    Data.withObject
      "ComboChartVisual"
      ( \x ->
          ComboChartVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x
                            Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable ComboChartVisual where
  hashWithSalt _salt ComboChartVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData ComboChartVisual where
  rnf ComboChartVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON ComboChartVisual where
  toJSON ComboChartVisual' {..} =
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
