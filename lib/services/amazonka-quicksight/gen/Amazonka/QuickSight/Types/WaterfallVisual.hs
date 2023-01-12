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
-- Module      : Amazonka.QuickSight.Types.WaterfallVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WaterfallVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions
import Amazonka.QuickSight.Types.WaterfallChartConfiguration

-- | A waterfall chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/waterfall-chart.html Using waterfall charts>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newWaterfallVisual' smart constructor.
data WaterfallVisual = WaterfallVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration for a waterfall visual.
    chartConfiguration :: Prelude.Maybe WaterfallChartConfiguration,
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
-- Create a value of 'WaterfallVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'waterfallVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'waterfallVisual_chartConfiguration' - The configuration for a waterfall visual.
--
-- 'columnHierarchies', 'waterfallVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'waterfallVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'waterfallVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'waterfallVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newWaterfallVisual ::
  -- | 'visualId'
  Prelude.Text ->
  WaterfallVisual
newWaterfallVisual pVisualId_ =
  WaterfallVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
waterfallVisual_actions :: Lens.Lens' WaterfallVisual (Prelude.Maybe [VisualCustomAction])
waterfallVisual_actions = Lens.lens (\WaterfallVisual' {actions} -> actions) (\s@WaterfallVisual' {} a -> s {actions = a} :: WaterfallVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for a waterfall visual.
waterfallVisual_chartConfiguration :: Lens.Lens' WaterfallVisual (Prelude.Maybe WaterfallChartConfiguration)
waterfallVisual_chartConfiguration = Lens.lens (\WaterfallVisual' {chartConfiguration} -> chartConfiguration) (\s@WaterfallVisual' {} a -> s {chartConfiguration = a} :: WaterfallVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
waterfallVisual_columnHierarchies :: Lens.Lens' WaterfallVisual (Prelude.Maybe [ColumnHierarchy])
waterfallVisual_columnHierarchies = Lens.lens (\WaterfallVisual' {columnHierarchies} -> columnHierarchies) (\s@WaterfallVisual' {} a -> s {columnHierarchies = a} :: WaterfallVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
waterfallVisual_subtitle :: Lens.Lens' WaterfallVisual (Prelude.Maybe VisualSubtitleLabelOptions)
waterfallVisual_subtitle = Lens.lens (\WaterfallVisual' {subtitle} -> subtitle) (\s@WaterfallVisual' {} a -> s {subtitle = a} :: WaterfallVisual)

-- | The title that is displayed on the visual.
waterfallVisual_title :: Lens.Lens' WaterfallVisual (Prelude.Maybe VisualTitleLabelOptions)
waterfallVisual_title = Lens.lens (\WaterfallVisual' {title} -> title) (\s@WaterfallVisual' {} a -> s {title = a} :: WaterfallVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
waterfallVisual_visualId :: Lens.Lens' WaterfallVisual Prelude.Text
waterfallVisual_visualId = Lens.lens (\WaterfallVisual' {visualId} -> visualId) (\s@WaterfallVisual' {} a -> s {visualId = a} :: WaterfallVisual)

instance Data.FromJSON WaterfallVisual where
  parseJSON =
    Data.withObject
      "WaterfallVisual"
      ( \x ->
          WaterfallVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable WaterfallVisual where
  hashWithSalt _salt WaterfallVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData WaterfallVisual where
  rnf WaterfallVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON WaterfallVisual where
  toJSON WaterfallVisual' {..} =
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
