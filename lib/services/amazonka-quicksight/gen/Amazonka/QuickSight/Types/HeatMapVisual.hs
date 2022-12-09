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
-- Module      : Amazonka.QuickSight.Types.HeatMapVisual
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HeatMapVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.HeatMapConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A heat map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/heat-map.html Using heat maps>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newHeatMapVisual' smart constructor.
data HeatMapVisual = HeatMapVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a heat map.
    chartConfiguration :: Prelude.Maybe HeatMapConfiguration,
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
-- Create a value of 'HeatMapVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'heatMapVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'heatMapVisual_chartConfiguration' - The configuration of a heat map.
--
-- 'columnHierarchies', 'heatMapVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'heatMapVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'heatMapVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'heatMapVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newHeatMapVisual ::
  -- | 'visualId'
  Prelude.Text ->
  HeatMapVisual
newHeatMapVisual pVisualId_ =
  HeatMapVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
heatMapVisual_actions :: Lens.Lens' HeatMapVisual (Prelude.Maybe [VisualCustomAction])
heatMapVisual_actions = Lens.lens (\HeatMapVisual' {actions} -> actions) (\s@HeatMapVisual' {} a -> s {actions = a} :: HeatMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a heat map.
heatMapVisual_chartConfiguration :: Lens.Lens' HeatMapVisual (Prelude.Maybe HeatMapConfiguration)
heatMapVisual_chartConfiguration = Lens.lens (\HeatMapVisual' {chartConfiguration} -> chartConfiguration) (\s@HeatMapVisual' {} a -> s {chartConfiguration = a} :: HeatMapVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
heatMapVisual_columnHierarchies :: Lens.Lens' HeatMapVisual (Prelude.Maybe [ColumnHierarchy])
heatMapVisual_columnHierarchies = Lens.lens (\HeatMapVisual' {columnHierarchies} -> columnHierarchies) (\s@HeatMapVisual' {} a -> s {columnHierarchies = a} :: HeatMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
heatMapVisual_subtitle :: Lens.Lens' HeatMapVisual (Prelude.Maybe VisualSubtitleLabelOptions)
heatMapVisual_subtitle = Lens.lens (\HeatMapVisual' {subtitle} -> subtitle) (\s@HeatMapVisual' {} a -> s {subtitle = a} :: HeatMapVisual)

-- | The title that is displayed on the visual.
heatMapVisual_title :: Lens.Lens' HeatMapVisual (Prelude.Maybe VisualTitleLabelOptions)
heatMapVisual_title = Lens.lens (\HeatMapVisual' {title} -> title) (\s@HeatMapVisual' {} a -> s {title = a} :: HeatMapVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
heatMapVisual_visualId :: Lens.Lens' HeatMapVisual Prelude.Text
heatMapVisual_visualId = Lens.lens (\HeatMapVisual' {visualId} -> visualId) (\s@HeatMapVisual' {} a -> s {visualId = a} :: HeatMapVisual)

instance Data.FromJSON HeatMapVisual where
  parseJSON =
    Data.withObject
      "HeatMapVisual"
      ( \x ->
          HeatMapVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable HeatMapVisual where
  hashWithSalt _salt HeatMapVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData HeatMapVisual where
  rnf HeatMapVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON HeatMapVisual where
  toJSON HeatMapVisual' {..} =
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
