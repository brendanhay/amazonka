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
-- Module      : Amazonka.QuickSight.Types.RadarChartVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RadarChartVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.RadarChartConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A radar chart visual.
--
-- /See:/ 'newRadarChartVisual' smart constructor.
data RadarChartVisual = RadarChartVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe RadarChartConfiguration,
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
-- Create a value of 'RadarChartVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'radarChartVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'radarChartVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'radarChartVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'radarChartVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'radarChartVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'radarChartVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newRadarChartVisual ::
  -- | 'visualId'
  Prelude.Text ->
  RadarChartVisual
newRadarChartVisual pVisualId_ =
  RadarChartVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
radarChartVisual_actions :: Lens.Lens' RadarChartVisual (Prelude.Maybe [VisualCustomAction])
radarChartVisual_actions = Lens.lens (\RadarChartVisual' {actions} -> actions) (\s@RadarChartVisual' {} a -> s {actions = a} :: RadarChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
radarChartVisual_chartConfiguration :: Lens.Lens' RadarChartVisual (Prelude.Maybe RadarChartConfiguration)
radarChartVisual_chartConfiguration = Lens.lens (\RadarChartVisual' {chartConfiguration} -> chartConfiguration) (\s@RadarChartVisual' {} a -> s {chartConfiguration = a} :: RadarChartVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
radarChartVisual_columnHierarchies :: Lens.Lens' RadarChartVisual (Prelude.Maybe [ColumnHierarchy])
radarChartVisual_columnHierarchies = Lens.lens (\RadarChartVisual' {columnHierarchies} -> columnHierarchies) (\s@RadarChartVisual' {} a -> s {columnHierarchies = a} :: RadarChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
radarChartVisual_subtitle :: Lens.Lens' RadarChartVisual (Prelude.Maybe VisualSubtitleLabelOptions)
radarChartVisual_subtitle = Lens.lens (\RadarChartVisual' {subtitle} -> subtitle) (\s@RadarChartVisual' {} a -> s {subtitle = a} :: RadarChartVisual)

-- | The title that is displayed on the visual.
radarChartVisual_title :: Lens.Lens' RadarChartVisual (Prelude.Maybe VisualTitleLabelOptions)
radarChartVisual_title = Lens.lens (\RadarChartVisual' {title} -> title) (\s@RadarChartVisual' {} a -> s {title = a} :: RadarChartVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
radarChartVisual_visualId :: Lens.Lens' RadarChartVisual Prelude.Text
radarChartVisual_visualId = Lens.lens (\RadarChartVisual' {visualId} -> visualId) (\s@RadarChartVisual' {} a -> s {visualId = a} :: RadarChartVisual)

instance Data.FromJSON RadarChartVisual where
  parseJSON =
    Data.withObject
      "RadarChartVisual"
      ( \x ->
          RadarChartVisual'
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

instance Prelude.Hashable RadarChartVisual where
  hashWithSalt _salt RadarChartVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData RadarChartVisual where
  rnf RadarChartVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON RadarChartVisual where
  toJSON RadarChartVisual' {..} =
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
