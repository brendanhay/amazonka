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
-- Module      : Amazonka.QuickSight.Types.GeospatialMapVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialMapVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.GeospatialMapConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A geospatial map or a points on map visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/point-maps.html Creating point maps>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newGeospatialMapVisual' smart constructor.
data GeospatialMapVisual = GeospatialMapVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe GeospatialMapConfiguration,
    -- | The column hierarchy that is used during drill-downs and drill-ups.
    columnHierarchies :: Prelude.Maybe [ColumnHierarchy],
    -- | The subtitle that is displayed on the visual.
    subtitle :: Prelude.Maybe VisualSubtitleLabelOptions,
    -- | The title that is displayed on the visual.
    title :: Prelude.Maybe VisualTitleLabelOptions,
    -- | The unique identifier of a visual. This identifier must be unique within
    -- the context of a dashboard, template, or analysis. Two dashboards,
    -- analyses, or templates can have visuals with the same identifiers..
    visualId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialMapVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'geospatialMapVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'geospatialMapVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'geospatialMapVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'geospatialMapVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'geospatialMapVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'geospatialMapVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newGeospatialMapVisual ::
  -- | 'visualId'
  Prelude.Text ->
  GeospatialMapVisual
newGeospatialMapVisual pVisualId_ =
  GeospatialMapVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
geospatialMapVisual_actions :: Lens.Lens' GeospatialMapVisual (Prelude.Maybe [VisualCustomAction])
geospatialMapVisual_actions = Lens.lens (\GeospatialMapVisual' {actions} -> actions) (\s@GeospatialMapVisual' {} a -> s {actions = a} :: GeospatialMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
geospatialMapVisual_chartConfiguration :: Lens.Lens' GeospatialMapVisual (Prelude.Maybe GeospatialMapConfiguration)
geospatialMapVisual_chartConfiguration = Lens.lens (\GeospatialMapVisual' {chartConfiguration} -> chartConfiguration) (\s@GeospatialMapVisual' {} a -> s {chartConfiguration = a} :: GeospatialMapVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
geospatialMapVisual_columnHierarchies :: Lens.Lens' GeospatialMapVisual (Prelude.Maybe [ColumnHierarchy])
geospatialMapVisual_columnHierarchies = Lens.lens (\GeospatialMapVisual' {columnHierarchies} -> columnHierarchies) (\s@GeospatialMapVisual' {} a -> s {columnHierarchies = a} :: GeospatialMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
geospatialMapVisual_subtitle :: Lens.Lens' GeospatialMapVisual (Prelude.Maybe VisualSubtitleLabelOptions)
geospatialMapVisual_subtitle = Lens.lens (\GeospatialMapVisual' {subtitle} -> subtitle) (\s@GeospatialMapVisual' {} a -> s {subtitle = a} :: GeospatialMapVisual)

-- | The title that is displayed on the visual.
geospatialMapVisual_title :: Lens.Lens' GeospatialMapVisual (Prelude.Maybe VisualTitleLabelOptions)
geospatialMapVisual_title = Lens.lens (\GeospatialMapVisual' {title} -> title) (\s@GeospatialMapVisual' {} a -> s {title = a} :: GeospatialMapVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
geospatialMapVisual_visualId :: Lens.Lens' GeospatialMapVisual Prelude.Text
geospatialMapVisual_visualId = Lens.lens (\GeospatialMapVisual' {visualId} -> visualId) (\s@GeospatialMapVisual' {} a -> s {visualId = a} :: GeospatialMapVisual)

instance Data.FromJSON GeospatialMapVisual where
  parseJSON =
    Data.withObject
      "GeospatialMapVisual"
      ( \x ->
          GeospatialMapVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable GeospatialMapVisual where
  hashWithSalt _salt GeospatialMapVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData GeospatialMapVisual where
  rnf GeospatialMapVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON GeospatialMapVisual where
  toJSON GeospatialMapVisual' {..} =
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
