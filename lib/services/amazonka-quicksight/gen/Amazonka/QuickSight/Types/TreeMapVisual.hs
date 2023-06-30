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
-- Module      : Amazonka.QuickSight.Types.TreeMapVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TreeMapVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.TreeMapConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A tree map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/tree-map.html Using tree maps>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newTreeMapVisual' smart constructor.
data TreeMapVisual = TreeMapVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe TreeMapConfiguration,
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
-- Create a value of 'TreeMapVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'treeMapVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'treeMapVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'treeMapVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'treeMapVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'treeMapVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'treeMapVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newTreeMapVisual ::
  -- | 'visualId'
  Prelude.Text ->
  TreeMapVisual
newTreeMapVisual pVisualId_ =
  TreeMapVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
treeMapVisual_actions :: Lens.Lens' TreeMapVisual (Prelude.Maybe [VisualCustomAction])
treeMapVisual_actions = Lens.lens (\TreeMapVisual' {actions} -> actions) (\s@TreeMapVisual' {} a -> s {actions = a} :: TreeMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
treeMapVisual_chartConfiguration :: Lens.Lens' TreeMapVisual (Prelude.Maybe TreeMapConfiguration)
treeMapVisual_chartConfiguration = Lens.lens (\TreeMapVisual' {chartConfiguration} -> chartConfiguration) (\s@TreeMapVisual' {} a -> s {chartConfiguration = a} :: TreeMapVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
treeMapVisual_columnHierarchies :: Lens.Lens' TreeMapVisual (Prelude.Maybe [ColumnHierarchy])
treeMapVisual_columnHierarchies = Lens.lens (\TreeMapVisual' {columnHierarchies} -> columnHierarchies) (\s@TreeMapVisual' {} a -> s {columnHierarchies = a} :: TreeMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
treeMapVisual_subtitle :: Lens.Lens' TreeMapVisual (Prelude.Maybe VisualSubtitleLabelOptions)
treeMapVisual_subtitle = Lens.lens (\TreeMapVisual' {subtitle} -> subtitle) (\s@TreeMapVisual' {} a -> s {subtitle = a} :: TreeMapVisual)

-- | The title that is displayed on the visual.
treeMapVisual_title :: Lens.Lens' TreeMapVisual (Prelude.Maybe VisualTitleLabelOptions)
treeMapVisual_title = Lens.lens (\TreeMapVisual' {title} -> title) (\s@TreeMapVisual' {} a -> s {title = a} :: TreeMapVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
treeMapVisual_visualId :: Lens.Lens' TreeMapVisual Prelude.Text
treeMapVisual_visualId = Lens.lens (\TreeMapVisual' {visualId} -> visualId) (\s@TreeMapVisual' {} a -> s {visualId = a} :: TreeMapVisual)

instance Data.FromJSON TreeMapVisual where
  parseJSON =
    Data.withObject
      "TreeMapVisual"
      ( \x ->
          TreeMapVisual'
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

instance Prelude.Hashable TreeMapVisual where
  hashWithSalt _salt TreeMapVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData TreeMapVisual where
  rnf TreeMapVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON TreeMapVisual where
  toJSON TreeMapVisual' {..} =
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
