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
-- Module      : Amazonka.QuickSight.Types.ScatterPlotVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ScatterPlotVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.ScatterPlotConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A scatter plot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/scatter-plot.html Using scatter plots>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newScatterPlotVisual' smart constructor.
data ScatterPlotVisual = ScatterPlotVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe ScatterPlotConfiguration,
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
-- Create a value of 'ScatterPlotVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'scatterPlotVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'scatterPlotVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'scatterPlotVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'scatterPlotVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'scatterPlotVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'scatterPlotVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newScatterPlotVisual ::
  -- | 'visualId'
  Prelude.Text ->
  ScatterPlotVisual
newScatterPlotVisual pVisualId_ =
  ScatterPlotVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
scatterPlotVisual_actions :: Lens.Lens' ScatterPlotVisual (Prelude.Maybe [VisualCustomAction])
scatterPlotVisual_actions = Lens.lens (\ScatterPlotVisual' {actions} -> actions) (\s@ScatterPlotVisual' {} a -> s {actions = a} :: ScatterPlotVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
scatterPlotVisual_chartConfiguration :: Lens.Lens' ScatterPlotVisual (Prelude.Maybe ScatterPlotConfiguration)
scatterPlotVisual_chartConfiguration = Lens.lens (\ScatterPlotVisual' {chartConfiguration} -> chartConfiguration) (\s@ScatterPlotVisual' {} a -> s {chartConfiguration = a} :: ScatterPlotVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
scatterPlotVisual_columnHierarchies :: Lens.Lens' ScatterPlotVisual (Prelude.Maybe [ColumnHierarchy])
scatterPlotVisual_columnHierarchies = Lens.lens (\ScatterPlotVisual' {columnHierarchies} -> columnHierarchies) (\s@ScatterPlotVisual' {} a -> s {columnHierarchies = a} :: ScatterPlotVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
scatterPlotVisual_subtitle :: Lens.Lens' ScatterPlotVisual (Prelude.Maybe VisualSubtitleLabelOptions)
scatterPlotVisual_subtitle = Lens.lens (\ScatterPlotVisual' {subtitle} -> subtitle) (\s@ScatterPlotVisual' {} a -> s {subtitle = a} :: ScatterPlotVisual)

-- | The title that is displayed on the visual.
scatterPlotVisual_title :: Lens.Lens' ScatterPlotVisual (Prelude.Maybe VisualTitleLabelOptions)
scatterPlotVisual_title = Lens.lens (\ScatterPlotVisual' {title} -> title) (\s@ScatterPlotVisual' {} a -> s {title = a} :: ScatterPlotVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
scatterPlotVisual_visualId :: Lens.Lens' ScatterPlotVisual Prelude.Text
scatterPlotVisual_visualId = Lens.lens (\ScatterPlotVisual' {visualId} -> visualId) (\s@ScatterPlotVisual' {} a -> s {visualId = a} :: ScatterPlotVisual)

instance Data.FromJSON ScatterPlotVisual where
  parseJSON =
    Data.withObject
      "ScatterPlotVisual"
      ( \x ->
          ScatterPlotVisual'
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

instance Prelude.Hashable ScatterPlotVisual where
  hashWithSalt _salt ScatterPlotVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData ScatterPlotVisual where
  rnf ScatterPlotVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON ScatterPlotVisual where
  toJSON ScatterPlotVisual' {..} =
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
