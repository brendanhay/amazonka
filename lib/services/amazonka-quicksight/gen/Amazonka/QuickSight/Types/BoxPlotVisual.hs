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
-- Module      : Amazonka.QuickSight.Types.BoxPlotVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BoxPlotChartConfiguration
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A box plot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/box-plots.html Using box plots>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newBoxPlotVisual' smart constructor.
data BoxPlotVisual = BoxPlotVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe BoxPlotChartConfiguration,
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
-- Create a value of 'BoxPlotVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'boxPlotVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'boxPlotVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'boxPlotVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'boxPlotVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'boxPlotVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'boxPlotVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newBoxPlotVisual ::
  -- | 'visualId'
  Prelude.Text ->
  BoxPlotVisual
newBoxPlotVisual pVisualId_ =
  BoxPlotVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
boxPlotVisual_actions :: Lens.Lens' BoxPlotVisual (Prelude.Maybe [VisualCustomAction])
boxPlotVisual_actions = Lens.lens (\BoxPlotVisual' {actions} -> actions) (\s@BoxPlotVisual' {} a -> s {actions = a} :: BoxPlotVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
boxPlotVisual_chartConfiguration :: Lens.Lens' BoxPlotVisual (Prelude.Maybe BoxPlotChartConfiguration)
boxPlotVisual_chartConfiguration = Lens.lens (\BoxPlotVisual' {chartConfiguration} -> chartConfiguration) (\s@BoxPlotVisual' {} a -> s {chartConfiguration = a} :: BoxPlotVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
boxPlotVisual_columnHierarchies :: Lens.Lens' BoxPlotVisual (Prelude.Maybe [ColumnHierarchy])
boxPlotVisual_columnHierarchies = Lens.lens (\BoxPlotVisual' {columnHierarchies} -> columnHierarchies) (\s@BoxPlotVisual' {} a -> s {columnHierarchies = a} :: BoxPlotVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
boxPlotVisual_subtitle :: Lens.Lens' BoxPlotVisual (Prelude.Maybe VisualSubtitleLabelOptions)
boxPlotVisual_subtitle = Lens.lens (\BoxPlotVisual' {subtitle} -> subtitle) (\s@BoxPlotVisual' {} a -> s {subtitle = a} :: BoxPlotVisual)

-- | The title that is displayed on the visual.
boxPlotVisual_title :: Lens.Lens' BoxPlotVisual (Prelude.Maybe VisualTitleLabelOptions)
boxPlotVisual_title = Lens.lens (\BoxPlotVisual' {title} -> title) (\s@BoxPlotVisual' {} a -> s {title = a} :: BoxPlotVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
boxPlotVisual_visualId :: Lens.Lens' BoxPlotVisual Prelude.Text
boxPlotVisual_visualId = Lens.lens (\BoxPlotVisual' {visualId} -> visualId) (\s@BoxPlotVisual' {} a -> s {visualId = a} :: BoxPlotVisual)

instance Data.FromJSON BoxPlotVisual where
  parseJSON =
    Data.withObject
      "BoxPlotVisual"
      ( \x ->
          BoxPlotVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable BoxPlotVisual where
  hashWithSalt _salt BoxPlotVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData BoxPlotVisual where
  rnf BoxPlotVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON BoxPlotVisual where
  toJSON BoxPlotVisual' {..} =
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
