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
-- Module      : Amazonka.QuickSight.Types.FunnelChartVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FunnelChartVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.FunnelChartConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A funnel chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/funnel-visual-content.html Using funnel charts>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newFunnelChartVisual' smart constructor.
data FunnelChartVisual = FunnelChartVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a @FunnelChartVisual@.
    chartConfiguration :: Prelude.Maybe FunnelChartConfiguration,
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
-- Create a value of 'FunnelChartVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'funnelChartVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'funnelChartVisual_chartConfiguration' - The configuration of a @FunnelChartVisual@.
--
-- 'columnHierarchies', 'funnelChartVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'funnelChartVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'funnelChartVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'funnelChartVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newFunnelChartVisual ::
  -- | 'visualId'
  Prelude.Text ->
  FunnelChartVisual
newFunnelChartVisual pVisualId_ =
  FunnelChartVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
funnelChartVisual_actions :: Lens.Lens' FunnelChartVisual (Prelude.Maybe [VisualCustomAction])
funnelChartVisual_actions = Lens.lens (\FunnelChartVisual' {actions} -> actions) (\s@FunnelChartVisual' {} a -> s {actions = a} :: FunnelChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a @FunnelChartVisual@.
funnelChartVisual_chartConfiguration :: Lens.Lens' FunnelChartVisual (Prelude.Maybe FunnelChartConfiguration)
funnelChartVisual_chartConfiguration = Lens.lens (\FunnelChartVisual' {chartConfiguration} -> chartConfiguration) (\s@FunnelChartVisual' {} a -> s {chartConfiguration = a} :: FunnelChartVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
funnelChartVisual_columnHierarchies :: Lens.Lens' FunnelChartVisual (Prelude.Maybe [ColumnHierarchy])
funnelChartVisual_columnHierarchies = Lens.lens (\FunnelChartVisual' {columnHierarchies} -> columnHierarchies) (\s@FunnelChartVisual' {} a -> s {columnHierarchies = a} :: FunnelChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
funnelChartVisual_subtitle :: Lens.Lens' FunnelChartVisual (Prelude.Maybe VisualSubtitleLabelOptions)
funnelChartVisual_subtitle = Lens.lens (\FunnelChartVisual' {subtitle} -> subtitle) (\s@FunnelChartVisual' {} a -> s {subtitle = a} :: FunnelChartVisual)

-- | The title that is displayed on the visual.
funnelChartVisual_title :: Lens.Lens' FunnelChartVisual (Prelude.Maybe VisualTitleLabelOptions)
funnelChartVisual_title = Lens.lens (\FunnelChartVisual' {title} -> title) (\s@FunnelChartVisual' {} a -> s {title = a} :: FunnelChartVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
funnelChartVisual_visualId :: Lens.Lens' FunnelChartVisual Prelude.Text
funnelChartVisual_visualId = Lens.lens (\FunnelChartVisual' {visualId} -> visualId) (\s@FunnelChartVisual' {} a -> s {visualId = a} :: FunnelChartVisual)

instance Data.FromJSON FunnelChartVisual where
  parseJSON =
    Data.withObject
      "FunnelChartVisual"
      ( \x ->
          FunnelChartVisual'
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

instance Prelude.Hashable FunnelChartVisual where
  hashWithSalt _salt FunnelChartVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData FunnelChartVisual where
  rnf FunnelChartVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON FunnelChartVisual where
  toJSON FunnelChartVisual' {..} =
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
