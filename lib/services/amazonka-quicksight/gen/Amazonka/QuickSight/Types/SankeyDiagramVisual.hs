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
-- Module      : Amazonka.QuickSight.Types.SankeyDiagramVisual
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SankeyDiagramVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SankeyDiagramChartConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A sankey diagram.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/sankey-diagram.html Using Sankey diagrams>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newSankeyDiagramVisual' smart constructor.
data SankeyDiagramVisual = SankeyDiagramVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a sankey diagram.
    chartConfiguration :: Prelude.Maybe SankeyDiagramChartConfiguration,
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
-- Create a value of 'SankeyDiagramVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'sankeyDiagramVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'sankeyDiagramVisual_chartConfiguration' - The configuration of a sankey diagram.
--
-- 'subtitle', 'sankeyDiagramVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'sankeyDiagramVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'sankeyDiagramVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newSankeyDiagramVisual ::
  -- | 'visualId'
  Prelude.Text ->
  SankeyDiagramVisual
newSankeyDiagramVisual pVisualId_ =
  SankeyDiagramVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
sankeyDiagramVisual_actions :: Lens.Lens' SankeyDiagramVisual (Prelude.Maybe [VisualCustomAction])
sankeyDiagramVisual_actions = Lens.lens (\SankeyDiagramVisual' {actions} -> actions) (\s@SankeyDiagramVisual' {} a -> s {actions = a} :: SankeyDiagramVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a sankey diagram.
sankeyDiagramVisual_chartConfiguration :: Lens.Lens' SankeyDiagramVisual (Prelude.Maybe SankeyDiagramChartConfiguration)
sankeyDiagramVisual_chartConfiguration = Lens.lens (\SankeyDiagramVisual' {chartConfiguration} -> chartConfiguration) (\s@SankeyDiagramVisual' {} a -> s {chartConfiguration = a} :: SankeyDiagramVisual)

-- | The subtitle that is displayed on the visual.
sankeyDiagramVisual_subtitle :: Lens.Lens' SankeyDiagramVisual (Prelude.Maybe VisualSubtitleLabelOptions)
sankeyDiagramVisual_subtitle = Lens.lens (\SankeyDiagramVisual' {subtitle} -> subtitle) (\s@SankeyDiagramVisual' {} a -> s {subtitle = a} :: SankeyDiagramVisual)

-- | The title that is displayed on the visual.
sankeyDiagramVisual_title :: Lens.Lens' SankeyDiagramVisual (Prelude.Maybe VisualTitleLabelOptions)
sankeyDiagramVisual_title = Lens.lens (\SankeyDiagramVisual' {title} -> title) (\s@SankeyDiagramVisual' {} a -> s {title = a} :: SankeyDiagramVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
sankeyDiagramVisual_visualId :: Lens.Lens' SankeyDiagramVisual Prelude.Text
sankeyDiagramVisual_visualId = Lens.lens (\SankeyDiagramVisual' {visualId} -> visualId) (\s@SankeyDiagramVisual' {} a -> s {visualId = a} :: SankeyDiagramVisual)

instance Data.FromJSON SankeyDiagramVisual where
  parseJSON =
    Data.withObject
      "SankeyDiagramVisual"
      ( \x ->
          SankeyDiagramVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable SankeyDiagramVisual where
  hashWithSalt _salt SankeyDiagramVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData SankeyDiagramVisual where
  rnf SankeyDiagramVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON SankeyDiagramVisual where
  toJSON SankeyDiagramVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("ChartConfiguration" Data..=)
              Prelude.<$> chartConfiguration,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId)
          ]
      )
