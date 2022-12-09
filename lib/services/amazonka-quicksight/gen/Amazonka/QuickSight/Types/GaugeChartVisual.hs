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
-- Module      : Amazonka.QuickSight.Types.GaugeChartVisual
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GaugeChartConditionalFormatting
import Amazonka.QuickSight.Types.GaugeChartConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A gauge chart.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/gauge-chart.html Using gauge charts>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newGaugeChartVisual' smart constructor.
data GaugeChartVisual = GaugeChartVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a @GaugeChartVisual@.
    chartConfiguration :: Prelude.Maybe GaugeChartConfiguration,
    -- | The conditional formatting of a @GaugeChartVisual@.
    conditionalFormatting :: Prelude.Maybe GaugeChartConditionalFormatting,
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
-- Create a value of 'GaugeChartVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'gaugeChartVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'gaugeChartVisual_chartConfiguration' - The configuration of a @GaugeChartVisual@.
--
-- 'conditionalFormatting', 'gaugeChartVisual_conditionalFormatting' - The conditional formatting of a @GaugeChartVisual@.
--
-- 'subtitle', 'gaugeChartVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'gaugeChartVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'gaugeChartVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newGaugeChartVisual ::
  -- | 'visualId'
  Prelude.Text ->
  GaugeChartVisual
newGaugeChartVisual pVisualId_ =
  GaugeChartVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      conditionalFormatting = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
gaugeChartVisual_actions :: Lens.Lens' GaugeChartVisual (Prelude.Maybe [VisualCustomAction])
gaugeChartVisual_actions = Lens.lens (\GaugeChartVisual' {actions} -> actions) (\s@GaugeChartVisual' {} a -> s {actions = a} :: GaugeChartVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a @GaugeChartVisual@.
gaugeChartVisual_chartConfiguration :: Lens.Lens' GaugeChartVisual (Prelude.Maybe GaugeChartConfiguration)
gaugeChartVisual_chartConfiguration = Lens.lens (\GaugeChartVisual' {chartConfiguration} -> chartConfiguration) (\s@GaugeChartVisual' {} a -> s {chartConfiguration = a} :: GaugeChartVisual)

-- | The conditional formatting of a @GaugeChartVisual@.
gaugeChartVisual_conditionalFormatting :: Lens.Lens' GaugeChartVisual (Prelude.Maybe GaugeChartConditionalFormatting)
gaugeChartVisual_conditionalFormatting = Lens.lens (\GaugeChartVisual' {conditionalFormatting} -> conditionalFormatting) (\s@GaugeChartVisual' {} a -> s {conditionalFormatting = a} :: GaugeChartVisual)

-- | The subtitle that is displayed on the visual.
gaugeChartVisual_subtitle :: Lens.Lens' GaugeChartVisual (Prelude.Maybe VisualSubtitleLabelOptions)
gaugeChartVisual_subtitle = Lens.lens (\GaugeChartVisual' {subtitle} -> subtitle) (\s@GaugeChartVisual' {} a -> s {subtitle = a} :: GaugeChartVisual)

-- | The title that is displayed on the visual.
gaugeChartVisual_title :: Lens.Lens' GaugeChartVisual (Prelude.Maybe VisualTitleLabelOptions)
gaugeChartVisual_title = Lens.lens (\GaugeChartVisual' {title} -> title) (\s@GaugeChartVisual' {} a -> s {title = a} :: GaugeChartVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
gaugeChartVisual_visualId :: Lens.Lens' GaugeChartVisual Prelude.Text
gaugeChartVisual_visualId = Lens.lens (\GaugeChartVisual' {visualId} -> visualId) (\s@GaugeChartVisual' {} a -> s {visualId = a} :: GaugeChartVisual)

instance Data.FromJSON GaugeChartVisual where
  parseJSON =
    Data.withObject
      "GaugeChartVisual"
      ( \x ->
          GaugeChartVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> (x Data..:? "ConditionalFormatting")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable GaugeChartVisual where
  hashWithSalt _salt GaugeChartVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` conditionalFormatting
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData GaugeChartVisual where
  rnf GaugeChartVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf conditionalFormatting
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON GaugeChartVisual where
  toJSON GaugeChartVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("ChartConfiguration" Data..=)
              Prelude.<$> chartConfiguration,
            ("ConditionalFormatting" Data..=)
              Prelude.<$> conditionalFormatting,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId)
          ]
      )
