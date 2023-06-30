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
-- Module      : Amazonka.QuickSight.Types.HistogramVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HistogramVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.HistogramConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A histogram.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/histogram-charts.html Using histograms>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newHistogramVisual' smart constructor.
data HistogramVisual = HistogramVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration for a @HistogramVisual@.
    chartConfiguration :: Prelude.Maybe HistogramConfiguration,
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
-- Create a value of 'HistogramVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'histogramVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'histogramVisual_chartConfiguration' - The configuration for a @HistogramVisual@.
--
-- 'subtitle', 'histogramVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'histogramVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'histogramVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newHistogramVisual ::
  -- | 'visualId'
  Prelude.Text ->
  HistogramVisual
newHistogramVisual pVisualId_ =
  HistogramVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
histogramVisual_actions :: Lens.Lens' HistogramVisual (Prelude.Maybe [VisualCustomAction])
histogramVisual_actions = Lens.lens (\HistogramVisual' {actions} -> actions) (\s@HistogramVisual' {} a -> s {actions = a} :: HistogramVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for a @HistogramVisual@.
histogramVisual_chartConfiguration :: Lens.Lens' HistogramVisual (Prelude.Maybe HistogramConfiguration)
histogramVisual_chartConfiguration = Lens.lens (\HistogramVisual' {chartConfiguration} -> chartConfiguration) (\s@HistogramVisual' {} a -> s {chartConfiguration = a} :: HistogramVisual)

-- | The subtitle that is displayed on the visual.
histogramVisual_subtitle :: Lens.Lens' HistogramVisual (Prelude.Maybe VisualSubtitleLabelOptions)
histogramVisual_subtitle = Lens.lens (\HistogramVisual' {subtitle} -> subtitle) (\s@HistogramVisual' {} a -> s {subtitle = a} :: HistogramVisual)

-- | The title that is displayed on the visual.
histogramVisual_title :: Lens.Lens' HistogramVisual (Prelude.Maybe VisualTitleLabelOptions)
histogramVisual_title = Lens.lens (\HistogramVisual' {title} -> title) (\s@HistogramVisual' {} a -> s {title = a} :: HistogramVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
histogramVisual_visualId :: Lens.Lens' HistogramVisual Prelude.Text
histogramVisual_visualId = Lens.lens (\HistogramVisual' {visualId} -> visualId) (\s@HistogramVisual' {} a -> s {visualId = a} :: HistogramVisual)

instance Data.FromJSON HistogramVisual where
  parseJSON =
    Data.withObject
      "HistogramVisual"
      ( \x ->
          HistogramVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable HistogramVisual where
  hashWithSalt _salt HistogramVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData HistogramVisual where
  rnf HistogramVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON HistogramVisual where
  toJSON HistogramVisual' {..} =
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
