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
-- Module      : Amazonka.QuickSight.Types.FunnelChartDataLabelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FunnelChartDataLabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataLabelPosition
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.FunnelChartMeasureDataLabelStyle
import Amazonka.QuickSight.Types.Visibility

-- | The options that determine the presentation of the data labels.
--
-- /See:/ 'newFunnelChartDataLabelOptions' smart constructor.
data FunnelChartDataLabelOptions = FunnelChartDataLabelOptions'
  { -- | The visibility of the category labels within the data labels.
    categoryLabelVisibility :: Prelude.Maybe Visibility,
    -- | The color of the data label text.
    labelColor :: Prelude.Maybe Prelude.Text,
    -- | The font configuration for the data labels.
    --
    -- Only the @FontSize@ attribute of the font configuration is used for data
    -- labels.
    labelFontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | Determines the style of the metric labels.
    measureDataLabelStyle :: Prelude.Maybe FunnelChartMeasureDataLabelStyle,
    -- | The visibility of the measure labels within the data labels.
    measureLabelVisibility :: Prelude.Maybe Visibility,
    -- | Determines the positioning of the data label relative to a section of
    -- the funnel.
    position :: Prelude.Maybe DataLabelPosition,
    -- | The visibility option that determines if data labels are displayed.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunnelChartDataLabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryLabelVisibility', 'funnelChartDataLabelOptions_categoryLabelVisibility' - The visibility of the category labels within the data labels.
--
-- 'labelColor', 'funnelChartDataLabelOptions_labelColor' - The color of the data label text.
--
-- 'labelFontConfiguration', 'funnelChartDataLabelOptions_labelFontConfiguration' - The font configuration for the data labels.
--
-- Only the @FontSize@ attribute of the font configuration is used for data
-- labels.
--
-- 'measureDataLabelStyle', 'funnelChartDataLabelOptions_measureDataLabelStyle' - Determines the style of the metric labels.
--
-- 'measureLabelVisibility', 'funnelChartDataLabelOptions_measureLabelVisibility' - The visibility of the measure labels within the data labels.
--
-- 'position', 'funnelChartDataLabelOptions_position' - Determines the positioning of the data label relative to a section of
-- the funnel.
--
-- 'visibility', 'funnelChartDataLabelOptions_visibility' - The visibility option that determines if data labels are displayed.
newFunnelChartDataLabelOptions ::
  FunnelChartDataLabelOptions
newFunnelChartDataLabelOptions =
  FunnelChartDataLabelOptions'
    { categoryLabelVisibility =
        Prelude.Nothing,
      labelColor = Prelude.Nothing,
      labelFontConfiguration = Prelude.Nothing,
      measureDataLabelStyle = Prelude.Nothing,
      measureLabelVisibility = Prelude.Nothing,
      position = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The visibility of the category labels within the data labels.
funnelChartDataLabelOptions_categoryLabelVisibility :: Lens.Lens' FunnelChartDataLabelOptions (Prelude.Maybe Visibility)
funnelChartDataLabelOptions_categoryLabelVisibility = Lens.lens (\FunnelChartDataLabelOptions' {categoryLabelVisibility} -> categoryLabelVisibility) (\s@FunnelChartDataLabelOptions' {} a -> s {categoryLabelVisibility = a} :: FunnelChartDataLabelOptions)

-- | The color of the data label text.
funnelChartDataLabelOptions_labelColor :: Lens.Lens' FunnelChartDataLabelOptions (Prelude.Maybe Prelude.Text)
funnelChartDataLabelOptions_labelColor = Lens.lens (\FunnelChartDataLabelOptions' {labelColor} -> labelColor) (\s@FunnelChartDataLabelOptions' {} a -> s {labelColor = a} :: FunnelChartDataLabelOptions)

-- | The font configuration for the data labels.
--
-- Only the @FontSize@ attribute of the font configuration is used for data
-- labels.
funnelChartDataLabelOptions_labelFontConfiguration :: Lens.Lens' FunnelChartDataLabelOptions (Prelude.Maybe FontConfiguration)
funnelChartDataLabelOptions_labelFontConfiguration = Lens.lens (\FunnelChartDataLabelOptions' {labelFontConfiguration} -> labelFontConfiguration) (\s@FunnelChartDataLabelOptions' {} a -> s {labelFontConfiguration = a} :: FunnelChartDataLabelOptions)

-- | Determines the style of the metric labels.
funnelChartDataLabelOptions_measureDataLabelStyle :: Lens.Lens' FunnelChartDataLabelOptions (Prelude.Maybe FunnelChartMeasureDataLabelStyle)
funnelChartDataLabelOptions_measureDataLabelStyle = Lens.lens (\FunnelChartDataLabelOptions' {measureDataLabelStyle} -> measureDataLabelStyle) (\s@FunnelChartDataLabelOptions' {} a -> s {measureDataLabelStyle = a} :: FunnelChartDataLabelOptions)

-- | The visibility of the measure labels within the data labels.
funnelChartDataLabelOptions_measureLabelVisibility :: Lens.Lens' FunnelChartDataLabelOptions (Prelude.Maybe Visibility)
funnelChartDataLabelOptions_measureLabelVisibility = Lens.lens (\FunnelChartDataLabelOptions' {measureLabelVisibility} -> measureLabelVisibility) (\s@FunnelChartDataLabelOptions' {} a -> s {measureLabelVisibility = a} :: FunnelChartDataLabelOptions)

-- | Determines the positioning of the data label relative to a section of
-- the funnel.
funnelChartDataLabelOptions_position :: Lens.Lens' FunnelChartDataLabelOptions (Prelude.Maybe DataLabelPosition)
funnelChartDataLabelOptions_position = Lens.lens (\FunnelChartDataLabelOptions' {position} -> position) (\s@FunnelChartDataLabelOptions' {} a -> s {position = a} :: FunnelChartDataLabelOptions)

-- | The visibility option that determines if data labels are displayed.
funnelChartDataLabelOptions_visibility :: Lens.Lens' FunnelChartDataLabelOptions (Prelude.Maybe Visibility)
funnelChartDataLabelOptions_visibility = Lens.lens (\FunnelChartDataLabelOptions' {visibility} -> visibility) (\s@FunnelChartDataLabelOptions' {} a -> s {visibility = a} :: FunnelChartDataLabelOptions)

instance Data.FromJSON FunnelChartDataLabelOptions where
  parseJSON =
    Data.withObject
      "FunnelChartDataLabelOptions"
      ( \x ->
          FunnelChartDataLabelOptions'
            Prelude.<$> (x Data..:? "CategoryLabelVisibility")
            Prelude.<*> (x Data..:? "LabelColor")
            Prelude.<*> (x Data..:? "LabelFontConfiguration")
            Prelude.<*> (x Data..:? "MeasureDataLabelStyle")
            Prelude.<*> (x Data..:? "MeasureLabelVisibility")
            Prelude.<*> (x Data..:? "Position")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable FunnelChartDataLabelOptions where
  hashWithSalt _salt FunnelChartDataLabelOptions' {..} =
    _salt
      `Prelude.hashWithSalt` categoryLabelVisibility
      `Prelude.hashWithSalt` labelColor
      `Prelude.hashWithSalt` labelFontConfiguration
      `Prelude.hashWithSalt` measureDataLabelStyle
      `Prelude.hashWithSalt` measureLabelVisibility
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData FunnelChartDataLabelOptions where
  rnf FunnelChartDataLabelOptions' {..} =
    Prelude.rnf categoryLabelVisibility `Prelude.seq`
      Prelude.rnf labelColor `Prelude.seq`
        Prelude.rnf labelFontConfiguration `Prelude.seq`
          Prelude.rnf measureDataLabelStyle `Prelude.seq`
            Prelude.rnf measureLabelVisibility `Prelude.seq`
              Prelude.rnf position `Prelude.seq`
                Prelude.rnf visibility

instance Data.ToJSON FunnelChartDataLabelOptions where
  toJSON FunnelChartDataLabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryLabelVisibility" Data..=)
              Prelude.<$> categoryLabelVisibility,
            ("LabelColor" Data..=) Prelude.<$> labelColor,
            ("LabelFontConfiguration" Data..=)
              Prelude.<$> labelFontConfiguration,
            ("MeasureDataLabelStyle" Data..=)
              Prelude.<$> measureDataLabelStyle,
            ("MeasureLabelVisibility" Data..=)
              Prelude.<$> measureLabelVisibility,
            ("Position" Data..=) Prelude.<$> position,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
