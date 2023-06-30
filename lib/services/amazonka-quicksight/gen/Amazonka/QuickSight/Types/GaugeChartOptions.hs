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
-- Module      : Amazonka.QuickSight.Types.GaugeChartOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ArcAxisConfiguration
import Amazonka.QuickSight.Types.ArcConfiguration
import Amazonka.QuickSight.Types.ComparisonConfiguration
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.PrimaryValueDisplayType

-- | The options that determine the presentation of the @GaugeChartVisual@.
--
-- /See:/ 'newGaugeChartOptions' smart constructor.
data GaugeChartOptions = GaugeChartOptions'
  { -- | The arc configuration of a @GaugeChartVisual@.
    arc :: Prelude.Maybe ArcConfiguration,
    -- | The arc axis configuration of a @GaugeChartVisual@.
    arcAxis :: Prelude.Maybe ArcAxisConfiguration,
    -- | The comparison configuration of a @GaugeChartVisual@.
    comparison :: Prelude.Maybe ComparisonConfiguration,
    -- | The options that determine the primary value display type.
    primaryValueDisplayType :: Prelude.Maybe PrimaryValueDisplayType,
    -- | The options that determine the primary value font configuration.
    primaryValueFontConfiguration :: Prelude.Maybe FontConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GaugeChartOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arc', 'gaugeChartOptions_arc' - The arc configuration of a @GaugeChartVisual@.
--
-- 'arcAxis', 'gaugeChartOptions_arcAxis' - The arc axis configuration of a @GaugeChartVisual@.
--
-- 'comparison', 'gaugeChartOptions_comparison' - The comparison configuration of a @GaugeChartVisual@.
--
-- 'primaryValueDisplayType', 'gaugeChartOptions_primaryValueDisplayType' - The options that determine the primary value display type.
--
-- 'primaryValueFontConfiguration', 'gaugeChartOptions_primaryValueFontConfiguration' - The options that determine the primary value font configuration.
newGaugeChartOptions ::
  GaugeChartOptions
newGaugeChartOptions =
  GaugeChartOptions'
    { arc = Prelude.Nothing,
      arcAxis = Prelude.Nothing,
      comparison = Prelude.Nothing,
      primaryValueDisplayType = Prelude.Nothing,
      primaryValueFontConfiguration = Prelude.Nothing
    }

-- | The arc configuration of a @GaugeChartVisual@.
gaugeChartOptions_arc :: Lens.Lens' GaugeChartOptions (Prelude.Maybe ArcConfiguration)
gaugeChartOptions_arc = Lens.lens (\GaugeChartOptions' {arc} -> arc) (\s@GaugeChartOptions' {} a -> s {arc = a} :: GaugeChartOptions)

-- | The arc axis configuration of a @GaugeChartVisual@.
gaugeChartOptions_arcAxis :: Lens.Lens' GaugeChartOptions (Prelude.Maybe ArcAxisConfiguration)
gaugeChartOptions_arcAxis = Lens.lens (\GaugeChartOptions' {arcAxis} -> arcAxis) (\s@GaugeChartOptions' {} a -> s {arcAxis = a} :: GaugeChartOptions)

-- | The comparison configuration of a @GaugeChartVisual@.
gaugeChartOptions_comparison :: Lens.Lens' GaugeChartOptions (Prelude.Maybe ComparisonConfiguration)
gaugeChartOptions_comparison = Lens.lens (\GaugeChartOptions' {comparison} -> comparison) (\s@GaugeChartOptions' {} a -> s {comparison = a} :: GaugeChartOptions)

-- | The options that determine the primary value display type.
gaugeChartOptions_primaryValueDisplayType :: Lens.Lens' GaugeChartOptions (Prelude.Maybe PrimaryValueDisplayType)
gaugeChartOptions_primaryValueDisplayType = Lens.lens (\GaugeChartOptions' {primaryValueDisplayType} -> primaryValueDisplayType) (\s@GaugeChartOptions' {} a -> s {primaryValueDisplayType = a} :: GaugeChartOptions)

-- | The options that determine the primary value font configuration.
gaugeChartOptions_primaryValueFontConfiguration :: Lens.Lens' GaugeChartOptions (Prelude.Maybe FontConfiguration)
gaugeChartOptions_primaryValueFontConfiguration = Lens.lens (\GaugeChartOptions' {primaryValueFontConfiguration} -> primaryValueFontConfiguration) (\s@GaugeChartOptions' {} a -> s {primaryValueFontConfiguration = a} :: GaugeChartOptions)

instance Data.FromJSON GaugeChartOptions where
  parseJSON =
    Data.withObject
      "GaugeChartOptions"
      ( \x ->
          GaugeChartOptions'
            Prelude.<$> (x Data..:? "Arc")
            Prelude.<*> (x Data..:? "ArcAxis")
            Prelude.<*> (x Data..:? "Comparison")
            Prelude.<*> (x Data..:? "PrimaryValueDisplayType")
            Prelude.<*> (x Data..:? "PrimaryValueFontConfiguration")
      )

instance Prelude.Hashable GaugeChartOptions where
  hashWithSalt _salt GaugeChartOptions' {..} =
    _salt
      `Prelude.hashWithSalt` arc
      `Prelude.hashWithSalt` arcAxis
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` primaryValueDisplayType
      `Prelude.hashWithSalt` primaryValueFontConfiguration

instance Prelude.NFData GaugeChartOptions where
  rnf GaugeChartOptions' {..} =
    Prelude.rnf arc
      `Prelude.seq` Prelude.rnf arcAxis
      `Prelude.seq` Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf primaryValueDisplayType
      `Prelude.seq` Prelude.rnf primaryValueFontConfiguration

instance Data.ToJSON GaugeChartOptions where
  toJSON GaugeChartOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arc" Data..=) Prelude.<$> arc,
            ("ArcAxis" Data..=) Prelude.<$> arcAxis,
            ("Comparison" Data..=) Prelude.<$> comparison,
            ("PrimaryValueDisplayType" Data..=)
              Prelude.<$> primaryValueDisplayType,
            ("PrimaryValueFontConfiguration" Data..=)
              Prelude.<$> primaryValueFontConfiguration
          ]
      )
