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
-- Module      : Amazonka.QuickSight.Types.KPIOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ComparisonConfiguration
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.PrimaryValueDisplayType
import Amazonka.QuickSight.Types.ProgressBarOptions
import Amazonka.QuickSight.Types.SecondaryValueOptions
import Amazonka.QuickSight.Types.TrendArrowOptions

-- | The options that determine the presentation of a KPI visual.
--
-- /See:/ 'newKPIOptions' smart constructor.
data KPIOptions = KPIOptions'
  { -- | The comparison configuration of a KPI visual.
    comparison :: Prelude.Maybe ComparisonConfiguration,
    -- | The options that determine the primary value display type.
    primaryValueDisplayType :: Prelude.Maybe PrimaryValueDisplayType,
    -- | The options that determine the primary value font configuration.
    primaryValueFontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | The options that determine the presentation of the progress bar of a KPI
    -- visual.
    progressBar :: Prelude.Maybe ProgressBarOptions,
    -- | The options that determine the presentation of the secondary value of a
    -- KPI visual.
    secondaryValue :: Prelude.Maybe SecondaryValueOptions,
    -- | The options that determine the secondary value font configuration.
    secondaryValueFontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | The options that determine the presentation of trend arrows in a KPI
    -- visual.
    trendArrows :: Prelude.Maybe TrendArrowOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparison', 'kPIOptions_comparison' - The comparison configuration of a KPI visual.
--
-- 'primaryValueDisplayType', 'kPIOptions_primaryValueDisplayType' - The options that determine the primary value display type.
--
-- 'primaryValueFontConfiguration', 'kPIOptions_primaryValueFontConfiguration' - The options that determine the primary value font configuration.
--
-- 'progressBar', 'kPIOptions_progressBar' - The options that determine the presentation of the progress bar of a KPI
-- visual.
--
-- 'secondaryValue', 'kPIOptions_secondaryValue' - The options that determine the presentation of the secondary value of a
-- KPI visual.
--
-- 'secondaryValueFontConfiguration', 'kPIOptions_secondaryValueFontConfiguration' - The options that determine the secondary value font configuration.
--
-- 'trendArrows', 'kPIOptions_trendArrows' - The options that determine the presentation of trend arrows in a KPI
-- visual.
newKPIOptions ::
  KPIOptions
newKPIOptions =
  KPIOptions'
    { comparison = Prelude.Nothing,
      primaryValueDisplayType = Prelude.Nothing,
      primaryValueFontConfiguration = Prelude.Nothing,
      progressBar = Prelude.Nothing,
      secondaryValue = Prelude.Nothing,
      secondaryValueFontConfiguration = Prelude.Nothing,
      trendArrows = Prelude.Nothing
    }

-- | The comparison configuration of a KPI visual.
kPIOptions_comparison :: Lens.Lens' KPIOptions (Prelude.Maybe ComparisonConfiguration)
kPIOptions_comparison = Lens.lens (\KPIOptions' {comparison} -> comparison) (\s@KPIOptions' {} a -> s {comparison = a} :: KPIOptions)

-- | The options that determine the primary value display type.
kPIOptions_primaryValueDisplayType :: Lens.Lens' KPIOptions (Prelude.Maybe PrimaryValueDisplayType)
kPIOptions_primaryValueDisplayType = Lens.lens (\KPIOptions' {primaryValueDisplayType} -> primaryValueDisplayType) (\s@KPIOptions' {} a -> s {primaryValueDisplayType = a} :: KPIOptions)

-- | The options that determine the primary value font configuration.
kPIOptions_primaryValueFontConfiguration :: Lens.Lens' KPIOptions (Prelude.Maybe FontConfiguration)
kPIOptions_primaryValueFontConfiguration = Lens.lens (\KPIOptions' {primaryValueFontConfiguration} -> primaryValueFontConfiguration) (\s@KPIOptions' {} a -> s {primaryValueFontConfiguration = a} :: KPIOptions)

-- | The options that determine the presentation of the progress bar of a KPI
-- visual.
kPIOptions_progressBar :: Lens.Lens' KPIOptions (Prelude.Maybe ProgressBarOptions)
kPIOptions_progressBar = Lens.lens (\KPIOptions' {progressBar} -> progressBar) (\s@KPIOptions' {} a -> s {progressBar = a} :: KPIOptions)

-- | The options that determine the presentation of the secondary value of a
-- KPI visual.
kPIOptions_secondaryValue :: Lens.Lens' KPIOptions (Prelude.Maybe SecondaryValueOptions)
kPIOptions_secondaryValue = Lens.lens (\KPIOptions' {secondaryValue} -> secondaryValue) (\s@KPIOptions' {} a -> s {secondaryValue = a} :: KPIOptions)

-- | The options that determine the secondary value font configuration.
kPIOptions_secondaryValueFontConfiguration :: Lens.Lens' KPIOptions (Prelude.Maybe FontConfiguration)
kPIOptions_secondaryValueFontConfiguration = Lens.lens (\KPIOptions' {secondaryValueFontConfiguration} -> secondaryValueFontConfiguration) (\s@KPIOptions' {} a -> s {secondaryValueFontConfiguration = a} :: KPIOptions)

-- | The options that determine the presentation of trend arrows in a KPI
-- visual.
kPIOptions_trendArrows :: Lens.Lens' KPIOptions (Prelude.Maybe TrendArrowOptions)
kPIOptions_trendArrows = Lens.lens (\KPIOptions' {trendArrows} -> trendArrows) (\s@KPIOptions' {} a -> s {trendArrows = a} :: KPIOptions)

instance Data.FromJSON KPIOptions where
  parseJSON =
    Data.withObject
      "KPIOptions"
      ( \x ->
          KPIOptions'
            Prelude.<$> (x Data..:? "Comparison")
            Prelude.<*> (x Data..:? "PrimaryValueDisplayType")
            Prelude.<*> (x Data..:? "PrimaryValueFontConfiguration")
            Prelude.<*> (x Data..:? "ProgressBar")
            Prelude.<*> (x Data..:? "SecondaryValue")
            Prelude.<*> (x Data..:? "SecondaryValueFontConfiguration")
            Prelude.<*> (x Data..:? "TrendArrows")
      )

instance Prelude.Hashable KPIOptions where
  hashWithSalt _salt KPIOptions' {..} =
    _salt
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` primaryValueDisplayType
      `Prelude.hashWithSalt` primaryValueFontConfiguration
      `Prelude.hashWithSalt` progressBar
      `Prelude.hashWithSalt` secondaryValue
      `Prelude.hashWithSalt` secondaryValueFontConfiguration
      `Prelude.hashWithSalt` trendArrows

instance Prelude.NFData KPIOptions where
  rnf KPIOptions' {..} =
    Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf primaryValueDisplayType
      `Prelude.seq` Prelude.rnf primaryValueFontConfiguration
      `Prelude.seq` Prelude.rnf progressBar
      `Prelude.seq` Prelude.rnf secondaryValue
      `Prelude.seq` Prelude.rnf secondaryValueFontConfiguration
      `Prelude.seq` Prelude.rnf trendArrows

instance Data.ToJSON KPIOptions where
  toJSON KPIOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Comparison" Data..=) Prelude.<$> comparison,
            ("PrimaryValueDisplayType" Data..=)
              Prelude.<$> primaryValueDisplayType,
            ("PrimaryValueFontConfiguration" Data..=)
              Prelude.<$> primaryValueFontConfiguration,
            ("ProgressBar" Data..=) Prelude.<$> progressBar,
            ("SecondaryValue" Data..=)
              Prelude.<$> secondaryValue,
            ("SecondaryValueFontConfiguration" Data..=)
              Prelude.<$> secondaryValueFontConfiguration,
            ("TrendArrows" Data..=) Prelude.<$> trendArrows
          ]
      )
