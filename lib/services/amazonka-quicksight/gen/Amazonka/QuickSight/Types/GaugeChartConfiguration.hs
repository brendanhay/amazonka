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
-- Module      : Amazonka.QuickSight.Types.GaugeChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.GaugeChartFieldWells
import Amazonka.QuickSight.Types.GaugeChartOptions
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a @GaugeChartVisual@.
--
-- /See:/ 'newGaugeChartConfiguration' smart constructor.
data GaugeChartConfiguration = GaugeChartConfiguration'
  { -- | The data label configuration of a @GaugeChartVisual@.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field well configuration of a @GaugeChartVisual@.
    fieldWells :: Prelude.Maybe GaugeChartFieldWells,
    -- | The options that determine the presentation of the @GaugeChartVisual@.
    gaugeChartOptions :: Prelude.Maybe GaugeChartOptions,
    -- | The tooltip configuration of a @GaugeChartVisual@.
    tooltipOptions :: Prelude.Maybe TooltipOptions,
    -- | The visual palette configuration of a @GaugeChartVisual@.
    visualPalette :: Prelude.Maybe VisualPalette
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GaugeChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLabels', 'gaugeChartConfiguration_dataLabels' - The data label configuration of a @GaugeChartVisual@.
--
-- 'fieldWells', 'gaugeChartConfiguration_fieldWells' - The field well configuration of a @GaugeChartVisual@.
--
-- 'gaugeChartOptions', 'gaugeChartConfiguration_gaugeChartOptions' - The options that determine the presentation of the @GaugeChartVisual@.
--
-- 'tooltipOptions', 'gaugeChartConfiguration_tooltipOptions' - The tooltip configuration of a @GaugeChartVisual@.
--
-- 'visualPalette', 'gaugeChartConfiguration_visualPalette' - The visual palette configuration of a @GaugeChartVisual@.
newGaugeChartConfiguration ::
  GaugeChartConfiguration
newGaugeChartConfiguration =
  GaugeChartConfiguration'
    { dataLabels =
        Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      gaugeChartOptions = Prelude.Nothing,
      tooltipOptions = Prelude.Nothing,
      visualPalette = Prelude.Nothing
    }

-- | The data label configuration of a @GaugeChartVisual@.
gaugeChartConfiguration_dataLabels :: Lens.Lens' GaugeChartConfiguration (Prelude.Maybe DataLabelOptions)
gaugeChartConfiguration_dataLabels = Lens.lens (\GaugeChartConfiguration' {dataLabels} -> dataLabels) (\s@GaugeChartConfiguration' {} a -> s {dataLabels = a} :: GaugeChartConfiguration)

-- | The field well configuration of a @GaugeChartVisual@.
gaugeChartConfiguration_fieldWells :: Lens.Lens' GaugeChartConfiguration (Prelude.Maybe GaugeChartFieldWells)
gaugeChartConfiguration_fieldWells = Lens.lens (\GaugeChartConfiguration' {fieldWells} -> fieldWells) (\s@GaugeChartConfiguration' {} a -> s {fieldWells = a} :: GaugeChartConfiguration)

-- | The options that determine the presentation of the @GaugeChartVisual@.
gaugeChartConfiguration_gaugeChartOptions :: Lens.Lens' GaugeChartConfiguration (Prelude.Maybe GaugeChartOptions)
gaugeChartConfiguration_gaugeChartOptions = Lens.lens (\GaugeChartConfiguration' {gaugeChartOptions} -> gaugeChartOptions) (\s@GaugeChartConfiguration' {} a -> s {gaugeChartOptions = a} :: GaugeChartConfiguration)

-- | The tooltip configuration of a @GaugeChartVisual@.
gaugeChartConfiguration_tooltipOptions :: Lens.Lens' GaugeChartConfiguration (Prelude.Maybe TooltipOptions)
gaugeChartConfiguration_tooltipOptions = Lens.lens (\GaugeChartConfiguration' {tooltipOptions} -> tooltipOptions) (\s@GaugeChartConfiguration' {} a -> s {tooltipOptions = a} :: GaugeChartConfiguration)

-- | The visual palette configuration of a @GaugeChartVisual@.
gaugeChartConfiguration_visualPalette :: Lens.Lens' GaugeChartConfiguration (Prelude.Maybe VisualPalette)
gaugeChartConfiguration_visualPalette = Lens.lens (\GaugeChartConfiguration' {visualPalette} -> visualPalette) (\s@GaugeChartConfiguration' {} a -> s {visualPalette = a} :: GaugeChartConfiguration)

instance Data.FromJSON GaugeChartConfiguration where
  parseJSON =
    Data.withObject
      "GaugeChartConfiguration"
      ( \x ->
          GaugeChartConfiguration'
            Prelude.<$> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "GaugeChartOptions")
            Prelude.<*> (x Data..:? "TooltipOptions")
            Prelude.<*> (x Data..:? "VisualPalette")
      )

instance Prelude.Hashable GaugeChartConfiguration where
  hashWithSalt _salt GaugeChartConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` gaugeChartOptions
      `Prelude.hashWithSalt` tooltipOptions
      `Prelude.hashWithSalt` visualPalette

instance Prelude.NFData GaugeChartConfiguration where
  rnf GaugeChartConfiguration' {..} =
    Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf gaugeChartOptions
      `Prelude.seq` Prelude.rnf tooltipOptions
      `Prelude.seq` Prelude.rnf visualPalette

instance Data.ToJSON GaugeChartConfiguration where
  toJSON GaugeChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("GaugeChartOptions" Data..=)
              Prelude.<$> gaugeChartOptions,
            ("TooltipOptions" Data..=)
              Prelude.<$> tooltipOptions,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette
          ]
      )
