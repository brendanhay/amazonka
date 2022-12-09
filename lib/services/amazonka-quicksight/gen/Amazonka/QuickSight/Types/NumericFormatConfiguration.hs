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
-- Module      : Amazonka.QuickSight.Types.NumericFormatConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CurrencyDisplayFormatConfiguration
import Amazonka.QuickSight.Types.NumberDisplayFormatConfiguration
import Amazonka.QuickSight.Types.PercentageDisplayFormatConfiguration

-- | The options that determine the numeric format configuration.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newNumericFormatConfiguration' smart constructor.
data NumericFormatConfiguration = NumericFormatConfiguration'
  { -- | The options that determine the currency display format configuration.
    currencyDisplayFormatConfiguration :: Prelude.Maybe CurrencyDisplayFormatConfiguration,
    -- | The options that determine the number display format configuration.
    numberDisplayFormatConfiguration :: Prelude.Maybe NumberDisplayFormatConfiguration,
    -- | The options that determine the percentage display format configuration.
    percentageDisplayFormatConfiguration :: Prelude.Maybe PercentageDisplayFormatConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyDisplayFormatConfiguration', 'numericFormatConfiguration_currencyDisplayFormatConfiguration' - The options that determine the currency display format configuration.
--
-- 'numberDisplayFormatConfiguration', 'numericFormatConfiguration_numberDisplayFormatConfiguration' - The options that determine the number display format configuration.
--
-- 'percentageDisplayFormatConfiguration', 'numericFormatConfiguration_percentageDisplayFormatConfiguration' - The options that determine the percentage display format configuration.
newNumericFormatConfiguration ::
  NumericFormatConfiguration
newNumericFormatConfiguration =
  NumericFormatConfiguration'
    { currencyDisplayFormatConfiguration =
        Prelude.Nothing,
      numberDisplayFormatConfiguration =
        Prelude.Nothing,
      percentageDisplayFormatConfiguration =
        Prelude.Nothing
    }

-- | The options that determine the currency display format configuration.
numericFormatConfiguration_currencyDisplayFormatConfiguration :: Lens.Lens' NumericFormatConfiguration (Prelude.Maybe CurrencyDisplayFormatConfiguration)
numericFormatConfiguration_currencyDisplayFormatConfiguration = Lens.lens (\NumericFormatConfiguration' {currencyDisplayFormatConfiguration} -> currencyDisplayFormatConfiguration) (\s@NumericFormatConfiguration' {} a -> s {currencyDisplayFormatConfiguration = a} :: NumericFormatConfiguration)

-- | The options that determine the number display format configuration.
numericFormatConfiguration_numberDisplayFormatConfiguration :: Lens.Lens' NumericFormatConfiguration (Prelude.Maybe NumberDisplayFormatConfiguration)
numericFormatConfiguration_numberDisplayFormatConfiguration = Lens.lens (\NumericFormatConfiguration' {numberDisplayFormatConfiguration} -> numberDisplayFormatConfiguration) (\s@NumericFormatConfiguration' {} a -> s {numberDisplayFormatConfiguration = a} :: NumericFormatConfiguration)

-- | The options that determine the percentage display format configuration.
numericFormatConfiguration_percentageDisplayFormatConfiguration :: Lens.Lens' NumericFormatConfiguration (Prelude.Maybe PercentageDisplayFormatConfiguration)
numericFormatConfiguration_percentageDisplayFormatConfiguration = Lens.lens (\NumericFormatConfiguration' {percentageDisplayFormatConfiguration} -> percentageDisplayFormatConfiguration) (\s@NumericFormatConfiguration' {} a -> s {percentageDisplayFormatConfiguration = a} :: NumericFormatConfiguration)

instance Data.FromJSON NumericFormatConfiguration where
  parseJSON =
    Data.withObject
      "NumericFormatConfiguration"
      ( \x ->
          NumericFormatConfiguration'
            Prelude.<$> (x Data..:? "CurrencyDisplayFormatConfiguration")
            Prelude.<*> (x Data..:? "NumberDisplayFormatConfiguration")
            Prelude.<*> (x Data..:? "PercentageDisplayFormatConfiguration")
      )

instance Prelude.Hashable NumericFormatConfiguration where
  hashWithSalt _salt NumericFormatConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` currencyDisplayFormatConfiguration
      `Prelude.hashWithSalt` numberDisplayFormatConfiguration
      `Prelude.hashWithSalt` percentageDisplayFormatConfiguration

instance Prelude.NFData NumericFormatConfiguration where
  rnf NumericFormatConfiguration' {..} =
    Prelude.rnf currencyDisplayFormatConfiguration
      `Prelude.seq` Prelude.rnf numberDisplayFormatConfiguration
      `Prelude.seq` Prelude.rnf percentageDisplayFormatConfiguration

instance Data.ToJSON NumericFormatConfiguration where
  toJSON NumericFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CurrencyDisplayFormatConfiguration" Data..=)
              Prelude.<$> currencyDisplayFormatConfiguration,
            ("NumberDisplayFormatConfiguration" Data..=)
              Prelude.<$> numberDisplayFormatConfiguration,
            ("PercentageDisplayFormatConfiguration" Data..=)
              Prelude.<$> percentageDisplayFormatConfiguration
          ]
      )
