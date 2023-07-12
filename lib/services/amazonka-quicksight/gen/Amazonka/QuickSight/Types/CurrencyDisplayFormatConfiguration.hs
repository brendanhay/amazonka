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
-- Module      : Amazonka.QuickSight.Types.CurrencyDisplayFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CurrencyDisplayFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DecimalPlacesConfiguration
import Amazonka.QuickSight.Types.NegativeValueConfiguration
import Amazonka.QuickSight.Types.NullValueFormatConfiguration
import Amazonka.QuickSight.Types.NumberScale
import Amazonka.QuickSight.Types.NumericSeparatorConfiguration

-- | The options that determine the currency display format configuration.
--
-- /See:/ 'newCurrencyDisplayFormatConfiguration' smart constructor.
data CurrencyDisplayFormatConfiguration = CurrencyDisplayFormatConfiguration'
  { -- | The option that determines the decimal places configuration.
    decimalPlacesConfiguration :: Prelude.Maybe DecimalPlacesConfiguration,
    -- | The options that determine the negative value configuration.
    negativeValueConfiguration :: Prelude.Maybe NegativeValueConfiguration,
    -- | The options that determine the null value format configuration.
    nullValueFormatConfiguration :: Prelude.Maybe NullValueFormatConfiguration,
    -- | Determines the number scale value for the currency format.
    numberScale :: Prelude.Maybe NumberScale,
    -- | Determines the prefix value of the currency format.
    prefix :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The options that determine the numeric separator configuration.
    separatorConfiguration :: Prelude.Maybe NumericSeparatorConfiguration,
    -- | Determines the suffix value of the currency format.
    suffix :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Determines the symbol for the currency format.
    symbol :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CurrencyDisplayFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalPlacesConfiguration', 'currencyDisplayFormatConfiguration_decimalPlacesConfiguration' - The option that determines the decimal places configuration.
--
-- 'negativeValueConfiguration', 'currencyDisplayFormatConfiguration_negativeValueConfiguration' - The options that determine the negative value configuration.
--
-- 'nullValueFormatConfiguration', 'currencyDisplayFormatConfiguration_nullValueFormatConfiguration' - The options that determine the null value format configuration.
--
-- 'numberScale', 'currencyDisplayFormatConfiguration_numberScale' - Determines the number scale value for the currency format.
--
-- 'prefix', 'currencyDisplayFormatConfiguration_prefix' - Determines the prefix value of the currency format.
--
-- 'separatorConfiguration', 'currencyDisplayFormatConfiguration_separatorConfiguration' - The options that determine the numeric separator configuration.
--
-- 'suffix', 'currencyDisplayFormatConfiguration_suffix' - Determines the suffix value of the currency format.
--
-- 'symbol', 'currencyDisplayFormatConfiguration_symbol' - Determines the symbol for the currency format.
newCurrencyDisplayFormatConfiguration ::
  CurrencyDisplayFormatConfiguration
newCurrencyDisplayFormatConfiguration =
  CurrencyDisplayFormatConfiguration'
    { decimalPlacesConfiguration =
        Prelude.Nothing,
      negativeValueConfiguration =
        Prelude.Nothing,
      nullValueFormatConfiguration =
        Prelude.Nothing,
      numberScale = Prelude.Nothing,
      prefix = Prelude.Nothing,
      separatorConfiguration =
        Prelude.Nothing,
      suffix = Prelude.Nothing,
      symbol = Prelude.Nothing
    }

-- | The option that determines the decimal places configuration.
currencyDisplayFormatConfiguration_decimalPlacesConfiguration :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe DecimalPlacesConfiguration)
currencyDisplayFormatConfiguration_decimalPlacesConfiguration = Lens.lens (\CurrencyDisplayFormatConfiguration' {decimalPlacesConfiguration} -> decimalPlacesConfiguration) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {decimalPlacesConfiguration = a} :: CurrencyDisplayFormatConfiguration)

-- | The options that determine the negative value configuration.
currencyDisplayFormatConfiguration_negativeValueConfiguration :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe NegativeValueConfiguration)
currencyDisplayFormatConfiguration_negativeValueConfiguration = Lens.lens (\CurrencyDisplayFormatConfiguration' {negativeValueConfiguration} -> negativeValueConfiguration) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {negativeValueConfiguration = a} :: CurrencyDisplayFormatConfiguration)

-- | The options that determine the null value format configuration.
currencyDisplayFormatConfiguration_nullValueFormatConfiguration :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe NullValueFormatConfiguration)
currencyDisplayFormatConfiguration_nullValueFormatConfiguration = Lens.lens (\CurrencyDisplayFormatConfiguration' {nullValueFormatConfiguration} -> nullValueFormatConfiguration) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {nullValueFormatConfiguration = a} :: CurrencyDisplayFormatConfiguration)

-- | Determines the number scale value for the currency format.
currencyDisplayFormatConfiguration_numberScale :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe NumberScale)
currencyDisplayFormatConfiguration_numberScale = Lens.lens (\CurrencyDisplayFormatConfiguration' {numberScale} -> numberScale) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {numberScale = a} :: CurrencyDisplayFormatConfiguration)

-- | Determines the prefix value of the currency format.
currencyDisplayFormatConfiguration_prefix :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe Prelude.Text)
currencyDisplayFormatConfiguration_prefix = Lens.lens (\CurrencyDisplayFormatConfiguration' {prefix} -> prefix) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {prefix = a} :: CurrencyDisplayFormatConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The options that determine the numeric separator configuration.
currencyDisplayFormatConfiguration_separatorConfiguration :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe NumericSeparatorConfiguration)
currencyDisplayFormatConfiguration_separatorConfiguration = Lens.lens (\CurrencyDisplayFormatConfiguration' {separatorConfiguration} -> separatorConfiguration) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {separatorConfiguration = a} :: CurrencyDisplayFormatConfiguration)

-- | Determines the suffix value of the currency format.
currencyDisplayFormatConfiguration_suffix :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe Prelude.Text)
currencyDisplayFormatConfiguration_suffix = Lens.lens (\CurrencyDisplayFormatConfiguration' {suffix} -> suffix) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {suffix = a} :: CurrencyDisplayFormatConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | Determines the symbol for the currency format.
currencyDisplayFormatConfiguration_symbol :: Lens.Lens' CurrencyDisplayFormatConfiguration (Prelude.Maybe Prelude.Text)
currencyDisplayFormatConfiguration_symbol = Lens.lens (\CurrencyDisplayFormatConfiguration' {symbol} -> symbol) (\s@CurrencyDisplayFormatConfiguration' {} a -> s {symbol = a} :: CurrencyDisplayFormatConfiguration)

instance
  Data.FromJSON
    CurrencyDisplayFormatConfiguration
  where
  parseJSON =
    Data.withObject
      "CurrencyDisplayFormatConfiguration"
      ( \x ->
          CurrencyDisplayFormatConfiguration'
            Prelude.<$> (x Data..:? "DecimalPlacesConfiguration")
            Prelude.<*> (x Data..:? "NegativeValueConfiguration")
            Prelude.<*> (x Data..:? "NullValueFormatConfiguration")
            Prelude.<*> (x Data..:? "NumberScale")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "SeparatorConfiguration")
            Prelude.<*> (x Data..:? "Suffix")
            Prelude.<*> (x Data..:? "Symbol")
      )

instance
  Prelude.Hashable
    CurrencyDisplayFormatConfiguration
  where
  hashWithSalt
    _salt
    CurrencyDisplayFormatConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` decimalPlacesConfiguration
        `Prelude.hashWithSalt` negativeValueConfiguration
        `Prelude.hashWithSalt` nullValueFormatConfiguration
        `Prelude.hashWithSalt` numberScale
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` separatorConfiguration
        `Prelude.hashWithSalt` suffix
        `Prelude.hashWithSalt` symbol

instance
  Prelude.NFData
    CurrencyDisplayFormatConfiguration
  where
  rnf CurrencyDisplayFormatConfiguration' {..} =
    Prelude.rnf decimalPlacesConfiguration
      `Prelude.seq` Prelude.rnf negativeValueConfiguration
      `Prelude.seq` Prelude.rnf nullValueFormatConfiguration
      `Prelude.seq` Prelude.rnf numberScale
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf separatorConfiguration
      `Prelude.seq` Prelude.rnf suffix
      `Prelude.seq` Prelude.rnf symbol

instance
  Data.ToJSON
    CurrencyDisplayFormatConfiguration
  where
  toJSON CurrencyDisplayFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DecimalPlacesConfiguration" Data..=)
              Prelude.<$> decimalPlacesConfiguration,
            ("NegativeValueConfiguration" Data..=)
              Prelude.<$> negativeValueConfiguration,
            ("NullValueFormatConfiguration" Data..=)
              Prelude.<$> nullValueFormatConfiguration,
            ("NumberScale" Data..=) Prelude.<$> numberScale,
            ("Prefix" Data..=) Prelude.<$> prefix,
            ("SeparatorConfiguration" Data..=)
              Prelude.<$> separatorConfiguration,
            ("Suffix" Data..=) Prelude.<$> suffix,
            ("Symbol" Data..=) Prelude.<$> symbol
          ]
      )
