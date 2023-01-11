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
-- Module      : Amazonka.QuickSight.Types.NumberDisplayFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumberDisplayFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DecimalPlacesConfiguration
import Amazonka.QuickSight.Types.NegativeValueConfiguration
import Amazonka.QuickSight.Types.NullValueFormatConfiguration
import Amazonka.QuickSight.Types.NumberScale
import Amazonka.QuickSight.Types.NumericSeparatorConfiguration

-- | The options that determine the number display format configuration.
--
-- /See:/ 'newNumberDisplayFormatConfiguration' smart constructor.
data NumberDisplayFormatConfiguration = NumberDisplayFormatConfiguration'
  { -- | The option that determines the decimal places configuration.
    decimalPlacesConfiguration :: Prelude.Maybe DecimalPlacesConfiguration,
    -- | The options that determine the negative value configuration.
    negativeValueConfiguration :: Prelude.Maybe NegativeValueConfiguration,
    -- | The options that determine the null value format configuration.
    nullValueFormatConfiguration :: Prelude.Maybe NullValueFormatConfiguration,
    -- | Determines the number scale value of the number format.
    numberScale :: Prelude.Maybe NumberScale,
    -- | Determines the prefix value of the number format.
    prefix :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The options that determine the numeric separator configuration.
    separatorConfiguration :: Prelude.Maybe NumericSeparatorConfiguration,
    -- | Determines the suffix value of the number format.
    suffix :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberDisplayFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalPlacesConfiguration', 'numberDisplayFormatConfiguration_decimalPlacesConfiguration' - The option that determines the decimal places configuration.
--
-- 'negativeValueConfiguration', 'numberDisplayFormatConfiguration_negativeValueConfiguration' - The options that determine the negative value configuration.
--
-- 'nullValueFormatConfiguration', 'numberDisplayFormatConfiguration_nullValueFormatConfiguration' - The options that determine the null value format configuration.
--
-- 'numberScale', 'numberDisplayFormatConfiguration_numberScale' - Determines the number scale value of the number format.
--
-- 'prefix', 'numberDisplayFormatConfiguration_prefix' - Determines the prefix value of the number format.
--
-- 'separatorConfiguration', 'numberDisplayFormatConfiguration_separatorConfiguration' - The options that determine the numeric separator configuration.
--
-- 'suffix', 'numberDisplayFormatConfiguration_suffix' - Determines the suffix value of the number format.
newNumberDisplayFormatConfiguration ::
  NumberDisplayFormatConfiguration
newNumberDisplayFormatConfiguration =
  NumberDisplayFormatConfiguration'
    { decimalPlacesConfiguration =
        Prelude.Nothing,
      negativeValueConfiguration =
        Prelude.Nothing,
      nullValueFormatConfiguration =
        Prelude.Nothing,
      numberScale = Prelude.Nothing,
      prefix = Prelude.Nothing,
      separatorConfiguration = Prelude.Nothing,
      suffix = Prelude.Nothing
    }

-- | The option that determines the decimal places configuration.
numberDisplayFormatConfiguration_decimalPlacesConfiguration :: Lens.Lens' NumberDisplayFormatConfiguration (Prelude.Maybe DecimalPlacesConfiguration)
numberDisplayFormatConfiguration_decimalPlacesConfiguration = Lens.lens (\NumberDisplayFormatConfiguration' {decimalPlacesConfiguration} -> decimalPlacesConfiguration) (\s@NumberDisplayFormatConfiguration' {} a -> s {decimalPlacesConfiguration = a} :: NumberDisplayFormatConfiguration)

-- | The options that determine the negative value configuration.
numberDisplayFormatConfiguration_negativeValueConfiguration :: Lens.Lens' NumberDisplayFormatConfiguration (Prelude.Maybe NegativeValueConfiguration)
numberDisplayFormatConfiguration_negativeValueConfiguration = Lens.lens (\NumberDisplayFormatConfiguration' {negativeValueConfiguration} -> negativeValueConfiguration) (\s@NumberDisplayFormatConfiguration' {} a -> s {negativeValueConfiguration = a} :: NumberDisplayFormatConfiguration)

-- | The options that determine the null value format configuration.
numberDisplayFormatConfiguration_nullValueFormatConfiguration :: Lens.Lens' NumberDisplayFormatConfiguration (Prelude.Maybe NullValueFormatConfiguration)
numberDisplayFormatConfiguration_nullValueFormatConfiguration = Lens.lens (\NumberDisplayFormatConfiguration' {nullValueFormatConfiguration} -> nullValueFormatConfiguration) (\s@NumberDisplayFormatConfiguration' {} a -> s {nullValueFormatConfiguration = a} :: NumberDisplayFormatConfiguration)

-- | Determines the number scale value of the number format.
numberDisplayFormatConfiguration_numberScale :: Lens.Lens' NumberDisplayFormatConfiguration (Prelude.Maybe NumberScale)
numberDisplayFormatConfiguration_numberScale = Lens.lens (\NumberDisplayFormatConfiguration' {numberScale} -> numberScale) (\s@NumberDisplayFormatConfiguration' {} a -> s {numberScale = a} :: NumberDisplayFormatConfiguration)

-- | Determines the prefix value of the number format.
numberDisplayFormatConfiguration_prefix :: Lens.Lens' NumberDisplayFormatConfiguration (Prelude.Maybe Prelude.Text)
numberDisplayFormatConfiguration_prefix = Lens.lens (\NumberDisplayFormatConfiguration' {prefix} -> prefix) (\s@NumberDisplayFormatConfiguration' {} a -> s {prefix = a} :: NumberDisplayFormatConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The options that determine the numeric separator configuration.
numberDisplayFormatConfiguration_separatorConfiguration :: Lens.Lens' NumberDisplayFormatConfiguration (Prelude.Maybe NumericSeparatorConfiguration)
numberDisplayFormatConfiguration_separatorConfiguration = Lens.lens (\NumberDisplayFormatConfiguration' {separatorConfiguration} -> separatorConfiguration) (\s@NumberDisplayFormatConfiguration' {} a -> s {separatorConfiguration = a} :: NumberDisplayFormatConfiguration)

-- | Determines the suffix value of the number format.
numberDisplayFormatConfiguration_suffix :: Lens.Lens' NumberDisplayFormatConfiguration (Prelude.Maybe Prelude.Text)
numberDisplayFormatConfiguration_suffix = Lens.lens (\NumberDisplayFormatConfiguration' {suffix} -> suffix) (\s@NumberDisplayFormatConfiguration' {} a -> s {suffix = a} :: NumberDisplayFormatConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    NumberDisplayFormatConfiguration
  where
  parseJSON =
    Data.withObject
      "NumberDisplayFormatConfiguration"
      ( \x ->
          NumberDisplayFormatConfiguration'
            Prelude.<$> (x Data..:? "DecimalPlacesConfiguration")
            Prelude.<*> (x Data..:? "NegativeValueConfiguration")
            Prelude.<*> (x Data..:? "NullValueFormatConfiguration")
            Prelude.<*> (x Data..:? "NumberScale")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "SeparatorConfiguration")
            Prelude.<*> (x Data..:? "Suffix")
      )

instance
  Prelude.Hashable
    NumberDisplayFormatConfiguration
  where
  hashWithSalt
    _salt
    NumberDisplayFormatConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` decimalPlacesConfiguration
        `Prelude.hashWithSalt` negativeValueConfiguration
        `Prelude.hashWithSalt` nullValueFormatConfiguration
        `Prelude.hashWithSalt` numberScale
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` separatorConfiguration
        `Prelude.hashWithSalt` suffix

instance
  Prelude.NFData
    NumberDisplayFormatConfiguration
  where
  rnf NumberDisplayFormatConfiguration' {..} =
    Prelude.rnf decimalPlacesConfiguration
      `Prelude.seq` Prelude.rnf negativeValueConfiguration
      `Prelude.seq` Prelude.rnf nullValueFormatConfiguration
      `Prelude.seq` Prelude.rnf numberScale
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf separatorConfiguration
      `Prelude.seq` Prelude.rnf suffix

instance Data.ToJSON NumberDisplayFormatConfiguration where
  toJSON NumberDisplayFormatConfiguration' {..} =
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
            ("Suffix" Data..=) Prelude.<$> suffix
          ]
      )
