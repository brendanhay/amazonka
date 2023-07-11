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
-- Module      : Amazonka.QuickSight.Types.PercentageDisplayFormatConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PercentageDisplayFormatConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DecimalPlacesConfiguration
import Amazonka.QuickSight.Types.NegativeValueConfiguration
import Amazonka.QuickSight.Types.NullValueFormatConfiguration
import Amazonka.QuickSight.Types.NumericSeparatorConfiguration

-- | The options that determine the percentage display format configuration.
--
-- /See:/ 'newPercentageDisplayFormatConfiguration' smart constructor.
data PercentageDisplayFormatConfiguration = PercentageDisplayFormatConfiguration'
  { -- | The option that determines the decimal places configuration.
    decimalPlacesConfiguration :: Prelude.Maybe DecimalPlacesConfiguration,
    -- | The options that determine the negative value configuration.
    negativeValueConfiguration :: Prelude.Maybe NegativeValueConfiguration,
    -- | The options that determine the null value format configuration.
    nullValueFormatConfiguration :: Prelude.Maybe NullValueFormatConfiguration,
    -- | Determines the prefix value of the percentage format.
    prefix :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The options that determine the numeric separator configuration.
    separatorConfiguration :: Prelude.Maybe NumericSeparatorConfiguration,
    -- | Determines the suffix value of the percentage format.
    suffix :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PercentageDisplayFormatConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalPlacesConfiguration', 'percentageDisplayFormatConfiguration_decimalPlacesConfiguration' - The option that determines the decimal places configuration.
--
-- 'negativeValueConfiguration', 'percentageDisplayFormatConfiguration_negativeValueConfiguration' - The options that determine the negative value configuration.
--
-- 'nullValueFormatConfiguration', 'percentageDisplayFormatConfiguration_nullValueFormatConfiguration' - The options that determine the null value format configuration.
--
-- 'prefix', 'percentageDisplayFormatConfiguration_prefix' - Determines the prefix value of the percentage format.
--
-- 'separatorConfiguration', 'percentageDisplayFormatConfiguration_separatorConfiguration' - The options that determine the numeric separator configuration.
--
-- 'suffix', 'percentageDisplayFormatConfiguration_suffix' - Determines the suffix value of the percentage format.
newPercentageDisplayFormatConfiguration ::
  PercentageDisplayFormatConfiguration
newPercentageDisplayFormatConfiguration =
  PercentageDisplayFormatConfiguration'
    { decimalPlacesConfiguration =
        Prelude.Nothing,
      negativeValueConfiguration =
        Prelude.Nothing,
      nullValueFormatConfiguration =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      separatorConfiguration =
        Prelude.Nothing,
      suffix = Prelude.Nothing
    }

-- | The option that determines the decimal places configuration.
percentageDisplayFormatConfiguration_decimalPlacesConfiguration :: Lens.Lens' PercentageDisplayFormatConfiguration (Prelude.Maybe DecimalPlacesConfiguration)
percentageDisplayFormatConfiguration_decimalPlacesConfiguration = Lens.lens (\PercentageDisplayFormatConfiguration' {decimalPlacesConfiguration} -> decimalPlacesConfiguration) (\s@PercentageDisplayFormatConfiguration' {} a -> s {decimalPlacesConfiguration = a} :: PercentageDisplayFormatConfiguration)

-- | The options that determine the negative value configuration.
percentageDisplayFormatConfiguration_negativeValueConfiguration :: Lens.Lens' PercentageDisplayFormatConfiguration (Prelude.Maybe NegativeValueConfiguration)
percentageDisplayFormatConfiguration_negativeValueConfiguration = Lens.lens (\PercentageDisplayFormatConfiguration' {negativeValueConfiguration} -> negativeValueConfiguration) (\s@PercentageDisplayFormatConfiguration' {} a -> s {negativeValueConfiguration = a} :: PercentageDisplayFormatConfiguration)

-- | The options that determine the null value format configuration.
percentageDisplayFormatConfiguration_nullValueFormatConfiguration :: Lens.Lens' PercentageDisplayFormatConfiguration (Prelude.Maybe NullValueFormatConfiguration)
percentageDisplayFormatConfiguration_nullValueFormatConfiguration = Lens.lens (\PercentageDisplayFormatConfiguration' {nullValueFormatConfiguration} -> nullValueFormatConfiguration) (\s@PercentageDisplayFormatConfiguration' {} a -> s {nullValueFormatConfiguration = a} :: PercentageDisplayFormatConfiguration)

-- | Determines the prefix value of the percentage format.
percentageDisplayFormatConfiguration_prefix :: Lens.Lens' PercentageDisplayFormatConfiguration (Prelude.Maybe Prelude.Text)
percentageDisplayFormatConfiguration_prefix = Lens.lens (\PercentageDisplayFormatConfiguration' {prefix} -> prefix) (\s@PercentageDisplayFormatConfiguration' {} a -> s {prefix = a} :: PercentageDisplayFormatConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The options that determine the numeric separator configuration.
percentageDisplayFormatConfiguration_separatorConfiguration :: Lens.Lens' PercentageDisplayFormatConfiguration (Prelude.Maybe NumericSeparatorConfiguration)
percentageDisplayFormatConfiguration_separatorConfiguration = Lens.lens (\PercentageDisplayFormatConfiguration' {separatorConfiguration} -> separatorConfiguration) (\s@PercentageDisplayFormatConfiguration' {} a -> s {separatorConfiguration = a} :: PercentageDisplayFormatConfiguration)

-- | Determines the suffix value of the percentage format.
percentageDisplayFormatConfiguration_suffix :: Lens.Lens' PercentageDisplayFormatConfiguration (Prelude.Maybe Prelude.Text)
percentageDisplayFormatConfiguration_suffix = Lens.lens (\PercentageDisplayFormatConfiguration' {suffix} -> suffix) (\s@PercentageDisplayFormatConfiguration' {} a -> s {suffix = a} :: PercentageDisplayFormatConfiguration) Prelude.. Lens.mapping Data._Sensitive

instance
  Data.FromJSON
    PercentageDisplayFormatConfiguration
  where
  parseJSON =
    Data.withObject
      "PercentageDisplayFormatConfiguration"
      ( \x ->
          PercentageDisplayFormatConfiguration'
            Prelude.<$> (x Data..:? "DecimalPlacesConfiguration")
            Prelude.<*> (x Data..:? "NegativeValueConfiguration")
            Prelude.<*> (x Data..:? "NullValueFormatConfiguration")
            Prelude.<*> (x Data..:? "Prefix")
            Prelude.<*> (x Data..:? "SeparatorConfiguration")
            Prelude.<*> (x Data..:? "Suffix")
      )

instance
  Prelude.Hashable
    PercentageDisplayFormatConfiguration
  where
  hashWithSalt
    _salt
    PercentageDisplayFormatConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` decimalPlacesConfiguration
        `Prelude.hashWithSalt` negativeValueConfiguration
        `Prelude.hashWithSalt` nullValueFormatConfiguration
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` separatorConfiguration
        `Prelude.hashWithSalt` suffix

instance
  Prelude.NFData
    PercentageDisplayFormatConfiguration
  where
  rnf PercentageDisplayFormatConfiguration' {..} =
    Prelude.rnf decimalPlacesConfiguration
      `Prelude.seq` Prelude.rnf negativeValueConfiguration
      `Prelude.seq` Prelude.rnf nullValueFormatConfiguration
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf separatorConfiguration
      `Prelude.seq` Prelude.rnf suffix

instance
  Data.ToJSON
    PercentageDisplayFormatConfiguration
  where
  toJSON PercentageDisplayFormatConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DecimalPlacesConfiguration" Data..=)
              Prelude.<$> decimalPlacesConfiguration,
            ("NegativeValueConfiguration" Data..=)
              Prelude.<$> negativeValueConfiguration,
            ("NullValueFormatConfiguration" Data..=)
              Prelude.<$> nullValueFormatConfiguration,
            ("Prefix" Data..=) Prelude.<$> prefix,
            ("SeparatorConfiguration" Data..=)
              Prelude.<$> separatorConfiguration,
            ("Suffix" Data..=) Prelude.<$> suffix
          ]
      )
