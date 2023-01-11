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
-- Module      : Amazonka.QuickSight.Types.NumericSeparatorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericSeparatorConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NumericSeparatorSymbol
import Amazonka.QuickSight.Types.ThousandSeparatorOptions

-- | The options that determine the numeric separator configuration.
--
-- /See:/ 'newNumericSeparatorConfiguration' smart constructor.
data NumericSeparatorConfiguration = NumericSeparatorConfiguration'
  { -- | Determines the decimal separator.
    decimalSeparator :: Prelude.Maybe NumericSeparatorSymbol,
    -- | The options that determine the thousands separator configuration.
    thousandsSeparator :: Prelude.Maybe ThousandSeparatorOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericSeparatorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decimalSeparator', 'numericSeparatorConfiguration_decimalSeparator' - Determines the decimal separator.
--
-- 'thousandsSeparator', 'numericSeparatorConfiguration_thousandsSeparator' - The options that determine the thousands separator configuration.
newNumericSeparatorConfiguration ::
  NumericSeparatorConfiguration
newNumericSeparatorConfiguration =
  NumericSeparatorConfiguration'
    { decimalSeparator =
        Prelude.Nothing,
      thousandsSeparator = Prelude.Nothing
    }

-- | Determines the decimal separator.
numericSeparatorConfiguration_decimalSeparator :: Lens.Lens' NumericSeparatorConfiguration (Prelude.Maybe NumericSeparatorSymbol)
numericSeparatorConfiguration_decimalSeparator = Lens.lens (\NumericSeparatorConfiguration' {decimalSeparator} -> decimalSeparator) (\s@NumericSeparatorConfiguration' {} a -> s {decimalSeparator = a} :: NumericSeparatorConfiguration)

-- | The options that determine the thousands separator configuration.
numericSeparatorConfiguration_thousandsSeparator :: Lens.Lens' NumericSeparatorConfiguration (Prelude.Maybe ThousandSeparatorOptions)
numericSeparatorConfiguration_thousandsSeparator = Lens.lens (\NumericSeparatorConfiguration' {thousandsSeparator} -> thousandsSeparator) (\s@NumericSeparatorConfiguration' {} a -> s {thousandsSeparator = a} :: NumericSeparatorConfiguration)

instance Data.FromJSON NumericSeparatorConfiguration where
  parseJSON =
    Data.withObject
      "NumericSeparatorConfiguration"
      ( \x ->
          NumericSeparatorConfiguration'
            Prelude.<$> (x Data..:? "DecimalSeparator")
            Prelude.<*> (x Data..:? "ThousandsSeparator")
      )

instance
  Prelude.Hashable
    NumericSeparatorConfiguration
  where
  hashWithSalt _salt NumericSeparatorConfiguration' {..} =
    _salt `Prelude.hashWithSalt` decimalSeparator
      `Prelude.hashWithSalt` thousandsSeparator

instance Prelude.NFData NumericSeparatorConfiguration where
  rnf NumericSeparatorConfiguration' {..} =
    Prelude.rnf decimalSeparator
      `Prelude.seq` Prelude.rnf thousandsSeparator

instance Data.ToJSON NumericSeparatorConfiguration where
  toJSON NumericSeparatorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DecimalSeparator" Data..=)
              Prelude.<$> decimalSeparator,
            ("ThousandsSeparator" Data..=)
              Prelude.<$> thousandsSeparator
          ]
      )
