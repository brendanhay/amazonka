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
-- Module      : Amazonka.QuickSight.Types.ThousandSeparatorOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThousandSeparatorOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NumericSeparatorSymbol
import Amazonka.QuickSight.Types.Visibility

-- | The options that determine the thousands separator configuration.
--
-- /See:/ 'newThousandSeparatorOptions' smart constructor.
data ThousandSeparatorOptions = ThousandSeparatorOptions'
  { -- | Determines the thousands separator symbol.
    symbol :: Prelude.Maybe NumericSeparatorSymbol,
    -- | Determines the visibility of the thousands separator.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThousandSeparatorOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'symbol', 'thousandSeparatorOptions_symbol' - Determines the thousands separator symbol.
--
-- 'visibility', 'thousandSeparatorOptions_visibility' - Determines the visibility of the thousands separator.
newThousandSeparatorOptions ::
  ThousandSeparatorOptions
newThousandSeparatorOptions =
  ThousandSeparatorOptions'
    { symbol = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | Determines the thousands separator symbol.
thousandSeparatorOptions_symbol :: Lens.Lens' ThousandSeparatorOptions (Prelude.Maybe NumericSeparatorSymbol)
thousandSeparatorOptions_symbol = Lens.lens (\ThousandSeparatorOptions' {symbol} -> symbol) (\s@ThousandSeparatorOptions' {} a -> s {symbol = a} :: ThousandSeparatorOptions)

-- | Determines the visibility of the thousands separator.
thousandSeparatorOptions_visibility :: Lens.Lens' ThousandSeparatorOptions (Prelude.Maybe Visibility)
thousandSeparatorOptions_visibility = Lens.lens (\ThousandSeparatorOptions' {visibility} -> visibility) (\s@ThousandSeparatorOptions' {} a -> s {visibility = a} :: ThousandSeparatorOptions)

instance Data.FromJSON ThousandSeparatorOptions where
  parseJSON =
    Data.withObject
      "ThousandSeparatorOptions"
      ( \x ->
          ThousandSeparatorOptions'
            Prelude.<$> (x Data..:? "Symbol")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable ThousandSeparatorOptions where
  hashWithSalt _salt ThousandSeparatorOptions' {..} =
    _salt
      `Prelude.hashWithSalt` symbol
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData ThousandSeparatorOptions where
  rnf ThousandSeparatorOptions' {..} =
    Prelude.rnf symbol `Prelude.seq`
      Prelude.rnf visibility

instance Data.ToJSON ThousandSeparatorOptions where
  toJSON ThousandSeparatorOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Symbol" Data..=) Prelude.<$> symbol,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
