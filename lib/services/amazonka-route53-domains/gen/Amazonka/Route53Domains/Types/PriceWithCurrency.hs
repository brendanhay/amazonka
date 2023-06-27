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
-- Module      : Amazonka.Route53Domains.Types.PriceWithCurrency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.PriceWithCurrency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Currency-specific price information.
--
-- /See:/ 'newPriceWithCurrency' smart constructor.
data PriceWithCurrency = PriceWithCurrency'
  { -- | The price of a domain, in a specific currency.
    price :: Prelude.Double,
    -- | The currency specifier.
    currency :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PriceWithCurrency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'price', 'priceWithCurrency_price' - The price of a domain, in a specific currency.
--
-- 'currency', 'priceWithCurrency_currency' - The currency specifier.
newPriceWithCurrency ::
  -- | 'price'
  Prelude.Double ->
  -- | 'currency'
  Prelude.Text ->
  PriceWithCurrency
newPriceWithCurrency pPrice_ pCurrency_ =
  PriceWithCurrency'
    { price = pPrice_,
      currency = pCurrency_
    }

-- | The price of a domain, in a specific currency.
priceWithCurrency_price :: Lens.Lens' PriceWithCurrency Prelude.Double
priceWithCurrency_price = Lens.lens (\PriceWithCurrency' {price} -> price) (\s@PriceWithCurrency' {} a -> s {price = a} :: PriceWithCurrency)

-- | The currency specifier.
priceWithCurrency_currency :: Lens.Lens' PriceWithCurrency Prelude.Text
priceWithCurrency_currency = Lens.lens (\PriceWithCurrency' {currency} -> currency) (\s@PriceWithCurrency' {} a -> s {currency = a} :: PriceWithCurrency)

instance Data.FromJSON PriceWithCurrency where
  parseJSON =
    Data.withObject
      "PriceWithCurrency"
      ( \x ->
          PriceWithCurrency'
            Prelude.<$> (x Data..: "Price")
            Prelude.<*> (x Data..: "Currency")
      )

instance Prelude.Hashable PriceWithCurrency where
  hashWithSalt _salt PriceWithCurrency' {..} =
    _salt
      `Prelude.hashWithSalt` price
      `Prelude.hashWithSalt` currency

instance Prelude.NFData PriceWithCurrency where
  rnf PriceWithCurrency' {..} =
    Prelude.rnf price
      `Prelude.seq` Prelude.rnf currency
