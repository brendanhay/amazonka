{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.Types.MonetaryAmount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.MonetaryAmount where

import Network.AWS.DeviceFarm.Types.CurrencyCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A number that represents the monetary amount for an offering or
-- transaction.
--
-- /See:/ 'newMonetaryAmount' smart constructor.
data MonetaryAmount = MonetaryAmount'
  { -- | The numerical amount of an offering or transaction.
    amount :: Prelude.Maybe Prelude.Double,
    -- | The currency code of a monetary amount. For example, @USD@ means U.S.
    -- dollars.
    currencyCode :: Prelude.Maybe CurrencyCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MonetaryAmount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'monetaryAmount_amount' - The numerical amount of an offering or transaction.
--
-- 'currencyCode', 'monetaryAmount_currencyCode' - The currency code of a monetary amount. For example, @USD@ means U.S.
-- dollars.
newMonetaryAmount ::
  MonetaryAmount
newMonetaryAmount =
  MonetaryAmount'
    { amount = Prelude.Nothing,
      currencyCode = Prelude.Nothing
    }

-- | The numerical amount of an offering or transaction.
monetaryAmount_amount :: Lens.Lens' MonetaryAmount (Prelude.Maybe Prelude.Double)
monetaryAmount_amount = Lens.lens (\MonetaryAmount' {amount} -> amount) (\s@MonetaryAmount' {} a -> s {amount = a} :: MonetaryAmount)

-- | The currency code of a monetary amount. For example, @USD@ means U.S.
-- dollars.
monetaryAmount_currencyCode :: Lens.Lens' MonetaryAmount (Prelude.Maybe CurrencyCode)
monetaryAmount_currencyCode = Lens.lens (\MonetaryAmount' {currencyCode} -> currencyCode) (\s@MonetaryAmount' {} a -> s {currencyCode = a} :: MonetaryAmount)

instance Prelude.FromJSON MonetaryAmount where
  parseJSON =
    Prelude.withObject
      "MonetaryAmount"
      ( \x ->
          MonetaryAmount'
            Prelude.<$> (x Prelude..:? "amount")
            Prelude.<*> (x Prelude..:? "currencyCode")
      )

instance Prelude.Hashable MonetaryAmount

instance Prelude.NFData MonetaryAmount
