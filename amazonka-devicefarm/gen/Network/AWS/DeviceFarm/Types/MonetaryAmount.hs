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

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.CurrencyCode
import qualified Network.AWS.Lens as Lens

-- | A number that represents the monetary amount for an offering or
-- transaction.
--
-- /See:/ 'newMonetaryAmount' smart constructor.
data MonetaryAmount = MonetaryAmount'
  { -- | The numerical amount of an offering or transaction.
    amount :: Core.Maybe Core.Double,
    -- | The currency code of a monetary amount. For example, @USD@ means U.S.
    -- dollars.
    currencyCode :: Core.Maybe CurrencyCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { amount = Core.Nothing,
      currencyCode = Core.Nothing
    }

-- | The numerical amount of an offering or transaction.
monetaryAmount_amount :: Lens.Lens' MonetaryAmount (Core.Maybe Core.Double)
monetaryAmount_amount = Lens.lens (\MonetaryAmount' {amount} -> amount) (\s@MonetaryAmount' {} a -> s {amount = a} :: MonetaryAmount)

-- | The currency code of a monetary amount. For example, @USD@ means U.S.
-- dollars.
monetaryAmount_currencyCode :: Lens.Lens' MonetaryAmount (Core.Maybe CurrencyCode)
monetaryAmount_currencyCode = Lens.lens (\MonetaryAmount' {currencyCode} -> currencyCode) (\s@MonetaryAmount' {} a -> s {currencyCode = a} :: MonetaryAmount)

instance Core.FromJSON MonetaryAmount where
  parseJSON =
    Core.withObject
      "MonetaryAmount"
      ( \x ->
          MonetaryAmount'
            Core.<$> (x Core..:? "amount")
            Core.<*> (x Core..:? "currencyCode")
      )

instance Core.Hashable MonetaryAmount

instance Core.NFData MonetaryAmount
