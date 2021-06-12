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
-- Module      : Network.AWS.EC2.Types.ReservedInstanceLimitPrice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceLimitPrice where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import qualified Network.AWS.Lens as Lens

-- | Describes the limit price of a Reserved Instance offering.
--
-- /See:/ 'newReservedInstanceLimitPrice' smart constructor.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice'
  { -- | Used for Reserved Instance Marketplace offerings. Specifies the limit
    -- price on the total order (instanceCount * price).
    amount :: Core.Maybe Core.Double,
    -- | The currency in which the @limitPrice@ amount is specified. At this
    -- time, the only supported currency is @USD@.
    currencyCode :: Core.Maybe CurrencyCodeValues
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedInstanceLimitPrice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'reservedInstanceLimitPrice_amount' - Used for Reserved Instance Marketplace offerings. Specifies the limit
-- price on the total order (instanceCount * price).
--
-- 'currencyCode', 'reservedInstanceLimitPrice_currencyCode' - The currency in which the @limitPrice@ amount is specified. At this
-- time, the only supported currency is @USD@.
newReservedInstanceLimitPrice ::
  ReservedInstanceLimitPrice
newReservedInstanceLimitPrice =
  ReservedInstanceLimitPrice'
    { amount = Core.Nothing,
      currencyCode = Core.Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit
-- price on the total order (instanceCount * price).
reservedInstanceLimitPrice_amount :: Lens.Lens' ReservedInstanceLimitPrice (Core.Maybe Core.Double)
reservedInstanceLimitPrice_amount = Lens.lens (\ReservedInstanceLimitPrice' {amount} -> amount) (\s@ReservedInstanceLimitPrice' {} a -> s {amount = a} :: ReservedInstanceLimitPrice)

-- | The currency in which the @limitPrice@ amount is specified. At this
-- time, the only supported currency is @USD@.
reservedInstanceLimitPrice_currencyCode :: Lens.Lens' ReservedInstanceLimitPrice (Core.Maybe CurrencyCodeValues)
reservedInstanceLimitPrice_currencyCode = Lens.lens (\ReservedInstanceLimitPrice' {currencyCode} -> currencyCode) (\s@ReservedInstanceLimitPrice' {} a -> s {currencyCode = a} :: ReservedInstanceLimitPrice)

instance Core.Hashable ReservedInstanceLimitPrice

instance Core.NFData ReservedInstanceLimitPrice

instance Core.ToQuery ReservedInstanceLimitPrice where
  toQuery ReservedInstanceLimitPrice' {..} =
    Core.mconcat
      [ "Amount" Core.=: amount,
        "CurrencyCode" Core.=: currencyCode
      ]
