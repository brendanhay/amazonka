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
-- Module      : Amazonka.EC2.Types.ReservedInstanceLimitPrice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstanceLimitPrice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CurrencyCodeValues
import qualified Amazonka.Prelude as Prelude

-- | Describes the limit price of a Reserved Instance offering.
--
-- /See:/ 'newReservedInstanceLimitPrice' smart constructor.
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice'
  { -- | Used for Reserved Instance Marketplace offerings. Specifies the limit
    -- price on the total order (instanceCount * price).
    amount :: Prelude.Maybe Prelude.Double,
    -- | The currency in which the @limitPrice@ amount is specified. At this
    -- time, the only supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { amount =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing
    }

-- | Used for Reserved Instance Marketplace offerings. Specifies the limit
-- price on the total order (instanceCount * price).
reservedInstanceLimitPrice_amount :: Lens.Lens' ReservedInstanceLimitPrice (Prelude.Maybe Prelude.Double)
reservedInstanceLimitPrice_amount = Lens.lens (\ReservedInstanceLimitPrice' {amount} -> amount) (\s@ReservedInstanceLimitPrice' {} a -> s {amount = a} :: ReservedInstanceLimitPrice)

-- | The currency in which the @limitPrice@ amount is specified. At this
-- time, the only supported currency is @USD@.
reservedInstanceLimitPrice_currencyCode :: Lens.Lens' ReservedInstanceLimitPrice (Prelude.Maybe CurrencyCodeValues)
reservedInstanceLimitPrice_currencyCode = Lens.lens (\ReservedInstanceLimitPrice' {currencyCode} -> currencyCode) (\s@ReservedInstanceLimitPrice' {} a -> s {currencyCode = a} :: ReservedInstanceLimitPrice)

instance Prelude.Hashable ReservedInstanceLimitPrice where
  hashWithSalt _salt ReservedInstanceLimitPrice' {..} =
    _salt `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` currencyCode

instance Prelude.NFData ReservedInstanceLimitPrice where
  rnf ReservedInstanceLimitPrice' {..} =
    Prelude.rnf amount
      `Prelude.seq` Prelude.rnf currencyCode

instance Data.ToQuery ReservedInstanceLimitPrice where
  toQuery ReservedInstanceLimitPrice' {..} =
    Prelude.mconcat
      [ "Amount" Data.=: amount,
        "CurrencyCode" Data.=: currencyCode
      ]
