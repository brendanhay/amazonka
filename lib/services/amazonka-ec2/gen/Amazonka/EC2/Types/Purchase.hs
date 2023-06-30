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
-- Module      : Amazonka.EC2.Types.Purchase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Purchase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CurrencyCodeValues
import Amazonka.EC2.Types.PaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Describes the result of the purchase.
--
-- /See:/ 'newPurchase' smart constructor.
data Purchase = Purchase'
  { -- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
    -- specified. At this time, the only supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The duration of the reservation\'s term in seconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The IDs of the Dedicated Hosts associated with the reservation.
    hostIdSet :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the reservation.
    hostReservationId :: Prelude.Maybe Prelude.Text,
    -- | The hourly price of the reservation per hour.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The instance family on the Dedicated Host that the reservation can be
    -- associated with.
    instanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The payment option for the reservation.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | The upfront price of the reservation.
    upfrontPrice :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Purchase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currencyCode', 'purchase_currencyCode' - The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
--
-- 'duration', 'purchase_duration' - The duration of the reservation\'s term in seconds.
--
-- 'hostIdSet', 'purchase_hostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
--
-- 'hostReservationId', 'purchase_hostReservationId' - The ID of the reservation.
--
-- 'hourlyPrice', 'purchase_hourlyPrice' - The hourly price of the reservation per hour.
--
-- 'instanceFamily', 'purchase_instanceFamily' - The instance family on the Dedicated Host that the reservation can be
-- associated with.
--
-- 'paymentOption', 'purchase_paymentOption' - The payment option for the reservation.
--
-- 'upfrontPrice', 'purchase_upfrontPrice' - The upfront price of the reservation.
newPurchase ::
  Purchase
newPurchase =
  Purchase'
    { currencyCode = Prelude.Nothing,
      duration = Prelude.Nothing,
      hostIdSet = Prelude.Nothing,
      hostReservationId = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      instanceFamily = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      upfrontPrice = Prelude.Nothing
    }

-- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
purchase_currencyCode :: Lens.Lens' Purchase (Prelude.Maybe CurrencyCodeValues)
purchase_currencyCode = Lens.lens (\Purchase' {currencyCode} -> currencyCode) (\s@Purchase' {} a -> s {currencyCode = a} :: Purchase)

-- | The duration of the reservation\'s term in seconds.
purchase_duration :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Int)
purchase_duration = Lens.lens (\Purchase' {duration} -> duration) (\s@Purchase' {} a -> s {duration = a} :: Purchase)

-- | The IDs of the Dedicated Hosts associated with the reservation.
purchase_hostIdSet :: Lens.Lens' Purchase (Prelude.Maybe [Prelude.Text])
purchase_hostIdSet = Lens.lens (\Purchase' {hostIdSet} -> hostIdSet) (\s@Purchase' {} a -> s {hostIdSet = a} :: Purchase) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the reservation.
purchase_hostReservationId :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_hostReservationId = Lens.lens (\Purchase' {hostReservationId} -> hostReservationId) (\s@Purchase' {} a -> s {hostReservationId = a} :: Purchase)

-- | The hourly price of the reservation per hour.
purchase_hourlyPrice :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_hourlyPrice = Lens.lens (\Purchase' {hourlyPrice} -> hourlyPrice) (\s@Purchase' {} a -> s {hourlyPrice = a} :: Purchase)

-- | The instance family on the Dedicated Host that the reservation can be
-- associated with.
purchase_instanceFamily :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_instanceFamily = Lens.lens (\Purchase' {instanceFamily} -> instanceFamily) (\s@Purchase' {} a -> s {instanceFamily = a} :: Purchase)

-- | The payment option for the reservation.
purchase_paymentOption :: Lens.Lens' Purchase (Prelude.Maybe PaymentOption)
purchase_paymentOption = Lens.lens (\Purchase' {paymentOption} -> paymentOption) (\s@Purchase' {} a -> s {paymentOption = a} :: Purchase)

-- | The upfront price of the reservation.
purchase_upfrontPrice :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_upfrontPrice = Lens.lens (\Purchase' {upfrontPrice} -> upfrontPrice) (\s@Purchase' {} a -> s {upfrontPrice = a} :: Purchase)

instance Data.FromXML Purchase where
  parseXML x =
    Purchase'
      Prelude.<$> (x Data..@? "currencyCode")
      Prelude.<*> (x Data..@? "duration")
      Prelude.<*> ( x
                      Data..@? "hostIdSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "hostReservationId")
      Prelude.<*> (x Data..@? "hourlyPrice")
      Prelude.<*> (x Data..@? "instanceFamily")
      Prelude.<*> (x Data..@? "paymentOption")
      Prelude.<*> (x Data..@? "upfrontPrice")

instance Prelude.Hashable Purchase where
  hashWithSalt _salt Purchase' {..} =
    _salt
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` hostIdSet
      `Prelude.hashWithSalt` hostReservationId
      `Prelude.hashWithSalt` hourlyPrice
      `Prelude.hashWithSalt` instanceFamily
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` upfrontPrice

instance Prelude.NFData Purchase where
  rnf Purchase' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf hostIdSet
      `Prelude.seq` Prelude.rnf hostReservationId
      `Prelude.seq` Prelude.rnf hourlyPrice
      `Prelude.seq` Prelude.rnf instanceFamily
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf upfrontPrice
