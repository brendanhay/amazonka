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
-- Module      : Network.AWS.EC2.Types.Purchase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Purchase where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the result of the purchase.
--
-- /See:/ 'newPurchase' smart constructor.
data Purchase = Purchase'
  { -- | The instance family on the Dedicated Host that the reservation can be
    -- associated with.
    instanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the Dedicated Hosts associated with the reservation.
    hostIdSet :: Prelude.Maybe [Prelude.Text],
    -- | The upfront price of the reservation.
    upfrontPrice :: Prelude.Maybe Prelude.Text,
    -- | The payment option for the reservation.
    paymentOption :: Prelude.Maybe PaymentOption,
    -- | The duration of the reservation\'s term in seconds.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The ID of the reservation.
    hostReservationId :: Prelude.Maybe Prelude.Text,
    -- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
    -- specified. At this time, the only supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The hourly price of the reservation per hour.
    hourlyPrice :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Purchase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'purchase_instanceFamily' - The instance family on the Dedicated Host that the reservation can be
-- associated with.
--
-- 'hostIdSet', 'purchase_hostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
--
-- 'upfrontPrice', 'purchase_upfrontPrice' - The upfront price of the reservation.
--
-- 'paymentOption', 'purchase_paymentOption' - The payment option for the reservation.
--
-- 'duration', 'purchase_duration' - The duration of the reservation\'s term in seconds.
--
-- 'hostReservationId', 'purchase_hostReservationId' - The ID of the reservation.
--
-- 'currencyCode', 'purchase_currencyCode' - The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
--
-- 'hourlyPrice', 'purchase_hourlyPrice' - The hourly price of the reservation per hour.
newPurchase ::
  Purchase
newPurchase =
  Purchase'
    { instanceFamily = Prelude.Nothing,
      hostIdSet = Prelude.Nothing,
      upfrontPrice = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      duration = Prelude.Nothing,
      hostReservationId = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      hourlyPrice = Prelude.Nothing
    }

-- | The instance family on the Dedicated Host that the reservation can be
-- associated with.
purchase_instanceFamily :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_instanceFamily = Lens.lens (\Purchase' {instanceFamily} -> instanceFamily) (\s@Purchase' {} a -> s {instanceFamily = a} :: Purchase)

-- | The IDs of the Dedicated Hosts associated with the reservation.
purchase_hostIdSet :: Lens.Lens' Purchase (Prelude.Maybe [Prelude.Text])
purchase_hostIdSet = Lens.lens (\Purchase' {hostIdSet} -> hostIdSet) (\s@Purchase' {} a -> s {hostIdSet = a} :: Purchase) Prelude.. Lens.mapping Prelude._Coerce

-- | The upfront price of the reservation.
purchase_upfrontPrice :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_upfrontPrice = Lens.lens (\Purchase' {upfrontPrice} -> upfrontPrice) (\s@Purchase' {} a -> s {upfrontPrice = a} :: Purchase)

-- | The payment option for the reservation.
purchase_paymentOption :: Lens.Lens' Purchase (Prelude.Maybe PaymentOption)
purchase_paymentOption = Lens.lens (\Purchase' {paymentOption} -> paymentOption) (\s@Purchase' {} a -> s {paymentOption = a} :: Purchase)

-- | The duration of the reservation\'s term in seconds.
purchase_duration :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Int)
purchase_duration = Lens.lens (\Purchase' {duration} -> duration) (\s@Purchase' {} a -> s {duration = a} :: Purchase)

-- | The ID of the reservation.
purchase_hostReservationId :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_hostReservationId = Lens.lens (\Purchase' {hostReservationId} -> hostReservationId) (\s@Purchase' {} a -> s {hostReservationId = a} :: Purchase)

-- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
purchase_currencyCode :: Lens.Lens' Purchase (Prelude.Maybe CurrencyCodeValues)
purchase_currencyCode = Lens.lens (\Purchase' {currencyCode} -> currencyCode) (\s@Purchase' {} a -> s {currencyCode = a} :: Purchase)

-- | The hourly price of the reservation per hour.
purchase_hourlyPrice :: Lens.Lens' Purchase (Prelude.Maybe Prelude.Text)
purchase_hourlyPrice = Lens.lens (\Purchase' {hourlyPrice} -> hourlyPrice) (\s@Purchase' {} a -> s {hourlyPrice = a} :: Purchase)

instance Prelude.FromXML Purchase where
  parseXML x =
    Purchase'
      Prelude.<$> (x Prelude..@? "instanceFamily")
      Prelude.<*> ( x Prelude..@? "hostIdSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "upfrontPrice")
      Prelude.<*> (x Prelude..@? "paymentOption")
      Prelude.<*> (x Prelude..@? "duration")
      Prelude.<*> (x Prelude..@? "hostReservationId")
      Prelude.<*> (x Prelude..@? "currencyCode")
      Prelude.<*> (x Prelude..@? "hourlyPrice")

instance Prelude.Hashable Purchase

instance Prelude.NFData Purchase
