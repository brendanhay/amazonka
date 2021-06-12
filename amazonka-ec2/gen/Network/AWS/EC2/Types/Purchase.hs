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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import qualified Network.AWS.Lens as Lens

-- | Describes the result of the purchase.
--
-- /See:/ 'newPurchase' smart constructor.
data Purchase = Purchase'
  { -- | The instance family on the Dedicated Host that the reservation can be
    -- associated with.
    instanceFamily :: Core.Maybe Core.Text,
    -- | The IDs of the Dedicated Hosts associated with the reservation.
    hostIdSet :: Core.Maybe [Core.Text],
    -- | The upfront price of the reservation.
    upfrontPrice :: Core.Maybe Core.Text,
    -- | The payment option for the reservation.
    paymentOption :: Core.Maybe PaymentOption,
    -- | The duration of the reservation\'s term in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The ID of the reservation.
    hostReservationId :: Core.Maybe Core.Text,
    -- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
    -- specified. At this time, the only supported currency is @USD@.
    currencyCode :: Core.Maybe CurrencyCodeValues,
    -- | The hourly price of the reservation per hour.
    hourlyPrice :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { instanceFamily = Core.Nothing,
      hostIdSet = Core.Nothing,
      upfrontPrice = Core.Nothing,
      paymentOption = Core.Nothing,
      duration = Core.Nothing,
      hostReservationId = Core.Nothing,
      currencyCode = Core.Nothing,
      hourlyPrice = Core.Nothing
    }

-- | The instance family on the Dedicated Host that the reservation can be
-- associated with.
purchase_instanceFamily :: Lens.Lens' Purchase (Core.Maybe Core.Text)
purchase_instanceFamily = Lens.lens (\Purchase' {instanceFamily} -> instanceFamily) (\s@Purchase' {} a -> s {instanceFamily = a} :: Purchase)

-- | The IDs of the Dedicated Hosts associated with the reservation.
purchase_hostIdSet :: Lens.Lens' Purchase (Core.Maybe [Core.Text])
purchase_hostIdSet = Lens.lens (\Purchase' {hostIdSet} -> hostIdSet) (\s@Purchase' {} a -> s {hostIdSet = a} :: Purchase) Core.. Lens.mapping Lens._Coerce

-- | The upfront price of the reservation.
purchase_upfrontPrice :: Lens.Lens' Purchase (Core.Maybe Core.Text)
purchase_upfrontPrice = Lens.lens (\Purchase' {upfrontPrice} -> upfrontPrice) (\s@Purchase' {} a -> s {upfrontPrice = a} :: Purchase)

-- | The payment option for the reservation.
purchase_paymentOption :: Lens.Lens' Purchase (Core.Maybe PaymentOption)
purchase_paymentOption = Lens.lens (\Purchase' {paymentOption} -> paymentOption) (\s@Purchase' {} a -> s {paymentOption = a} :: Purchase)

-- | The duration of the reservation\'s term in seconds.
purchase_duration :: Lens.Lens' Purchase (Core.Maybe Core.Int)
purchase_duration = Lens.lens (\Purchase' {duration} -> duration) (\s@Purchase' {} a -> s {duration = a} :: Purchase)

-- | The ID of the reservation.
purchase_hostReservationId :: Lens.Lens' Purchase (Core.Maybe Core.Text)
purchase_hostReservationId = Lens.lens (\Purchase' {hostReservationId} -> hostReservationId) (\s@Purchase' {} a -> s {hostReservationId = a} :: Purchase)

-- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are
-- specified. At this time, the only supported currency is @USD@.
purchase_currencyCode :: Lens.Lens' Purchase (Core.Maybe CurrencyCodeValues)
purchase_currencyCode = Lens.lens (\Purchase' {currencyCode} -> currencyCode) (\s@Purchase' {} a -> s {currencyCode = a} :: Purchase)

-- | The hourly price of the reservation per hour.
purchase_hourlyPrice :: Lens.Lens' Purchase (Core.Maybe Core.Text)
purchase_hourlyPrice = Lens.lens (\Purchase' {hourlyPrice} -> hourlyPrice) (\s@Purchase' {} a -> s {hourlyPrice = a} :: Purchase)

instance Core.FromXML Purchase where
  parseXML x =
    Purchase'
      Core.<$> (x Core..@? "instanceFamily")
      Core.<*> ( x Core..@? "hostIdSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "upfrontPrice")
      Core.<*> (x Core..@? "paymentOption")
      Core.<*> (x Core..@? "duration")
      Core.<*> (x Core..@? "hostReservationId")
      Core.<*> (x Core..@? "currencyCode")
      Core.<*> (x Core..@? "hourlyPrice")

instance Core.Hashable Purchase

instance Core.NFData Purchase
