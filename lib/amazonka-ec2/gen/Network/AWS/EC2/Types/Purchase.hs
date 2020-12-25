{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Purchase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Purchase
  ( Purchase (..),

    -- * Smart constructor
    mkPurchase,

    -- * Lenses
    pCurrencyCode,
    pDuration,
    pHostIdSet,
    pHostReservationId,
    pHourlyPrice,
    pInstanceFamily,
    pPaymentOption,
    pUpfrontPrice,
  )
where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.EC2.Types.PaymentOption as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the result of the purchase.
--
-- /See:/ 'mkPurchase' smart constructor.
data Purchase = Purchase'
  { -- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Core.Maybe Types.CurrencyCodeValues,
    -- | The duration of the reservation's term in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The IDs of the Dedicated Hosts associated with the reservation.
    hostIdSet :: Core.Maybe [Types.String],
    -- | The ID of the reservation.
    hostReservationId :: Core.Maybe Types.String,
    -- | The hourly price of the reservation per hour.
    hourlyPrice :: Core.Maybe Types.String,
    -- | The instance family on the Dedicated Host that the reservation can be associated with.
    instanceFamily :: Core.Maybe Types.String,
    -- | The payment option for the reservation.
    paymentOption :: Core.Maybe Types.PaymentOption,
    -- | The upfront price of the reservation.
    upfrontPrice :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Purchase' value with any optional fields omitted.
mkPurchase ::
  Purchase
mkPurchase =
  Purchase'
    { currencyCode = Core.Nothing,
      duration = Core.Nothing,
      hostIdSet = Core.Nothing,
      hostReservationId = Core.Nothing,
      hourlyPrice = Core.Nothing,
      instanceFamily = Core.Nothing,
      paymentOption = Core.Nothing,
      upfrontPrice = Core.Nothing
    }

-- | The currency in which the @UpfrontPrice@ and @HourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCurrencyCode :: Lens.Lens' Purchase (Core.Maybe Types.CurrencyCodeValues)
pCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED pCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The duration of the reservation's term in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDuration :: Lens.Lens' Purchase (Core.Maybe Core.Int)
pDuration = Lens.field @"duration"
{-# DEPRECATED pDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The IDs of the Dedicated Hosts associated with the reservation.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostIdSet :: Lens.Lens' Purchase (Core.Maybe [Types.String])
pHostIdSet = Lens.field @"hostIdSet"
{-# DEPRECATED pHostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead." #-}

-- | The ID of the reservation.
--
-- /Note:/ Consider using 'hostReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHostReservationId :: Lens.Lens' Purchase (Core.Maybe Types.String)
pHostReservationId = Lens.field @"hostReservationId"
{-# DEPRECATED pHostReservationId "Use generic-lens or generic-optics with 'hostReservationId' instead." #-}

-- | The hourly price of the reservation per hour.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHourlyPrice :: Lens.Lens' Purchase (Core.Maybe Types.String)
pHourlyPrice = Lens.field @"hourlyPrice"
{-# DEPRECATED pHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The instance family on the Dedicated Host that the reservation can be associated with.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pInstanceFamily :: Lens.Lens' Purchase (Core.Maybe Types.String)
pInstanceFamily = Lens.field @"instanceFamily"
{-# DEPRECATED pInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The payment option for the reservation.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPaymentOption :: Lens.Lens' Purchase (Core.Maybe Types.PaymentOption)
pPaymentOption = Lens.field @"paymentOption"
{-# DEPRECATED pPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

-- | The upfront price of the reservation.
--
-- /Note:/ Consider using 'upfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUpfrontPrice :: Lens.Lens' Purchase (Core.Maybe Types.String)
pUpfrontPrice = Lens.field @"upfrontPrice"
{-# DEPRECATED pUpfrontPrice "Use generic-lens or generic-optics with 'upfrontPrice' instead." #-}

instance Core.FromXML Purchase where
  parseXML x =
    Purchase'
      Core.<$> (x Core..@? "currencyCode")
      Core.<*> (x Core..@? "duration")
      Core.<*> (x Core..@? "hostIdSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "hostReservationId")
      Core.<*> (x Core..@? "hourlyPrice")
      Core.<*> (x Core..@? "instanceFamily")
      Core.<*> (x Core..@? "paymentOption")
      Core.<*> (x Core..@? "upfrontPrice")
