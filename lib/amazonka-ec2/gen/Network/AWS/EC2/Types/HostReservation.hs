{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.HostReservation
  ( HostReservation (..)
  -- * Smart constructor
  , mkHostReservation
  -- * Lenses
  , hrCount
  , hrCurrencyCode
  , hrDuration
  , hrEnd
  , hrHostIdSet
  , hrHostReservationId
  , hrHourlyPrice
  , hrInstanceFamily
  , hrOfferingId
  , hrPaymentOption
  , hrStart
  , hrState
  , hrTags
  , hrUpfrontPrice
  ) where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.EC2.Types.PaymentOption as Types
import qualified Network.AWS.EC2.Types.ReservationState as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the Dedicated Host Reservation and associated Dedicated Hosts.
--
-- /See:/ 'mkHostReservation' smart constructor.
data HostReservation = HostReservation'
  { count :: Core.Maybe Core.Int
    -- ^ The number of Dedicated Hosts the reservation is associated with.
  , currencyCode :: Core.Maybe Types.CurrencyCodeValues
    -- ^ The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
  , duration :: Core.Maybe Core.Int
    -- ^ The length of the reservation's term, specified in seconds. Can be @31536000 (1 year)@ | @94608000 (3 years)@ .
  , end :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the reservation ends.
  , hostIdSet :: Core.Maybe [Core.Text]
    -- ^ The IDs of the Dedicated Hosts associated with the reservation.
  , hostReservationId :: Core.Maybe Core.Text
    -- ^ The ID of the reservation that specifies the associated Dedicated Hosts.
  , hourlyPrice :: Core.Maybe Core.Text
    -- ^ The hourly price of the reservation.
  , instanceFamily :: Core.Maybe Core.Text
    -- ^ The instance family of the Dedicated Host Reservation. The instance family on the Dedicated Host must be the same in order for it to benefit from the reservation.
  , offeringId :: Core.Maybe Core.Text
    -- ^ The ID of the reservation. This remains the same regardless of which Dedicated Hosts are associated with it.
  , paymentOption :: Core.Maybe Types.PaymentOption
    -- ^ The payment option selected for this reservation.
  , start :: Core.Maybe Core.UTCTime
    -- ^ The date and time that the reservation started.
  , state :: Core.Maybe Types.ReservationState
    -- ^ The state of the reservation.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the Dedicated Host Reservation.
  , upfrontPrice :: Core.Maybe Core.Text
    -- ^ The upfront price of the reservation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'HostReservation' value with any optional fields omitted.
mkHostReservation
    :: HostReservation
mkHostReservation
  = HostReservation'{count = Core.Nothing,
                     currencyCode = Core.Nothing, duration = Core.Nothing,
                     end = Core.Nothing, hostIdSet = Core.Nothing,
                     hostReservationId = Core.Nothing, hourlyPrice = Core.Nothing,
                     instanceFamily = Core.Nothing, offeringId = Core.Nothing,
                     paymentOption = Core.Nothing, start = Core.Nothing,
                     state = Core.Nothing, tags = Core.Nothing,
                     upfrontPrice = Core.Nothing}

-- | The number of Dedicated Hosts the reservation is associated with.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrCount :: Lens.Lens' HostReservation (Core.Maybe Core.Int)
hrCount = Lens.field @"count"
{-# INLINEABLE hrCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrCurrencyCode :: Lens.Lens' HostReservation (Core.Maybe Types.CurrencyCodeValues)
hrCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE hrCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The length of the reservation's term, specified in seconds. Can be @31536000 (1 year)@ | @94608000 (3 years)@ .
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrDuration :: Lens.Lens' HostReservation (Core.Maybe Core.Int)
hrDuration = Lens.field @"duration"
{-# INLINEABLE hrDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The date and time that the reservation ends.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrEnd :: Lens.Lens' HostReservation (Core.Maybe Core.UTCTime)
hrEnd = Lens.field @"end"
{-# INLINEABLE hrEnd #-}
{-# DEPRECATED end "Use generic-lens or generic-optics with 'end' instead"  #-}

-- | The IDs of the Dedicated Hosts associated with the reservation.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrHostIdSet :: Lens.Lens' HostReservation (Core.Maybe [Core.Text])
hrHostIdSet = Lens.field @"hostIdSet"
{-# INLINEABLE hrHostIdSet #-}
{-# DEPRECATED hostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead"  #-}

-- | The ID of the reservation that specifies the associated Dedicated Hosts.
--
-- /Note:/ Consider using 'hostReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrHostReservationId :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hrHostReservationId = Lens.field @"hostReservationId"
{-# INLINEABLE hrHostReservationId #-}
{-# DEPRECATED hostReservationId "Use generic-lens or generic-optics with 'hostReservationId' instead"  #-}

-- | The hourly price of the reservation.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrHourlyPrice :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hrHourlyPrice = Lens.field @"hourlyPrice"
{-# INLINEABLE hrHourlyPrice #-}
{-# DEPRECATED hourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead"  #-}

-- | The instance family of the Dedicated Host Reservation. The instance family on the Dedicated Host must be the same in order for it to benefit from the reservation.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrInstanceFamily :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hrInstanceFamily = Lens.field @"instanceFamily"
{-# INLINEABLE hrInstanceFamily #-}
{-# DEPRECATED instanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead"  #-}

-- | The ID of the reservation. This remains the same regardless of which Dedicated Hosts are associated with it.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrOfferingId :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hrOfferingId = Lens.field @"offeringId"
{-# INLINEABLE hrOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

-- | The payment option selected for this reservation.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrPaymentOption :: Lens.Lens' HostReservation (Core.Maybe Types.PaymentOption)
hrPaymentOption = Lens.field @"paymentOption"
{-# INLINEABLE hrPaymentOption #-}
{-# DEPRECATED paymentOption "Use generic-lens or generic-optics with 'paymentOption' instead"  #-}

-- | The date and time that the reservation started.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrStart :: Lens.Lens' HostReservation (Core.Maybe Core.UTCTime)
hrStart = Lens.field @"start"
{-# INLINEABLE hrStart #-}
{-# DEPRECATED start "Use generic-lens or generic-optics with 'start' instead"  #-}

-- | The state of the reservation.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrState :: Lens.Lens' HostReservation (Core.Maybe Types.ReservationState)
hrState = Lens.field @"state"
{-# INLINEABLE hrState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Any tags assigned to the Dedicated Host Reservation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrTags :: Lens.Lens' HostReservation (Core.Maybe [Types.Tag])
hrTags = Lens.field @"tags"
{-# INLINEABLE hrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The upfront price of the reservation.
--
-- /Note:/ Consider using 'upfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrUpfrontPrice :: Lens.Lens' HostReservation (Core.Maybe Core.Text)
hrUpfrontPrice = Lens.field @"upfrontPrice"
{-# INLINEABLE hrUpfrontPrice #-}
{-# DEPRECATED upfrontPrice "Use generic-lens or generic-optics with 'upfrontPrice' instead"  #-}

instance Core.FromXML HostReservation where
        parseXML x
          = HostReservation' Core.<$>
              (x Core..@? "count") Core.<*> x Core..@? "currencyCode" Core.<*>
                x Core..@? "duration"
                Core.<*> x Core..@? "end"
                Core.<*> x Core..@? "hostIdSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "hostReservationId"
                Core.<*> x Core..@? "hourlyPrice"
                Core.<*> x Core..@? "instanceFamily"
                Core.<*> x Core..@? "offeringId"
                Core.<*> x Core..@? "paymentOption"
                Core.<*> x Core..@? "start"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "upfrontPrice"
