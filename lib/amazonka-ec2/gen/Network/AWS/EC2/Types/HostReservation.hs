{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostReservation
  ( HostReservation (..),

    -- * Smart constructor
    mkHostReservation,

    -- * Lenses
    hrState,
    hrInstanceFamily,
    hrCurrencyCode,
    hrHostReservationId,
    hrStart,
    hrHourlyPrice,
    hrCount,
    hrUpfrontPrice,
    hrEnd,
    hrHostIdSet,
    hrOfferingId,
    hrDuration,
    hrTags,
    hrPaymentOption,
  )
where

import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.PaymentOption
import Network.AWS.EC2.Types.ReservationState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the Dedicated Host Reservation and associated Dedicated Hosts.
--
-- /See:/ 'mkHostReservation' smart constructor.
data HostReservation = HostReservation'
  { -- | The state of the reservation.
    state :: Lude.Maybe ReservationState,
    -- | The instance family of the Dedicated Host Reservation. The instance family on the Dedicated Host must be the same in order for it to benefit from the reservation.
    instanceFamily :: Lude.Maybe Lude.Text,
    -- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
    currencyCode :: Lude.Maybe CurrencyCodeValues,
    -- | The ID of the reservation that specifies the associated Dedicated Hosts.
    hostReservationId :: Lude.Maybe Lude.Text,
    -- | The date and time that the reservation started.
    start :: Lude.Maybe Lude.DateTime,
    -- | The hourly price of the reservation.
    hourlyPrice :: Lude.Maybe Lude.Text,
    -- | The number of Dedicated Hosts the reservation is associated with.
    count :: Lude.Maybe Lude.Int,
    -- | The upfront price of the reservation.
    upfrontPrice :: Lude.Maybe Lude.Text,
    -- | The date and time that the reservation ends.
    end :: Lude.Maybe Lude.DateTime,
    -- | The IDs of the Dedicated Hosts associated with the reservation.
    hostIdSet :: Lude.Maybe [Lude.Text],
    -- | The ID of the reservation. This remains the same regardless of which Dedicated Hosts are associated with it.
    offeringId :: Lude.Maybe Lude.Text,
    -- | The length of the reservation's term, specified in seconds. Can be @31536000 (1 year)@ | @94608000 (3 years)@ .
    duration :: Lude.Maybe Lude.Int,
    -- | Any tags assigned to the Dedicated Host Reservation.
    tags :: Lude.Maybe [Tag],
    -- | The payment option selected for this reservation.
    paymentOption :: Lude.Maybe PaymentOption
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostReservation' with the minimum fields required to make a request.
--
-- * 'state' - The state of the reservation.
-- * 'instanceFamily' - The instance family of the Dedicated Host Reservation. The instance family on the Dedicated Host must be the same in order for it to benefit from the reservation.
-- * 'currencyCode' - The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
-- * 'hostReservationId' - The ID of the reservation that specifies the associated Dedicated Hosts.
-- * 'start' - The date and time that the reservation started.
-- * 'hourlyPrice' - The hourly price of the reservation.
-- * 'count' - The number of Dedicated Hosts the reservation is associated with.
-- * 'upfrontPrice' - The upfront price of the reservation.
-- * 'end' - The date and time that the reservation ends.
-- * 'hostIdSet' - The IDs of the Dedicated Hosts associated with the reservation.
-- * 'offeringId' - The ID of the reservation. This remains the same regardless of which Dedicated Hosts are associated with it.
-- * 'duration' - The length of the reservation's term, specified in seconds. Can be @31536000 (1 year)@ | @94608000 (3 years)@ .
-- * 'tags' - Any tags assigned to the Dedicated Host Reservation.
-- * 'paymentOption' - The payment option selected for this reservation.
mkHostReservation ::
  HostReservation
mkHostReservation =
  HostReservation'
    { state = Lude.Nothing,
      instanceFamily = Lude.Nothing,
      currencyCode = Lude.Nothing,
      hostReservationId = Lude.Nothing,
      start = Lude.Nothing,
      hourlyPrice = Lude.Nothing,
      count = Lude.Nothing,
      upfrontPrice = Lude.Nothing,
      end = Lude.Nothing,
      hostIdSet = Lude.Nothing,
      offeringId = Lude.Nothing,
      duration = Lude.Nothing,
      tags = Lude.Nothing,
      paymentOption = Lude.Nothing
    }

-- | The state of the reservation.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrState :: Lens.Lens' HostReservation (Lude.Maybe ReservationState)
hrState = Lens.lens (state :: HostReservation -> Lude.Maybe ReservationState) (\s a -> s {state = a} :: HostReservation)
{-# DEPRECATED hrState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The instance family of the Dedicated Host Reservation. The instance family on the Dedicated Host must be the same in order for it to benefit from the reservation.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrInstanceFamily :: Lens.Lens' HostReservation (Lude.Maybe Lude.Text)
hrInstanceFamily = Lens.lens (instanceFamily :: HostReservation -> Lude.Maybe Lude.Text) (\s a -> s {instanceFamily = a} :: HostReservation)
{-# DEPRECATED hrInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The currency in which the @upfrontPrice@ and @hourlyPrice@ amounts are specified. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrCurrencyCode :: Lens.Lens' HostReservation (Lude.Maybe CurrencyCodeValues)
hrCurrencyCode = Lens.lens (currencyCode :: HostReservation -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: HostReservation)
{-# DEPRECATED hrCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The ID of the reservation that specifies the associated Dedicated Hosts.
--
-- /Note:/ Consider using 'hostReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrHostReservationId :: Lens.Lens' HostReservation (Lude.Maybe Lude.Text)
hrHostReservationId = Lens.lens (hostReservationId :: HostReservation -> Lude.Maybe Lude.Text) (\s a -> s {hostReservationId = a} :: HostReservation)
{-# DEPRECATED hrHostReservationId "Use generic-lens or generic-optics with 'hostReservationId' instead." #-}

-- | The date and time that the reservation started.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrStart :: Lens.Lens' HostReservation (Lude.Maybe Lude.DateTime)
hrStart = Lens.lens (start :: HostReservation -> Lude.Maybe Lude.DateTime) (\s a -> s {start = a} :: HostReservation)
{-# DEPRECATED hrStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The hourly price of the reservation.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrHourlyPrice :: Lens.Lens' HostReservation (Lude.Maybe Lude.Text)
hrHourlyPrice = Lens.lens (hourlyPrice :: HostReservation -> Lude.Maybe Lude.Text) (\s a -> s {hourlyPrice = a} :: HostReservation)
{-# DEPRECATED hrHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The number of Dedicated Hosts the reservation is associated with.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrCount :: Lens.Lens' HostReservation (Lude.Maybe Lude.Int)
hrCount = Lens.lens (count :: HostReservation -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: HostReservation)
{-# DEPRECATED hrCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The upfront price of the reservation.
--
-- /Note:/ Consider using 'upfrontPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrUpfrontPrice :: Lens.Lens' HostReservation (Lude.Maybe Lude.Text)
hrUpfrontPrice = Lens.lens (upfrontPrice :: HostReservation -> Lude.Maybe Lude.Text) (\s a -> s {upfrontPrice = a} :: HostReservation)
{-# DEPRECATED hrUpfrontPrice "Use generic-lens or generic-optics with 'upfrontPrice' instead." #-}

-- | The date and time that the reservation ends.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrEnd :: Lens.Lens' HostReservation (Lude.Maybe Lude.DateTime)
hrEnd = Lens.lens (end :: HostReservation -> Lude.Maybe Lude.DateTime) (\s a -> s {end = a} :: HostReservation)
{-# DEPRECATED hrEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | The IDs of the Dedicated Hosts associated with the reservation.
--
-- /Note:/ Consider using 'hostIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrHostIdSet :: Lens.Lens' HostReservation (Lude.Maybe [Lude.Text])
hrHostIdSet = Lens.lens (hostIdSet :: HostReservation -> Lude.Maybe [Lude.Text]) (\s a -> s {hostIdSet = a} :: HostReservation)
{-# DEPRECATED hrHostIdSet "Use generic-lens or generic-optics with 'hostIdSet' instead." #-}

-- | The ID of the reservation. This remains the same regardless of which Dedicated Hosts are associated with it.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrOfferingId :: Lens.Lens' HostReservation (Lude.Maybe Lude.Text)
hrOfferingId = Lens.lens (offeringId :: HostReservation -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: HostReservation)
{-# DEPRECATED hrOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | The length of the reservation's term, specified in seconds. Can be @31536000 (1 year)@ | @94608000 (3 years)@ .
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrDuration :: Lens.Lens' HostReservation (Lude.Maybe Lude.Int)
hrDuration = Lens.lens (duration :: HostReservation -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: HostReservation)
{-# DEPRECATED hrDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Any tags assigned to the Dedicated Host Reservation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrTags :: Lens.Lens' HostReservation (Lude.Maybe [Tag])
hrTags = Lens.lens (tags :: HostReservation -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: HostReservation)
{-# DEPRECATED hrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The payment option selected for this reservation.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrPaymentOption :: Lens.Lens' HostReservation (Lude.Maybe PaymentOption)
hrPaymentOption = Lens.lens (paymentOption :: HostReservation -> Lude.Maybe PaymentOption) (\s a -> s {paymentOption = a} :: HostReservation)
{-# DEPRECATED hrPaymentOption "Use generic-lens or generic-optics with 'paymentOption' instead." #-}

instance Lude.FromXML HostReservation where
  parseXML x =
    HostReservation'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "instanceFamily")
      Lude.<*> (x Lude..@? "currencyCode")
      Lude.<*> (x Lude..@? "hostReservationId")
      Lude.<*> (x Lude..@? "start")
      Lude.<*> (x Lude..@? "hourlyPrice")
      Lude.<*> (x Lude..@? "count")
      Lude.<*> (x Lude..@? "upfrontPrice")
      Lude.<*> (x Lude..@? "end")
      Lude.<*> ( x Lude..@? "hostIdSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "offeringId")
      Lude.<*> (x Lude..@? "duration")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "paymentOption")
