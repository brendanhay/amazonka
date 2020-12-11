-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Reservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Reservation
  ( Reservation (..),

    -- * Smart constructor
    mkReservation,

    -- * Lenses
    rState,
    rResourceSpecification,
    rCurrencyCode,
    rARN,
    rStart,
    rCount,
    rEnd,
    rName,
    rReservationId,
    rOfferingId,
    rRegion,
    rOfferingType,
    rUsagePrice,
    rFixedPrice,
    rDurationUnits,
    rOfferingDescription,
    rDuration,
    rTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OfferingDurationUnits
import Network.AWS.MediaLive.Types.OfferingType
import Network.AWS.MediaLive.Types.ReservationResourceSpecification
import Network.AWS.MediaLive.Types.ReservationState
import qualified Network.AWS.Prelude as Lude

-- | Reserved resources available to use
--
-- /See:/ 'mkReservation' smart constructor.
data Reservation = Reservation'
  { state ::
      Lude.Maybe ReservationState,
    resourceSpecification ::
      Lude.Maybe ReservationResourceSpecification,
    currencyCode :: Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    start :: Lude.Maybe Lude.Text,
    count :: Lude.Maybe Lude.Int,
    end :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    reservationId :: Lude.Maybe Lude.Text,
    offeringId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    offeringType :: Lude.Maybe OfferingType,
    usagePrice :: Lude.Maybe Lude.Double,
    fixedPrice :: Lude.Maybe Lude.Double,
    durationUnits :: Lude.Maybe OfferingDurationUnits,
    offeringDescription :: Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Int,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Reservation' with the minimum fields required to make a request.
--
-- * 'arn' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
-- * 'count' - Number of reserved resources
-- * 'currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
-- * 'duration' - Lease duration, e.g. '12'
-- * 'durationUnits' - Units for duration, e.g. 'MONTHS'
-- * 'end' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
-- * 'fixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
-- * 'name' - User specified reservation name
-- * 'offeringDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
-- * 'offeringId' - Unique offering ID, e.g. '87654321'
-- * 'offeringType' - Offering type, e.g. 'NO_UPFRONT'
-- * 'region' - AWS region, e.g. 'us-west-2'
-- * 'reservationId' - Unique reservation ID, e.g. '1234567'
-- * 'resourceSpecification' - Resource configuration details
-- * 'start' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
-- * 'state' - Current state of reservation, e.g. 'ACTIVE'
-- * 'tags' - A collection of key-value pairs
-- * 'usagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
mkReservation ::
  Reservation
mkReservation =
  Reservation'
    { state = Lude.Nothing,
      resourceSpecification = Lude.Nothing,
      currencyCode = Lude.Nothing,
      arn = Lude.Nothing,
      start = Lude.Nothing,
      count = Lude.Nothing,
      end = Lude.Nothing,
      name = Lude.Nothing,
      reservationId = Lude.Nothing,
      offeringId = Lude.Nothing,
      region = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      durationUnits = Lude.Nothing,
      offeringDescription = Lude.Nothing,
      duration = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Current state of reservation, e.g. 'ACTIVE'
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Reservation (Lude.Maybe ReservationState)
rState = Lens.lens (state :: Reservation -> Lude.Maybe ReservationState) (\s a -> s {state = a} :: Reservation)
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceSpecification :: Lens.Lens' Reservation (Lude.Maybe ReservationResourceSpecification)
rResourceSpecification = Lens.lens (resourceSpecification :: Reservation -> Lude.Maybe ReservationResourceSpecification) (\s a -> s {resourceSpecification = a} :: Reservation)
{-# DEPRECATED rResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCurrencyCode :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rCurrencyCode = Lens.lens (currencyCode :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: Reservation)
{-# DEPRECATED rCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rARN :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rARN = Lens.lens (arn :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Reservation)
{-# DEPRECATED rARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStart :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rStart = Lens.lens (start :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {start = a} :: Reservation)
{-# DEPRECATED rStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Number of reserved resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCount :: Lens.Lens' Reservation (Lude.Maybe Lude.Int)
rCount = Lens.lens (count :: Reservation -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: Reservation)
{-# DEPRECATED rCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnd :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rEnd = Lens.lens (end :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {end = a} :: Reservation)
{-# DEPRECATED rEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | User specified reservation name
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rName = Lens.lens (name :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Reservation)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReservationId :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rReservationId = Lens.lens (reservationId :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {reservationId = a} :: Reservation)
{-# DEPRECATED rReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOfferingId :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rOfferingId = Lens.lens (offeringId :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: Reservation)
{-# DEPRECATED rOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegion :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rRegion = Lens.lens (region :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Reservation)
{-# DEPRECATED rRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOfferingType :: Lens.Lens' Reservation (Lude.Maybe OfferingType)
rOfferingType = Lens.lens (offeringType :: Reservation -> Lude.Maybe OfferingType) (\s a -> s {offeringType = a} :: Reservation)
{-# DEPRECATED rOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUsagePrice :: Lens.Lens' Reservation (Lude.Maybe Lude.Double)
rUsagePrice = Lens.lens (usagePrice :: Reservation -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: Reservation)
{-# DEPRECATED rUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFixedPrice :: Lens.Lens' Reservation (Lude.Maybe Lude.Double)
rFixedPrice = Lens.lens (fixedPrice :: Reservation -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: Reservation)
{-# DEPRECATED rFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDurationUnits :: Lens.Lens' Reservation (Lude.Maybe OfferingDurationUnits)
rDurationUnits = Lens.lens (durationUnits :: Reservation -> Lude.Maybe OfferingDurationUnits) (\s a -> s {durationUnits = a} :: Reservation)
{-# DEPRECATED rDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOfferingDescription :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rOfferingDescription = Lens.lens (offeringDescription :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {offeringDescription = a} :: Reservation)
{-# DEPRECATED rOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDuration :: Lens.Lens' Reservation (Lude.Maybe Lude.Int)
rDuration = Lens.lens (duration :: Reservation -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: Reservation)
{-# DEPRECATED rDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTags :: Lens.Lens' Reservation (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rTags = Lens.lens (tags :: Reservation -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Reservation)
{-# DEPRECATED rTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Reservation where
  parseJSON =
    Lude.withObject
      "Reservation"
      ( \x ->
          Reservation'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "resourceSpecification")
            Lude.<*> (x Lude..:? "currencyCode")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "start")
            Lude.<*> (x Lude..:? "count")
            Lude.<*> (x Lude..:? "end")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "reservationId")
            Lude.<*> (x Lude..:? "offeringId")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "offeringType")
            Lude.<*> (x Lude..:? "usagePrice")
            Lude.<*> (x Lude..:? "fixedPrice")
            Lude.<*> (x Lude..:? "durationUnits")
            Lude.<*> (x Lude..:? "offeringDescription")
            Lude.<*> (x Lude..:? "duration")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
