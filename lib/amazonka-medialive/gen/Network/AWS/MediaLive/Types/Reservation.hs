{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    rArn,
    rCount,
    rCurrencyCode,
    rDuration,
    rDurationUnits,
    rEnd,
    rFixedPrice,
    rName,
    rOfferingDescription,
    rOfferingId,
    rOfferingType,
    rRegion,
    rReservationId,
    rResourceSpecification,
    rStart,
    rState,
    rTags,
    rUsagePrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.OfferingDurationUnits as Types
import qualified Network.AWS.MediaLive.Types.OfferingType as Types
import qualified Network.AWS.MediaLive.Types.ReservationResourceSpecification as Types
import qualified Network.AWS.MediaLive.Types.ReservationState as Types
import qualified Network.AWS.Prelude as Core

-- | Reserved resources available to use
--
-- /See:/ 'mkReservation' smart constructor.
data Reservation = Reservation'
  { -- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
    arn :: Core.Maybe Core.Text,
    -- | Number of reserved resources
    count :: Core.Maybe Core.Int,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
    currencyCode :: Core.Maybe Core.Text,
    -- | Lease duration, e.g. '12'
    duration :: Core.Maybe Core.Int,
    -- | Units for duration, e.g. 'MONTHS'
    durationUnits :: Core.Maybe Types.OfferingDurationUnits,
    -- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
    end :: Core.Maybe Core.Text,
    -- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
    fixedPrice :: Core.Maybe Core.Double,
    -- | User specified reservation name
    name :: Core.Maybe Core.Text,
    -- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
    offeringDescription :: Core.Maybe Core.Text,
    -- | Unique offering ID, e.g. '87654321'
    offeringId :: Core.Maybe Core.Text,
    -- | Offering type, e.g. 'NO_UPFRONT'
    offeringType :: Core.Maybe Types.OfferingType,
    -- | AWS region, e.g. 'us-west-2'
    region :: Core.Maybe Core.Text,
    -- | Unique reservation ID, e.g. '1234567'
    reservationId :: Core.Maybe Core.Text,
    -- | Resource configuration details
    resourceSpecification :: Core.Maybe Types.ReservationResourceSpecification,
    -- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
    start :: Core.Maybe Core.Text,
    -- | Current state of reservation, e.g. 'ACTIVE'
    state :: Core.Maybe Types.ReservationState,
    -- | A collection of key-value pairs
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Recurring usage charge for each reserved resource, e.g. '157.0'
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Reservation' value with any optional fields omitted.
mkReservation ::
  Reservation
mkReservation =
  Reservation'
    { arn = Core.Nothing,
      count = Core.Nothing,
      currencyCode = Core.Nothing,
      duration = Core.Nothing,
      durationUnits = Core.Nothing,
      end = Core.Nothing,
      fixedPrice = Core.Nothing,
      name = Core.Nothing,
      offeringDescription = Core.Nothing,
      offeringId = Core.Nothing,
      offeringType = Core.Nothing,
      region = Core.Nothing,
      reservationId = Core.Nothing,
      resourceSpecification = Core.Nothing,
      start = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rArn :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rArn = Lens.field @"arn"
{-# DEPRECATED rArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Number of reserved resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCount :: Lens.Lens' Reservation (Core.Maybe Core.Int)
rCount = Lens.field @"count"
{-# DEPRECATED rCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCurrencyCode :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDuration :: Lens.Lens' Reservation (Core.Maybe Core.Int)
rDuration = Lens.field @"duration"
{-# DEPRECATED rDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDurationUnits :: Lens.Lens' Reservation (Core.Maybe Types.OfferingDurationUnits)
rDurationUnits = Lens.field @"durationUnits"
{-# DEPRECATED rDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnd :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rEnd = Lens.field @"end"
{-# DEPRECATED rEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFixedPrice :: Lens.Lens' Reservation (Core.Maybe Core.Double)
rFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED rFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | User specified reservation name
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rName = Lens.field @"name"
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOfferingDescription :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rOfferingDescription = Lens.field @"offeringDescription"
{-# DEPRECATED rOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOfferingId :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rOfferingId = Lens.field @"offeringId"
{-# DEPRECATED rOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOfferingType :: Lens.Lens' Reservation (Core.Maybe Types.OfferingType)
rOfferingType = Lens.field @"offeringType"
{-# DEPRECATED rOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegion :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rRegion = Lens.field @"region"
{-# DEPRECATED rRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReservationId :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rReservationId = Lens.field @"reservationId"
{-# DEPRECATED rReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceSpecification :: Lens.Lens' Reservation (Core.Maybe Types.ReservationResourceSpecification)
rResourceSpecification = Lens.field @"resourceSpecification"
{-# DEPRECATED rResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStart :: Lens.Lens' Reservation (Core.Maybe Core.Text)
rStart = Lens.field @"start"
{-# DEPRECATED rStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Current state of reservation, e.g. 'ACTIVE'
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Reservation (Core.Maybe Types.ReservationState)
rState = Lens.field @"state"
{-# DEPRECATED rState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTags :: Lens.Lens' Reservation (Core.Maybe (Core.HashMap Core.Text Core.Text))
rTags = Lens.field @"tags"
{-# DEPRECATED rTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rUsagePrice :: Lens.Lens' Reservation (Core.Maybe Core.Double)
rUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED rUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromJSON Reservation where
  parseJSON =
    Core.withObject "Reservation" Core.$
      \x ->
        Reservation'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "count")
          Core.<*> (x Core..:? "currencyCode")
          Core.<*> (x Core..:? "duration")
          Core.<*> (x Core..:? "durationUnits")
          Core.<*> (x Core..:? "end")
          Core.<*> (x Core..:? "fixedPrice")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "offeringDescription")
          Core.<*> (x Core..:? "offeringId")
          Core.<*> (x Core..:? "offeringType")
          Core.<*> (x Core..:? "region")
          Core.<*> (x Core..:? "reservationId")
          Core.<*> (x Core..:? "resourceSpecification")
          Core.<*> (x Core..:? "start")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "usagePrice")
