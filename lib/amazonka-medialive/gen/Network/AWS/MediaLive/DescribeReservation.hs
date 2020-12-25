{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for a reservation.
module Network.AWS.MediaLive.DescribeReservation
  ( -- * Creating a request
    DescribeReservation (..),
    mkDescribeReservation,

    -- ** Request lenses
    drReservationId,

    -- * Destructuring the response
    DescribeReservationResponse (..),
    mkDescribeReservationResponse,

    -- ** Response lenses
    drrfrsArn,
    drrfrsCount,
    drrfrsCurrencyCode,
    drrfrsDuration,
    drrfrsDurationUnits,
    drrfrsEnd,
    drrfrsFixedPrice,
    drrfrsName,
    drrfrsOfferingDescription,
    drrfrsOfferingId,
    drrfrsOfferingType,
    drrfrsRegion,
    drrfrsReservationId,
    drrfrsResourceSpecification,
    drrfrsStart,
    drrfrsState,
    drrfrsTags,
    drrfrsUsagePrice,
    drrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeReservationRequest
--
-- /See:/ 'mkDescribeReservation' smart constructor.
newtype DescribeReservation = DescribeReservation'
  { -- | Unique reservation ID, e.g. '1234567'
    reservationId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservation' value with any optional fields omitted.
mkDescribeReservation ::
  -- | 'reservationId'
  Core.Text ->
  DescribeReservation
mkDescribeReservation reservationId =
  DescribeReservation' {reservationId}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drReservationId :: Lens.Lens' DescribeReservation Core.Text
drReservationId = Lens.field @"reservationId"
{-# DEPRECATED drReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

instance Core.AWSRequest DescribeReservation where
  type Rs DescribeReservation = DescribeReservationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/prod/reservations/" Core.<> (Core.toText reservationId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservationResponse'
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
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DescribeReservationResponse
--
-- /See:/ 'mkDescribeReservationResponse' smart constructor.
data DescribeReservationResponse = DescribeReservationResponse'
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
    usagePrice :: Core.Maybe Core.Double,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeReservationResponse' value with any optional fields omitted.
mkDescribeReservationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeReservationResponse
mkDescribeReservationResponse responseStatus =
  DescribeReservationResponse'
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
      usagePrice = Core.Nothing,
      responseStatus
    }

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsArn :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsArn = Lens.field @"arn"
{-# DEPRECATED drrfrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Number of reserved resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsCount :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Int)
drrfrsCount = Lens.field @"count"
{-# DEPRECATED drrfrsCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsCurrencyCode :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED drrfrsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsDuration :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Int)
drrfrsDuration = Lens.field @"duration"
{-# DEPRECATED drrfrsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsDurationUnits :: Lens.Lens' DescribeReservationResponse (Core.Maybe Types.OfferingDurationUnits)
drrfrsDurationUnits = Lens.field @"durationUnits"
{-# DEPRECATED drrfrsDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsEnd :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsEnd = Lens.field @"end"
{-# DEPRECATED drrfrsEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsFixedPrice :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Double)
drrfrsFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED drrfrsFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | User specified reservation name
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsName :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsName = Lens.field @"name"
{-# DEPRECATED drrfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsOfferingDescription :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsOfferingDescription = Lens.field @"offeringDescription"
{-# DEPRECATED drrfrsOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsOfferingId :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsOfferingId = Lens.field @"offeringId"
{-# DEPRECATED drrfrsOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsOfferingType :: Lens.Lens' DescribeReservationResponse (Core.Maybe Types.OfferingType)
drrfrsOfferingType = Lens.field @"offeringType"
{-# DEPRECATED drrfrsOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsRegion :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsRegion = Lens.field @"region"
{-# DEPRECATED drrfrsRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsReservationId :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsReservationId = Lens.field @"reservationId"
{-# DEPRECATED drrfrsReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsResourceSpecification :: Lens.Lens' DescribeReservationResponse (Core.Maybe Types.ReservationResourceSpecification)
drrfrsResourceSpecification = Lens.field @"resourceSpecification"
{-# DEPRECATED drrfrsResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsStart :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Text)
drrfrsStart = Lens.field @"start"
{-# DEPRECATED drrfrsStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | Current state of reservation, e.g. 'ACTIVE'
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsState :: Lens.Lens' DescribeReservationResponse (Core.Maybe Types.ReservationState)
drrfrsState = Lens.field @"state"
{-# DEPRECATED drrfrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsTags :: Lens.Lens' DescribeReservationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
drrfrsTags = Lens.field @"tags"
{-# DEPRECATED drrfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsUsagePrice :: Lens.Lens' DescribeReservationResponse (Core.Maybe Core.Double)
drrfrsUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED drrfrsUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrfrsResponseStatus :: Lens.Lens' DescribeReservationResponse Core.Int
drrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
