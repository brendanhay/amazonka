{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DeleteReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an expired reservation.
module Network.AWS.MediaLive.DeleteReservation
    (
    -- * Creating a request
      DeleteReservation (..)
    , mkDeleteReservation
    -- ** Request lenses
    , dReservationId

    -- * Destructuring the response
    , DeleteReservationResponse (..)
    , mkDeleteReservationResponse
    -- ** Response lenses
    , drrrsArn
    , drrrsCount
    , drrrsCurrencyCode
    , drrrsDuration
    , drrrsDurationUnits
    , drrrsEnd
    , drrrsFixedPrice
    , drrrsName
    , drrrsOfferingDescription
    , drrrsOfferingId
    , drrrsOfferingType
    , drrrsRegion
    , drrrsReservationId
    , drrrsResourceSpecification
    , drrrsStart
    , drrrsState
    , drrrsTags
    , drrrsUsagePrice
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteReservationRequest
--
-- /See:/ 'mkDeleteReservation' smart constructor.
newtype DeleteReservation = DeleteReservation'
  { reservationId :: Core.Text
    -- ^ Unique reservation ID, e.g. '1234567'
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReservation' value with any optional fields omitted.
mkDeleteReservation
    :: Core.Text -- ^ 'reservationId'
    -> DeleteReservation
mkDeleteReservation reservationId
  = DeleteReservation'{reservationId}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReservationId :: Lens.Lens' DeleteReservation Core.Text
dReservationId = Lens.field @"reservationId"
{-# INLINEABLE dReservationId #-}
{-# DEPRECATED reservationId "Use generic-lens or generic-optics with 'reservationId' instead"  #-}

instance Core.ToQuery DeleteReservation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReservation where
        toHeaders DeleteReservation{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteReservation where
        type Rs DeleteReservation = DeleteReservationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/prod/reservations/" Core.<> Core.toText reservationId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteReservationResponse' Core.<$>
                   (x Core..:? "arn") Core.<*> x Core..:? "count" Core.<*>
                     x Core..:? "currencyCode"
                     Core.<*> x Core..:? "duration"
                     Core.<*> x Core..:? "durationUnits"
                     Core.<*> x Core..:? "end"
                     Core.<*> x Core..:? "fixedPrice"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "offeringDescription"
                     Core.<*> x Core..:? "offeringId"
                     Core.<*> x Core..:? "offeringType"
                     Core.<*> x Core..:? "region"
                     Core.<*> x Core..:? "reservationId"
                     Core.<*> x Core..:? "resourceSpecification"
                     Core.<*> x Core..:? "start"
                     Core.<*> x Core..:? "state"
                     Core.<*> x Core..:? "tags"
                     Core.<*> x Core..:? "usagePrice"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for DeleteReservationResponse
--
-- /See:/ 'mkDeleteReservationResponse' smart constructor.
data DeleteReservationResponse = DeleteReservationResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
  , count :: Core.Maybe Core.Int
    -- ^ Number of reserved resources
  , currencyCode :: Core.Maybe Core.Text
    -- ^ Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
  , duration :: Core.Maybe Core.Int
    -- ^ Lease duration, e.g. '12'
  , durationUnits :: Core.Maybe Types.OfferingDurationUnits
    -- ^ Units for duration, e.g. 'MONTHS'
  , end :: Core.Maybe Core.Text
    -- ^ Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
  , fixedPrice :: Core.Maybe Core.Double
    -- ^ One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
  , name :: Core.Maybe Core.Text
    -- ^ User specified reservation name
  , offeringDescription :: Core.Maybe Core.Text
    -- ^ Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
  , offeringId :: Core.Maybe Core.Text
    -- ^ Unique offering ID, e.g. '87654321'
  , offeringType :: Core.Maybe Types.OfferingType
    -- ^ Offering type, e.g. 'NO_UPFRONT'
  , region :: Core.Maybe Core.Text
    -- ^ AWS region, e.g. 'us-west-2'
  , reservationId :: Core.Maybe Core.Text
    -- ^ Unique reservation ID, e.g. '1234567'
  , resourceSpecification :: Core.Maybe Types.ReservationResourceSpecification
    -- ^ Resource configuration details
  , start :: Core.Maybe Core.Text
    -- ^ Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
  , state :: Core.Maybe Types.ReservationState
    -- ^ Current state of reservation, e.g. 'ACTIVE'
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A collection of key-value pairs
  , usagePrice :: Core.Maybe Core.Double
    -- ^ Recurring usage charge for each reserved resource, e.g. '157.0'
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReservationResponse' value with any optional fields omitted.
mkDeleteReservationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReservationResponse
mkDeleteReservationResponse responseStatus
  = DeleteReservationResponse'{arn = Core.Nothing,
                               count = Core.Nothing, currencyCode = Core.Nothing,
                               duration = Core.Nothing, durationUnits = Core.Nothing,
                               end = Core.Nothing, fixedPrice = Core.Nothing, name = Core.Nothing,
                               offeringDescription = Core.Nothing, offeringId = Core.Nothing,
                               offeringType = Core.Nothing, region = Core.Nothing,
                               reservationId = Core.Nothing, resourceSpecification = Core.Nothing,
                               start = Core.Nothing, state = Core.Nothing, tags = Core.Nothing,
                               usagePrice = Core.Nothing, responseStatus}

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsArn :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsArn = Lens.field @"arn"
{-# INLINEABLE drrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Number of reserved resources
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsCount :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Int)
drrrsCount = Lens.field @"count"
{-# INLINEABLE drrrsCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsCurrencyCode :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE drrrsCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsDuration :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Int)
drrrsDuration = Lens.field @"duration"
{-# INLINEABLE drrrsDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsDurationUnits :: Lens.Lens' DeleteReservationResponse (Core.Maybe Types.OfferingDurationUnits)
drrrsDurationUnits = Lens.field @"durationUnits"
{-# INLINEABLE drrrsDurationUnits #-}
{-# DEPRECATED durationUnits "Use generic-lens or generic-optics with 'durationUnits' instead"  #-}

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsEnd :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsEnd = Lens.field @"end"
{-# INLINEABLE drrrsEnd #-}
{-# DEPRECATED end "Use generic-lens or generic-optics with 'end' instead"  #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsFixedPrice :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Double)
drrrsFixedPrice = Lens.field @"fixedPrice"
{-# INLINEABLE drrrsFixedPrice #-}
{-# DEPRECATED fixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead"  #-}

-- | User specified reservation name
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsName :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsName = Lens.field @"name"
{-# INLINEABLE drrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsOfferingDescription :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsOfferingDescription = Lens.field @"offeringDescription"
{-# INLINEABLE drrrsOfferingDescription #-}
{-# DEPRECATED offeringDescription "Use generic-lens or generic-optics with 'offeringDescription' instead"  #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsOfferingId :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsOfferingId = Lens.field @"offeringId"
{-# INLINEABLE drrrsOfferingId #-}
{-# DEPRECATED offeringId "Use generic-lens or generic-optics with 'offeringId' instead"  #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsOfferingType :: Lens.Lens' DeleteReservationResponse (Core.Maybe Types.OfferingType)
drrrsOfferingType = Lens.field @"offeringType"
{-# INLINEABLE drrrsOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRegion :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsRegion = Lens.field @"region"
{-# INLINEABLE drrrsRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | Unique reservation ID, e.g. '1234567'
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsReservationId :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsReservationId = Lens.field @"reservationId"
{-# INLINEABLE drrrsReservationId #-}
{-# DEPRECATED reservationId "Use generic-lens or generic-optics with 'reservationId' instead"  #-}

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResourceSpecification :: Lens.Lens' DeleteReservationResponse (Core.Maybe Types.ReservationResourceSpecification)
drrrsResourceSpecification = Lens.field @"resourceSpecification"
{-# INLINEABLE drrrsResourceSpecification #-}
{-# DEPRECATED resourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead"  #-}

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsStart :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Text)
drrrsStart = Lens.field @"start"
{-# INLINEABLE drrrsStart #-}
{-# DEPRECATED start "Use generic-lens or generic-optics with 'start' instead"  #-}

-- | Current state of reservation, e.g. 'ACTIVE'
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsState :: Lens.Lens' DeleteReservationResponse (Core.Maybe Types.ReservationState)
drrrsState = Lens.field @"state"
{-# INLINEABLE drrrsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | A collection of key-value pairs
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsTags :: Lens.Lens' DeleteReservationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
drrrsTags = Lens.field @"tags"
{-# INLINEABLE drrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsUsagePrice :: Lens.Lens' DeleteReservationResponse (Core.Maybe Core.Double)
drrrsUsagePrice = Lens.field @"usagePrice"
{-# INLINEABLE drrrsUsagePrice #-}
{-# DEPRECATED usagePrice "Use generic-lens or generic-optics with 'usagePrice' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteReservationResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
