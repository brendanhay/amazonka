{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a journey from an application.
module Network.AWS.Pinpoint.DeleteJourney
    (
    -- * Creating a request
      DeleteJourney (..)
    , mkDeleteJourney
    -- ** Request lenses
    , djJourneyId
    , djApplicationId

    -- * Destructuring the response
    , DeleteJourneyResponse (..)
    , mkDeleteJourneyResponse
    -- ** Response lenses
    , djrrsJourneyResponse
    , djrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteJourney' smart constructor.
data DeleteJourney = DeleteJourney'
  { journeyId :: Core.Text
    -- ^ The unique identifier for the journey.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteJourney' value with any optional fields omitted.
mkDeleteJourney
    :: Core.Text -- ^ 'journeyId'
    -> Core.Text -- ^ 'applicationId'
    -> DeleteJourney
mkDeleteJourney journeyId applicationId
  = DeleteJourney'{journeyId, applicationId}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJourneyId :: Lens.Lens' DeleteJourney Core.Text
djJourneyId = Lens.field @"journeyId"
{-# INLINEABLE djJourneyId #-}
{-# DEPRECATED journeyId "Use generic-lens or generic-optics with 'journeyId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djApplicationId :: Lens.Lens' DeleteJourney Core.Text
djApplicationId = Lens.field @"applicationId"
{-# INLINEABLE djApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteJourney where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteJourney where
        toHeaders DeleteJourney{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteJourney where
        type Rs DeleteJourney = DeleteJourneyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/journeys/"
                             Core.<> Core.toText journeyId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteJourneyResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteJourneyResponse' smart constructor.
data DeleteJourneyResponse = DeleteJourneyResponse'
  { journeyResponse :: Types.JourneyResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteJourneyResponse' value with any optional fields omitted.
mkDeleteJourneyResponse
    :: Types.JourneyResponse -- ^ 'journeyResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteJourneyResponse
mkDeleteJourneyResponse journeyResponse responseStatus
  = DeleteJourneyResponse'{journeyResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsJourneyResponse :: Lens.Lens' DeleteJourneyResponse Types.JourneyResponse
djrrsJourneyResponse = Lens.field @"journeyResponse"
{-# INLINEABLE djrrsJourneyResponse #-}
{-# DEPRECATED journeyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrrsResponseStatus :: Lens.Lens' DeleteJourneyResponse Core.Int
djrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE djrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
