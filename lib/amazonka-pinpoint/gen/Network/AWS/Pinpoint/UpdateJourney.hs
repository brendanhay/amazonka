{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateJourney
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration and other settings for a journey.
module Network.AWS.Pinpoint.UpdateJourney
    (
    -- * Creating a request
      UpdateJourney (..)
    , mkUpdateJourney
    -- ** Request lenses
    , ujJourneyId
    , ujApplicationId
    , ujWriteJourneyRequest

    -- * Destructuring the response
    , UpdateJourneyResponse (..)
    , mkUpdateJourneyResponse
    -- ** Response lenses
    , ujrrsJourneyResponse
    , ujrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateJourney' smart constructor.
data UpdateJourney = UpdateJourney'
  { journeyId :: Core.Text
    -- ^ The unique identifier for the journey.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , writeJourneyRequest :: Types.WriteJourneyRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateJourney' value with any optional fields omitted.
mkUpdateJourney
    :: Core.Text -- ^ 'journeyId'
    -> Core.Text -- ^ 'applicationId'
    -> Types.WriteJourneyRequest -- ^ 'writeJourneyRequest'
    -> UpdateJourney
mkUpdateJourney journeyId applicationId writeJourneyRequest
  = UpdateJourney'{journeyId, applicationId, writeJourneyRequest}

-- | The unique identifier for the journey.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJourneyId :: Lens.Lens' UpdateJourney Core.Text
ujJourneyId = Lens.field @"journeyId"
{-# INLINEABLE ujJourneyId #-}
{-# DEPRECATED journeyId "Use generic-lens or generic-optics with 'journeyId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujApplicationId :: Lens.Lens' UpdateJourney Core.Text
ujApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ujApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeJourneyRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujWriteJourneyRequest :: Lens.Lens' UpdateJourney Types.WriteJourneyRequest
ujWriteJourneyRequest = Lens.field @"writeJourneyRequest"
{-# INLINEABLE ujWriteJourneyRequest #-}
{-# DEPRECATED writeJourneyRequest "Use generic-lens or generic-optics with 'writeJourneyRequest' instead"  #-}

instance Core.ToQuery UpdateJourney where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateJourney where
        toHeaders UpdateJourney{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateJourney where
        toJSON UpdateJourney{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WriteJourneyRequest" Core..= writeJourneyRequest)])

instance Core.AWSRequest UpdateJourney where
        type Rs UpdateJourney = UpdateJourneyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/journeys/"
                             Core.<> Core.toText journeyId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateJourneyResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateJourneyResponse' smart constructor.
data UpdateJourneyResponse = UpdateJourneyResponse'
  { journeyResponse :: Types.JourneyResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateJourneyResponse' value with any optional fields omitted.
mkUpdateJourneyResponse
    :: Types.JourneyResponse -- ^ 'journeyResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateJourneyResponse
mkUpdateJourneyResponse journeyResponse responseStatus
  = UpdateJourneyResponse'{journeyResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeyResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsJourneyResponse :: Lens.Lens' UpdateJourneyResponse Types.JourneyResponse
ujrrsJourneyResponse = Lens.field @"journeyResponse"
{-# INLINEABLE ujrrsJourneyResponse #-}
{-# DEPRECATED journeyResponse "Use generic-lens or generic-optics with 'journeyResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsResponseStatus :: Lens.Lens' UpdateJourneyResponse Core.Int
ujrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ujrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
