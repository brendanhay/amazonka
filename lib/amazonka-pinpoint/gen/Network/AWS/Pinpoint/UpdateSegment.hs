{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration, dimension, and other settings for an existing segment that's associated with an application.
module Network.AWS.Pinpoint.UpdateSegment
    (
    -- * Creating a request
      UpdateSegment (..)
    , mkUpdateSegment
    -- ** Request lenses
    , usSegmentId
    , usApplicationId
    , usWriteSegmentRequest

    -- * Destructuring the response
    , UpdateSegmentResponse (..)
    , mkUpdateSegmentResponse
    -- ** Response lenses
    , usrrsSegmentResponse
    , usrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSegment' smart constructor.
data UpdateSegment = UpdateSegment'
  { segmentId :: Core.Text
    -- ^ The unique identifier for the segment.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , writeSegmentRequest :: Types.WriteSegmentRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSegment' value with any optional fields omitted.
mkUpdateSegment
    :: Core.Text -- ^ 'segmentId'
    -> Core.Text -- ^ 'applicationId'
    -> Types.WriteSegmentRequest -- ^ 'writeSegmentRequest'
    -> UpdateSegment
mkUpdateSegment segmentId applicationId writeSegmentRequest
  = UpdateSegment'{segmentId, applicationId, writeSegmentRequest}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSegmentId :: Lens.Lens' UpdateSegment Core.Text
usSegmentId = Lens.field @"segmentId"
{-# INLINEABLE usSegmentId #-}
{-# DEPRECATED segmentId "Use generic-lens or generic-optics with 'segmentId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usApplicationId :: Lens.Lens' UpdateSegment Core.Text
usApplicationId = Lens.field @"applicationId"
{-# INLINEABLE usApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeSegmentRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usWriteSegmentRequest :: Lens.Lens' UpdateSegment Types.WriteSegmentRequest
usWriteSegmentRequest = Lens.field @"writeSegmentRequest"
{-# INLINEABLE usWriteSegmentRequest #-}
{-# DEPRECATED writeSegmentRequest "Use generic-lens or generic-optics with 'writeSegmentRequest' instead"  #-}

instance Core.ToQuery UpdateSegment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSegment where
        toHeaders UpdateSegment{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSegment where
        toJSON UpdateSegment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WriteSegmentRequest" Core..= writeSegmentRequest)])

instance Core.AWSRequest UpdateSegment where
        type Rs UpdateSegment = UpdateSegmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/segments/"
                             Core.<> Core.toText segmentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSegmentResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateSegmentResponse' smart constructor.
data UpdateSegmentResponse = UpdateSegmentResponse'
  { segmentResponse :: Types.SegmentResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSegmentResponse' value with any optional fields omitted.
mkUpdateSegmentResponse
    :: Types.SegmentResponse -- ^ 'segmentResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateSegmentResponse
mkUpdateSegmentResponse segmentResponse responseStatus
  = UpdateSegmentResponse'{segmentResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsSegmentResponse :: Lens.Lens' UpdateSegmentResponse Types.SegmentResponse
usrrsSegmentResponse = Lens.field @"segmentResponse"
{-# INLINEABLE usrrsSegmentResponse #-}
{-# DEPRECATED segmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSegmentResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
