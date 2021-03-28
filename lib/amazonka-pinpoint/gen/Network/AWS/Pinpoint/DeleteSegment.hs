{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a segment from an application.
module Network.AWS.Pinpoint.DeleteSegment
    (
    -- * Creating a request
      DeleteSegment (..)
    , mkDeleteSegment
    -- ** Request lenses
    , dsSegmentId
    , dsApplicationId

    -- * Destructuring the response
    , DeleteSegmentResponse (..)
    , mkDeleteSegmentResponse
    -- ** Response lenses
    , dsrrsSegmentResponse
    , dsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSegment' smart constructor.
data DeleteSegment = DeleteSegment'
  { segmentId :: Core.Text
    -- ^ The unique identifier for the segment.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSegment' value with any optional fields omitted.
mkDeleteSegment
    :: Core.Text -- ^ 'segmentId'
    -> Core.Text -- ^ 'applicationId'
    -> DeleteSegment
mkDeleteSegment segmentId applicationId
  = DeleteSegment'{segmentId, applicationId}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSegmentId :: Lens.Lens' DeleteSegment Core.Text
dsSegmentId = Lens.field @"segmentId"
{-# INLINEABLE dsSegmentId #-}
{-# DEPRECATED segmentId "Use generic-lens or generic-optics with 'segmentId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsApplicationId :: Lens.Lens' DeleteSegment Core.Text
dsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE dsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery DeleteSegment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSegment where
        toHeaders DeleteSegment{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteSegment where
        type Rs DeleteSegment = DeleteSegmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/segments/"
                             Core.<> Core.toText segmentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSegmentResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSegmentResponse' smart constructor.
data DeleteSegmentResponse = DeleteSegmentResponse'
  { segmentResponse :: Types.SegmentResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSegmentResponse' value with any optional fields omitted.
mkDeleteSegmentResponse
    :: Types.SegmentResponse -- ^ 'segmentResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> DeleteSegmentResponse
mkDeleteSegmentResponse segmentResponse responseStatus
  = DeleteSegmentResponse'{segmentResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsSegmentResponse :: Lens.Lens' DeleteSegmentResponse Types.SegmentResponse
dsrrsSegmentResponse = Lens.field @"segmentResponse"
{-# INLINEABLE dsrrsSegmentResponse #-}
{-# DEPRECATED segmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrrsResponseStatus :: Lens.Lens' DeleteSegmentResponse Core.Int
dsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
