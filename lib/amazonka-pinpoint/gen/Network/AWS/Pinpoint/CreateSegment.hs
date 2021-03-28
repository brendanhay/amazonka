{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration, dimension, and other settings for an existing segment that's associated with an application.
module Network.AWS.Pinpoint.CreateSegment
    (
    -- * Creating a request
      CreateSegment (..)
    , mkCreateSegment
    -- ** Request lenses
    , csApplicationId
    , csWriteSegmentRequest

    -- * Destructuring the response
    , CreateSegmentResponse (..)
    , mkCreateSegmentResponse
    -- ** Response lenses
    , csrrsSegmentResponse
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSegment' smart constructor.
data CreateSegment = CreateSegment'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , writeSegmentRequest :: Types.WriteSegmentRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSegment' value with any optional fields omitted.
mkCreateSegment
    :: Core.Text -- ^ 'applicationId'
    -> Types.WriteSegmentRequest -- ^ 'writeSegmentRequest'
    -> CreateSegment
mkCreateSegment applicationId writeSegmentRequest
  = CreateSegment'{applicationId, writeSegmentRequest}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csApplicationId :: Lens.Lens' CreateSegment Core.Text
csApplicationId = Lens.field @"applicationId"
{-# INLINEABLE csApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeSegmentRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csWriteSegmentRequest :: Lens.Lens' CreateSegment Types.WriteSegmentRequest
csWriteSegmentRequest = Lens.field @"writeSegmentRequest"
{-# INLINEABLE csWriteSegmentRequest #-}
{-# DEPRECATED writeSegmentRequest "Use generic-lens or generic-optics with 'writeSegmentRequest' instead"  #-}

instance Core.ToQuery CreateSegment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSegment where
        toHeaders CreateSegment{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateSegment where
        toJSON CreateSegment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WriteSegmentRequest" Core..= writeSegmentRequest)])

instance Core.AWSRequest CreateSegment where
        type Rs CreateSegment = CreateSegmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/segments",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSegmentResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSegmentResponse' smart constructor.
data CreateSegmentResponse = CreateSegmentResponse'
  { segmentResponse :: Types.SegmentResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSegmentResponse' value with any optional fields omitted.
mkCreateSegmentResponse
    :: Types.SegmentResponse -- ^ 'segmentResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateSegmentResponse
mkCreateSegmentResponse segmentResponse responseStatus
  = CreateSegmentResponse'{segmentResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsSegmentResponse :: Lens.Lens' CreateSegmentResponse Types.SegmentResponse
csrrsSegmentResponse = Lens.field @"segmentResponse"
{-# INLINEABLE csrrsSegmentResponse #-}
{-# DEPRECATED segmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateSegmentResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
