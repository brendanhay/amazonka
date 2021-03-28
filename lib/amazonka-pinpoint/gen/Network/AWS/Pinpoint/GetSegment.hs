{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for a specific segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegment
    (
    -- * Creating a request
      GetSegment (..)
    , mkGetSegment
    -- ** Request lenses
    , gsSegmentId
    , gsApplicationId

    -- * Destructuring the response
    , GetSegmentResponse (..)
    , mkGetSegmentResponse
    -- ** Response lenses
    , gsrfrsSegmentResponse
    , gsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegment' smart constructor.
data GetSegment = GetSegment'
  { segmentId :: Core.Text
    -- ^ The unique identifier for the segment.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegment' value with any optional fields omitted.
mkGetSegment
    :: Core.Text -- ^ 'segmentId'
    -> Core.Text -- ^ 'applicationId'
    -> GetSegment
mkGetSegment segmentId applicationId
  = GetSegment'{segmentId, applicationId}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsSegmentId :: Lens.Lens' GetSegment Core.Text
gsSegmentId = Lens.field @"segmentId"
{-# INLINEABLE gsSegmentId #-}
{-# DEPRECATED segmentId "Use generic-lens or generic-optics with 'segmentId' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsApplicationId :: Lens.Lens' GetSegment Core.Text
gsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.ToQuery GetSegment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSegment where
        toHeaders GetSegment{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSegment where
        type Rs GetSegment = GetSegmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/segments/"
                             Core.<> Core.toText segmentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSegmentResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSegmentResponse' smart constructor.
data GetSegmentResponse = GetSegmentResponse'
  { segmentResponse :: Types.SegmentResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentResponse' value with any optional fields omitted.
mkGetSegmentResponse
    :: Types.SegmentResponse -- ^ 'segmentResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetSegmentResponse
mkGetSegmentResponse segmentResponse responseStatus
  = GetSegmentResponse'{segmentResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfrsSegmentResponse :: Lens.Lens' GetSegmentResponse Types.SegmentResponse
gsrfrsSegmentResponse = Lens.field @"segmentResponse"
{-# INLINEABLE gsrfrsSegmentResponse #-}
{-# DEPRECATED segmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrfrsResponseStatus :: Lens.Lens' GetSegmentResponse Core.Int
gsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
