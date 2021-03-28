{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.ListStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @StreamInfo@ objects. Each object describes a stream. To retrieve only streams that satisfy a specific condition, you can specify a @StreamNameCondition@ . 
--
-- This operation returns paginated results.
module Network.AWS.KinesisVideo.ListStreams
    (
    -- * Creating a request
      ListStreams (..)
    , mkListStreams
    -- ** Request lenses
    , lsMaxResults
    , lsNextToken
    , lsStreamNameCondition

    -- * Destructuring the response
    , ListStreamsResponse (..)
    , mkListStreamsResponse
    -- ** Response lenses
    , lsrrsNextToken
    , lsrrsStreamInfoList
    , lsrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStreams' smart constructor.
data ListStreams = ListStreams'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of streams to return in the response. The default is 10,000.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If you specify this parameter, when the result of a @ListStreams@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of streams, provide this token in your next request.
  , streamNameCondition :: Core.Maybe Types.StreamNameCondition
    -- ^ Optional: Returns only streams that satisfy a specific condition. Currently, you can specify only the prefix of a stream name as a condition. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStreams' value with any optional fields omitted.
mkListStreams
    :: ListStreams
mkListStreams
  = ListStreams'{maxResults = Core.Nothing, nextToken = Core.Nothing,
                 streamNameCondition = Core.Nothing}

-- | The maximum number of streams to return in the response. The default is 10,000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsMaxResults :: Lens.Lens' ListStreams (Core.Maybe Core.Natural)
lsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If you specify this parameter, when the result of a @ListStreams@ operation is truncated, the call returns the @NextToken@ in the response. To get another batch of streams, provide this token in your next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsNextToken :: Lens.Lens' ListStreams (Core.Maybe Types.NextToken)
lsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Optional: Returns only streams that satisfy a specific condition. Currently, you can specify only the prefix of a stream name as a condition. 
--
-- /Note:/ Consider using 'streamNameCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStreamNameCondition :: Lens.Lens' ListStreams (Core.Maybe Types.StreamNameCondition)
lsStreamNameCondition = Lens.field @"streamNameCondition"
{-# INLINEABLE lsStreamNameCondition #-}
{-# DEPRECATED streamNameCondition "Use generic-lens or generic-optics with 'streamNameCondition' instead"  #-}

instance Core.ToQuery ListStreams where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListStreams where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON ListStreams where
        toJSON ListStreams{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("StreamNameCondition" Core..=) Core.<$> streamNameCondition])

instance Core.AWSRequest ListStreams where
        type Rs ListStreams = ListStreamsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/listStreams",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStreamsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "StreamInfoList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListStreams where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"streamInfoList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request. 
  , streamInfoList :: Core.Maybe [Types.StreamInfo]
    -- ^ An array of @StreamInfo@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListStreamsResponse' value with any optional fields omitted.
mkListStreamsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStreamsResponse
mkListStreamsResponse responseStatus
  = ListStreamsResponse'{nextToken = Core.Nothing,
                         streamInfoList = Core.Nothing, responseStatus}

-- | If the response is truncated, the call returns this element with a token. To get the next batch of streams, use this token in your next request. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsNextToken :: Lens.Lens' ListStreamsResponse (Core.Maybe Types.NextToken)
lsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | An array of @StreamInfo@ objects.
--
-- /Note:/ Consider using 'streamInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsStreamInfoList :: Lens.Lens' ListStreamsResponse (Core.Maybe [Types.StreamInfo])
lsrrsStreamInfoList = Lens.field @"streamInfoList"
{-# INLINEABLE lsrrsStreamInfoList #-}
{-# DEPRECATED streamInfoList "Use generic-lens or generic-optics with 'streamInfoList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrrsResponseStatus :: Lens.Lens' ListStreamsResponse Core.Int
lsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
