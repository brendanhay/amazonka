{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListEventSourceMappings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists event source mappings. Specify an @EventSourceArn@ to only show event source mappings for a single event source.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListEventSourceMappings
    (
    -- * Creating a request
      ListEventSourceMappings (..)
    , mkListEventSourceMappings
    -- ** Request lenses
    , lesmEventSourceArn
    , lesmFunctionName
    , lesmMarker
    , lesmMaxItems

    -- * Destructuring the response
    , ListEventSourceMappingsResponse (..)
    , mkListEventSourceMappingsResponse
    -- ** Response lenses
    , lesmrrsEventSourceMappings
    , lesmrrsNextMarker
    , lesmrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEventSourceMappings' smart constructor.
data ListEventSourceMappings = ListEventSourceMappings'
  { eventSourceArn :: Core.Maybe Types.EventSourceArn
    -- ^ The Amazon Resource Name (ARN) of the event source.
--
--
--     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.
--
--
--     * __Amazon DynamoDB Streams__ - The ARN of the stream.
--
--
--     * __Amazon Simple Queue Service__ - The ARN of the queue.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
--
--
  , functionName :: Core.Maybe Types.FunctionName
    -- ^ The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
  , marker :: Core.Maybe Core.Text
    -- ^ A pagination token returned by a previous call.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ The maximum number of event source mappings to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListEventSourceMappings' value with any optional fields omitted.
mkListEventSourceMappings
    :: ListEventSourceMappings
mkListEventSourceMappings
  = ListEventSourceMappings'{eventSourceArn = Core.Nothing,
                             functionName = Core.Nothing, marker = Core.Nothing,
                             maxItems = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the event source.
--
--
--     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.
--
--
--     * __Amazon DynamoDB Streams__ - The ARN of the stream.
--
--
--     * __Amazon Simple Queue Service__ - The ARN of the queue.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
--
--
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmEventSourceArn :: Lens.Lens' ListEventSourceMappings (Core.Maybe Types.EventSourceArn)
lesmEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE lesmEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

-- | The name of the Lambda function.
--
-- __Name formats__ 
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmFunctionName :: Lens.Lens' ListEventSourceMappings (Core.Maybe Types.FunctionName)
lesmFunctionName = Lens.field @"functionName"
{-# INLINEABLE lesmFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | A pagination token returned by a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmMarker :: Lens.Lens' ListEventSourceMappings (Core.Maybe Core.Text)
lesmMarker = Lens.field @"marker"
{-# INLINEABLE lesmMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of event source mappings to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmMaxItems :: Lens.Lens' ListEventSourceMappings (Core.Maybe Core.Natural)
lesmMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lesmMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListEventSourceMappings where
        toQuery ListEventSourceMappings{..}
          = Core.maybe Core.mempty (Core.toQueryPair "EventSourceArn")
              eventSourceArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "FunctionName")
                functionName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListEventSourceMappings where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListEventSourceMappings where
        type Rs ListEventSourceMappings = ListEventSourceMappingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2015-03-31/event-source-mappings/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListEventSourceMappingsResponse' Core.<$>
                   (x Core..:? "EventSourceMappings") Core.<*> x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListEventSourceMappings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"eventSourceMappings" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListEventSourceMappingsResponse' smart constructor.
data ListEventSourceMappingsResponse = ListEventSourceMappingsResponse'
  { eventSourceMappings :: Core.Maybe [Types.EventSourceMappingConfiguration]
    -- ^ A list of event source mappings.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ A pagination token that's returned when the response doesn't contain all event source mappings.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEventSourceMappingsResponse' value with any optional fields omitted.
mkListEventSourceMappingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListEventSourceMappingsResponse
mkListEventSourceMappingsResponse responseStatus
  = ListEventSourceMappingsResponse'{eventSourceMappings =
                                       Core.Nothing,
                                     nextMarker = Core.Nothing, responseStatus}

-- | A list of event source mappings.
--
-- /Note:/ Consider using 'eventSourceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmrrsEventSourceMappings :: Lens.Lens' ListEventSourceMappingsResponse (Core.Maybe [Types.EventSourceMappingConfiguration])
lesmrrsEventSourceMappings = Lens.field @"eventSourceMappings"
{-# INLINEABLE lesmrrsEventSourceMappings #-}
{-# DEPRECATED eventSourceMappings "Use generic-lens or generic-optics with 'eventSourceMappings' instead"  #-}

-- | A pagination token that's returned when the response doesn't contain all event source mappings.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmrrsNextMarker :: Lens.Lens' ListEventSourceMappingsResponse (Core.Maybe Core.Text)
lesmrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lesmrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lesmrrsResponseStatus :: Lens.Lens' ListEventSourceMappingsResponse Core.Int
lesmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lesmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
