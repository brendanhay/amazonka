{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.GetLogEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists log events from the specified log stream. You can list all of the log events or filter using a time range.
--
-- By default, this operation returns as many log events as can fit in a response size of 1MB (up to 10,000 log events). You can get additional log events by specifying one of the tokens in a subsequent call. This operation can return empty results while there are more log events available through the token.
module Network.AWS.CloudWatchLogs.GetLogEvents
    (
    -- * Creating a request
      GetLogEvents (..)
    , mkGetLogEvents
    -- ** Request lenses
    , gleLogGroupName
    , gleLogStreamName
    , gleEndTime
    , gleLimit
    , gleNextToken
    , gleStartFromHead
    , gleStartTime

    -- * Destructuring the response
    , GetLogEventsResponse (..)
    , mkGetLogEventsResponse
    -- ** Response lenses
    , glerrsEvents
    , glerrsNextBackwardToken
    , glerrsNextForwardToken
    , glerrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLogEvents' smart constructor.
data GetLogEvents = GetLogEvents'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  , logStreamName :: Types.LogStreamName
    -- ^ The name of the log stream.
  , endTime :: Core.Maybe Core.Natural
    -- ^ The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than this time are not included.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of log events returned. If you don't specify a value, the maximum is as many log events as can fit in a response size of 1 MB, up to 10,000 log events.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
--
-- Using this token works only when you specify @true@ for @startFromHead@ .
  , startFromHead :: Core.Maybe Core.Bool
    -- ^ If the value is true, the earliest log events are returned first. If the value is false, the latest log events are returned first. The default value is false.
--
-- If you are using @nextToken@ in this operation, you must specify @true@ for @startFromHead@ .
  , startTime :: Core.Maybe Core.Natural
    -- ^ The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this time or later than this time are included. Events with a timestamp earlier than this time are not included.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLogEvents' value with any optional fields omitted.
mkGetLogEvents
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> Types.LogStreamName -- ^ 'logStreamName'
    -> GetLogEvents
mkGetLogEvents logGroupName logStreamName
  = GetLogEvents'{logGroupName, logStreamName,
                  endTime = Core.Nothing, limit = Core.Nothing,
                  nextToken = Core.Nothing, startFromHead = Core.Nothing,
                  startTime = Core.Nothing}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleLogGroupName :: Lens.Lens' GetLogEvents Types.LogGroupName
gleLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE gleLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleLogStreamName :: Lens.Lens' GetLogEvents Types.LogStreamName
gleLogStreamName = Lens.field @"logStreamName"
{-# INLINEABLE gleLogStreamName #-}
{-# DEPRECATED logStreamName "Use generic-lens or generic-optics with 'logStreamName' instead"  #-}

-- | The end of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to or later than this time are not included.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleEndTime :: Lens.Lens' GetLogEvents (Core.Maybe Core.Natural)
gleEndTime = Lens.field @"endTime"
{-# INLINEABLE gleEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The maximum number of log events returned. If you don't specify a value, the maximum is as many log events as can fit in a response size of 1 MB, up to 10,000 log events.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleLimit :: Lens.Lens' GetLogEvents (Core.Maybe Core.Natural)
gleLimit = Lens.field @"limit"
{-# INLINEABLE gleLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- Using this token works only when you specify @true@ for @startFromHead@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleNextToken :: Lens.Lens' GetLogEvents (Core.Maybe Types.NextToken)
gleNextToken = Lens.field @"nextToken"
{-# INLINEABLE gleNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | If the value is true, the earliest log events are returned first. If the value is false, the latest log events are returned first. The default value is false.
--
-- If you are using @nextToken@ in this operation, you must specify @true@ for @startFromHead@ .
--
-- /Note:/ Consider using 'startFromHead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleStartFromHead :: Lens.Lens' GetLogEvents (Core.Maybe Core.Bool)
gleStartFromHead = Lens.field @"startFromHead"
{-# INLINEABLE gleStartFromHead #-}
{-# DEPRECATED startFromHead "Use generic-lens or generic-optics with 'startFromHead' instead"  #-}

-- | The start of the time range, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp equal to this time or later than this time are included. Events with a timestamp earlier than this time are not included.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gleStartTime :: Lens.Lens' GetLogEvents (Core.Maybe Core.Natural)
gleStartTime = Lens.field @"startTime"
{-# INLINEABLE gleStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery GetLogEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLogEvents where
        toHeaders GetLogEvents{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.GetLogEvents") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetLogEvents where
        toJSON GetLogEvents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logGroupName" Core..= logGroupName),
                  Core.Just ("logStreamName" Core..= logStreamName),
                  ("endTime" Core..=) Core.<$> endTime,
                  ("limit" Core..=) Core.<$> limit,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("startFromHead" Core..=) Core.<$> startFromHead,
                  ("startTime" Core..=) Core.<$> startTime])

instance Core.AWSRequest GetLogEvents where
        type Rs GetLogEvents = GetLogEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLogEventsResponse' Core.<$>
                   (x Core..:? "events") Core.<*> x Core..:? "nextBackwardToken"
                     Core.<*> x Core..:? "nextForwardToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLogEventsResponse' smart constructor.
data GetLogEventsResponse = GetLogEventsResponse'
  { events :: Core.Maybe [Types.OutputLogEvent]
    -- ^ The events.
  , nextBackwardToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items in the backward direction. The token expires after 24 hours. This token is never null. If you have reached the end of the stream, it returns the same token you passed in.
  , nextForwardToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items in the forward direction. The token expires after 24 hours. If you have reached the end of the stream, it returns the same token you passed in.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLogEventsResponse' value with any optional fields omitted.
mkGetLogEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLogEventsResponse
mkGetLogEventsResponse responseStatus
  = GetLogEventsResponse'{events = Core.Nothing,
                          nextBackwardToken = Core.Nothing, nextForwardToken = Core.Nothing,
                          responseStatus}

-- | The events.
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glerrsEvents :: Lens.Lens' GetLogEventsResponse (Core.Maybe [Types.OutputLogEvent])
glerrsEvents = Lens.field @"events"
{-# INLINEABLE glerrsEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | The token for the next set of items in the backward direction. The token expires after 24 hours. This token is never null. If you have reached the end of the stream, it returns the same token you passed in.
--
-- /Note:/ Consider using 'nextBackwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glerrsNextBackwardToken :: Lens.Lens' GetLogEventsResponse (Core.Maybe Types.NextToken)
glerrsNextBackwardToken = Lens.field @"nextBackwardToken"
{-# INLINEABLE glerrsNextBackwardToken #-}
{-# DEPRECATED nextBackwardToken "Use generic-lens or generic-optics with 'nextBackwardToken' instead"  #-}

-- | The token for the next set of items in the forward direction. The token expires after 24 hours. If you have reached the end of the stream, it returns the same token you passed in.
--
-- /Note:/ Consider using 'nextForwardToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glerrsNextForwardToken :: Lens.Lens' GetLogEventsResponse (Core.Maybe Types.NextToken)
glerrsNextForwardToken = Lens.field @"nextForwardToken"
{-# INLINEABLE glerrsNextForwardToken #-}
{-# DEPRECATED nextForwardToken "Use generic-lens or generic-optics with 'nextForwardToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glerrsResponseStatus :: Lens.Lens' GetLogEventsResponse Core.Int
glerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
