{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutLogEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a batch of log events to the specified log stream.
--
-- You must include the sequence token obtained from the response of the previous call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token in the @expectedSequenceToken@ field from @InvalidSequenceTokenException@ . If you call @PutLogEvents@ twice within a narrow time period using the same value for @sequenceToken@ , both calls might be successful or one might be rejected.
-- The batch of events must satisfy the following constraints:
--
--     * The maximum batch size is 1,048,576 bytes. This size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
--
--
--     * None of the log events in the batch can be more than 2 hours in the future.
--
--
--     * None of the log events in the batch can be older than 14 days or older than the retention period of the log group.
--
--
--     * The log events in the batch must be in chronological order by their timestamp. The timestamp is the time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. (In AWS Tools for PowerShell and the AWS SDK for .NET, the timestamp is specified in .NET format: yyyy-mm-ddThh:mm:ss. For example, 2017-09-15T13:45:30.) 
--
--
--     * A batch of log events in a single request cannot span more than 24 hours. Otherwise, the operation fails.
--
--
--     * The maximum number of log events in a batch is 10,000.
--
--
--     * There is a quota of 5 requests per second per log stream. Additional requests are throttled. This quota can't be changed.
--
--
-- If a call to @PutLogEvents@ returns "UnrecognizedClientException" the most likely cause is an invalid AWS access key ID or secret key. 
module Network.AWS.CloudWatchLogs.PutLogEvents
    (
    -- * Creating a request
      PutLogEvents (..)
    , mkPutLogEvents
    -- ** Request lenses
    , pleLogGroupName
    , pleLogStreamName
    , pleLogEvents
    , pleSequenceToken

    -- * Destructuring the response
    , PutLogEventsResponse (..)
    , mkPutLogEventsResponse
    -- ** Response lenses
    , plerrsNextSequenceToken
    , plerrsRejectedLogEventsInfo
    , plerrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutLogEvents' smart constructor.
data PutLogEvents = PutLogEvents'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  , logStreamName :: Types.LogStreamName
    -- ^ The name of the log stream.
  , logEvents :: Core.NonEmpty Types.InputLogEvent
    -- ^ The log events.
  , sequenceToken :: Core.Maybe Types.SequenceToken
    -- ^ The sequence token obtained from the response of the previous @PutLogEvents@ call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token using <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html DescribeLogStreams> . If you call @PutLogEvents@ twice within a narrow time period using the same value for @sequenceToken@ , both calls might be successful or one might be rejected.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLogEvents' value with any optional fields omitted.
mkPutLogEvents
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> Types.LogStreamName -- ^ 'logStreamName'
    -> Core.NonEmpty Types.InputLogEvent -- ^ 'logEvents'
    -> PutLogEvents
mkPutLogEvents logGroupName logStreamName logEvents
  = PutLogEvents'{logGroupName, logStreamName, logEvents,
                  sequenceToken = Core.Nothing}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleLogGroupName :: Lens.Lens' PutLogEvents Types.LogGroupName
pleLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE pleLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleLogStreamName :: Lens.Lens' PutLogEvents Types.LogStreamName
pleLogStreamName = Lens.field @"logStreamName"
{-# INLINEABLE pleLogStreamName #-}
{-# DEPRECATED logStreamName "Use generic-lens or generic-optics with 'logStreamName' instead"  #-}

-- | The log events.
--
-- /Note:/ Consider using 'logEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleLogEvents :: Lens.Lens' PutLogEvents (Core.NonEmpty Types.InputLogEvent)
pleLogEvents = Lens.field @"logEvents"
{-# INLINEABLE pleLogEvents #-}
{-# DEPRECATED logEvents "Use generic-lens or generic-optics with 'logEvents' instead"  #-}

-- | The sequence token obtained from the response of the previous @PutLogEvents@ call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token using <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html DescribeLogStreams> . If you call @PutLogEvents@ twice within a narrow time period using the same value for @sequenceToken@ , both calls might be successful or one might be rejected.
--
-- /Note:/ Consider using 'sequenceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleSequenceToken :: Lens.Lens' PutLogEvents (Core.Maybe Types.SequenceToken)
pleSequenceToken = Lens.field @"sequenceToken"
{-# INLINEABLE pleSequenceToken #-}
{-# DEPRECATED sequenceToken "Use generic-lens or generic-optics with 'sequenceToken' instead"  #-}

instance Core.ToQuery PutLogEvents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutLogEvents where
        toHeaders PutLogEvents{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.PutLogEvents") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutLogEvents where
        toJSON PutLogEvents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logGroupName" Core..= logGroupName),
                  Core.Just ("logStreamName" Core..= logStreamName),
                  Core.Just ("logEvents" Core..= logEvents),
                  ("sequenceToken" Core..=) Core.<$> sequenceToken])

instance Core.AWSRequest PutLogEvents where
        type Rs PutLogEvents = PutLogEventsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutLogEventsResponse' Core.<$>
                   (x Core..:? "nextSequenceToken") Core.<*>
                     x Core..:? "rejectedLogEventsInfo"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutLogEventsResponse' smart constructor.
data PutLogEventsResponse = PutLogEventsResponse'
  { nextSequenceToken :: Core.Maybe Types.SequenceToken
    -- ^ The next sequence token.
  , rejectedLogEventsInfo :: Core.Maybe Types.RejectedLogEventsInfo
    -- ^ The rejected events.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutLogEventsResponse' value with any optional fields omitted.
mkPutLogEventsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutLogEventsResponse
mkPutLogEventsResponse responseStatus
  = PutLogEventsResponse'{nextSequenceToken = Core.Nothing,
                          rejectedLogEventsInfo = Core.Nothing, responseStatus}

-- | The next sequence token.
--
-- /Note:/ Consider using 'nextSequenceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plerrsNextSequenceToken :: Lens.Lens' PutLogEventsResponse (Core.Maybe Types.SequenceToken)
plerrsNextSequenceToken = Lens.field @"nextSequenceToken"
{-# INLINEABLE plerrsNextSequenceToken #-}
{-# DEPRECATED nextSequenceToken "Use generic-lens or generic-optics with 'nextSequenceToken' instead"  #-}

-- | The rejected events.
--
-- /Note:/ Consider using 'rejectedLogEventsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plerrsRejectedLogEventsInfo :: Lens.Lens' PutLogEventsResponse (Core.Maybe Types.RejectedLogEventsInfo)
plerrsRejectedLogEventsInfo = Lens.field @"rejectedLogEventsInfo"
{-# INLINEABLE plerrsRejectedLogEventsInfo #-}
{-# DEPRECATED rejectedLogEventsInfo "Use generic-lens or generic-optics with 'rejectedLogEventsInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plerrsResponseStatus :: Lens.Lens' PutLogEventsResponse Core.Int
plerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE plerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
