{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutLogEvents (..),
    mkPutLogEvents,

    -- ** Request lenses
    pleSequenceToken,
    pleLogGroupName,
    pleLogStreamName,
    pleLogEvents,

    -- * Destructuring the response
    PutLogEventsResponse (..),
    mkPutLogEventsResponse,

    -- ** Response lenses
    plersRejectedLogEventsInfo,
    plersNextSequenceToken,
    plersResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLogEvents' smart constructor.
data PutLogEvents = PutLogEvents'
  { sequenceToken ::
      Lude.Maybe Lude.Text,
    logGroupName :: Lude.Text,
    logStreamName :: Lude.Text,
    logEvents :: Lude.NonEmpty InputLogEvent
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLogEvents' with the minimum fields required to make a request.
--
-- * 'logEvents' - The log events.
-- * 'logGroupName' - The name of the log group.
-- * 'logStreamName' - The name of the log stream.
-- * 'sequenceToken' - The sequence token obtained from the response of the previous @PutLogEvents@ call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token using <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html DescribeLogStreams> . If you call @PutLogEvents@ twice within a narrow time period using the same value for @sequenceToken@ , both calls might be successful or one might be rejected.
mkPutLogEvents ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'logStreamName'
  Lude.Text ->
  -- | 'logEvents'
  Lude.NonEmpty InputLogEvent ->
  PutLogEvents
mkPutLogEvents pLogGroupName_ pLogStreamName_ pLogEvents_ =
  PutLogEvents'
    { sequenceToken = Lude.Nothing,
      logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_,
      logEvents = pLogEvents_
    }

-- | The sequence token obtained from the response of the previous @PutLogEvents@ call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token using <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html DescribeLogStreams> . If you call @PutLogEvents@ twice within a narrow time period using the same value for @sequenceToken@ , both calls might be successful or one might be rejected.
--
-- /Note:/ Consider using 'sequenceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleSequenceToken :: Lens.Lens' PutLogEvents (Lude.Maybe Lude.Text)
pleSequenceToken = Lens.lens (sequenceToken :: PutLogEvents -> Lude.Maybe Lude.Text) (\s a -> s {sequenceToken = a} :: PutLogEvents)
{-# DEPRECATED pleSequenceToken "Use generic-lens or generic-optics with 'sequenceToken' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleLogGroupName :: Lens.Lens' PutLogEvents Lude.Text
pleLogGroupName = Lens.lens (logGroupName :: PutLogEvents -> Lude.Text) (\s a -> s {logGroupName = a} :: PutLogEvents)
{-# DEPRECATED pleLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleLogStreamName :: Lens.Lens' PutLogEvents Lude.Text
pleLogStreamName = Lens.lens (logStreamName :: PutLogEvents -> Lude.Text) (\s a -> s {logStreamName = a} :: PutLogEvents)
{-# DEPRECATED pleLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The log events.
--
-- /Note:/ Consider using 'logEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pleLogEvents :: Lens.Lens' PutLogEvents (Lude.NonEmpty InputLogEvent)
pleLogEvents = Lens.lens (logEvents :: PutLogEvents -> Lude.NonEmpty InputLogEvent) (\s a -> s {logEvents = a} :: PutLogEvents)
{-# DEPRECATED pleLogEvents "Use generic-lens or generic-optics with 'logEvents' instead." #-}

instance Lude.AWSRequest PutLogEvents where
  type Rs PutLogEvents = PutLogEventsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutLogEventsResponse'
            Lude.<$> (x Lude..?> "rejectedLogEventsInfo")
            Lude.<*> (x Lude..?> "nextSequenceToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutLogEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutLogEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutLogEvents where
  toJSON PutLogEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sequenceToken" Lude..=) Lude.<$> sequenceToken,
            Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("logStreamName" Lude..= logStreamName),
            Lude.Just ("logEvents" Lude..= logEvents)
          ]
      )

instance Lude.ToPath PutLogEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery PutLogEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutLogEventsResponse' smart constructor.
data PutLogEventsResponse = PutLogEventsResponse'
  { rejectedLogEventsInfo ::
      Lude.Maybe RejectedLogEventsInfo,
    nextSequenceToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLogEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextSequenceToken' - The next sequence token.
-- * 'rejectedLogEventsInfo' - The rejected events.
-- * 'responseStatus' - The response status code.
mkPutLogEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutLogEventsResponse
mkPutLogEventsResponse pResponseStatus_ =
  PutLogEventsResponse'
    { rejectedLogEventsInfo = Lude.Nothing,
      nextSequenceToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The rejected events.
--
-- /Note:/ Consider using 'rejectedLogEventsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plersRejectedLogEventsInfo :: Lens.Lens' PutLogEventsResponse (Lude.Maybe RejectedLogEventsInfo)
plersRejectedLogEventsInfo = Lens.lens (rejectedLogEventsInfo :: PutLogEventsResponse -> Lude.Maybe RejectedLogEventsInfo) (\s a -> s {rejectedLogEventsInfo = a} :: PutLogEventsResponse)
{-# DEPRECATED plersRejectedLogEventsInfo "Use generic-lens or generic-optics with 'rejectedLogEventsInfo' instead." #-}

-- | The next sequence token.
--
-- /Note:/ Consider using 'nextSequenceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plersNextSequenceToken :: Lens.Lens' PutLogEventsResponse (Lude.Maybe Lude.Text)
plersNextSequenceToken = Lens.lens (nextSequenceToken :: PutLogEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextSequenceToken = a} :: PutLogEventsResponse)
{-# DEPRECATED plersNextSequenceToken "Use generic-lens or generic-optics with 'nextSequenceToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plersResponseStatus :: Lens.Lens' PutLogEventsResponse Lude.Int
plersResponseStatus = Lens.lens (responseStatus :: PutLogEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutLogEventsResponse)
{-# DEPRECATED plersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
