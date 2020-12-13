{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the log streams for the specified log group. You can list all the log streams or filter the results by prefix. You can also control how the results are ordered.
--
-- This operation has a limit of five transactions per second, after which transactions are throttled.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogStreams
  ( -- * Creating a request
    DescribeLogStreams (..),
    mkDescribeLogStreams,

    -- ** Request lenses
    dlssOrderBy,
    dlssDescending,
    dlssLogGroupName,
    dlssNextToken,
    dlssLogStreamNamePrefix,
    dlssLimit,

    -- * Destructuring the response
    DescribeLogStreamsResponse (..),
    mkDescribeLogStreamsResponse,

    -- ** Response lenses
    dlsrsNextToken,
    dlsrsLogStreams,
    dlsrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLogStreams' smart constructor.
data DescribeLogStreams = DescribeLogStreams'
  { -- | If the value is @LogStreamName@ , the results are ordered by log stream name. If the value is @LastEventTime@ , the results are ordered by the event time. The default value is @LogStreamName@ .
    --
    -- If you order the results by event time, you cannot specify the @logStreamNamePrefix@ parameter.
    -- @lastEventTimeStamp@ represents the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. @lastEventTimeStamp@ updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
    orderBy :: Lude.Maybe OrderBy,
    -- | If the value is true, results are returned in descending order. If the value is to false, results are returned in ascending order. The default value is false.
    descending :: Lude.Maybe Lude.Bool,
    -- | The name of the log group.
    logGroupName :: Lude.Text,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Lude.Maybe Lude.Text,
    -- | The prefix to match.
    --
    -- If @orderBy@ is @LastEventTime@ , you cannot specify this parameter.
    logStreamNamePrefix :: Lude.Maybe Lude.Text,
    -- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLogStreams' with the minimum fields required to make a request.
--
-- * 'orderBy' - If the value is @LogStreamName@ , the results are ordered by log stream name. If the value is @LastEventTime@ , the results are ordered by the event time. The default value is @LogStreamName@ .
--
-- If you order the results by event time, you cannot specify the @logStreamNamePrefix@ parameter.
-- @lastEventTimeStamp@ represents the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. @lastEventTimeStamp@ updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
-- * 'descending' - If the value is true, results are returned in descending order. If the value is to false, results are returned in ascending order. The default value is false.
-- * 'logGroupName' - The name of the log group.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'logStreamNamePrefix' - The prefix to match.
--
-- If @orderBy@ is @LastEventTime@ , you cannot specify this parameter.
-- * 'limit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
mkDescribeLogStreams ::
  -- | 'logGroupName'
  Lude.Text ->
  DescribeLogStreams
mkDescribeLogStreams pLogGroupName_ =
  DescribeLogStreams'
    { orderBy = Lude.Nothing,
      descending = Lude.Nothing,
      logGroupName = pLogGroupName_,
      nextToken = Lude.Nothing,
      logStreamNamePrefix = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | If the value is @LogStreamName@ , the results are ordered by log stream name. If the value is @LastEventTime@ , the results are ordered by the event time. The default value is @LogStreamName@ .
--
-- If you order the results by event time, you cannot specify the @logStreamNamePrefix@ parameter.
-- @lastEventTimeStamp@ represents the time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. @lastEventTimeStamp@ updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
--
-- /Note:/ Consider using 'orderBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssOrderBy :: Lens.Lens' DescribeLogStreams (Lude.Maybe OrderBy)
dlssOrderBy = Lens.lens (orderBy :: DescribeLogStreams -> Lude.Maybe OrderBy) (\s a -> s {orderBy = a} :: DescribeLogStreams)
{-# DEPRECATED dlssOrderBy "Use generic-lens or generic-optics with 'orderBy' instead." #-}

-- | If the value is true, results are returned in descending order. If the value is to false, results are returned in ascending order. The default value is false.
--
-- /Note:/ Consider using 'descending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssDescending :: Lens.Lens' DescribeLogStreams (Lude.Maybe Lude.Bool)
dlssDescending = Lens.lens (descending :: DescribeLogStreams -> Lude.Maybe Lude.Bool) (\s a -> s {descending = a} :: DescribeLogStreams)
{-# DEPRECATED dlssDescending "Use generic-lens or generic-optics with 'descending' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssLogGroupName :: Lens.Lens' DescribeLogStreams Lude.Text
dlssLogGroupName = Lens.lens (logGroupName :: DescribeLogStreams -> Lude.Text) (\s a -> s {logGroupName = a} :: DescribeLogStreams)
{-# DEPRECATED dlssLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssNextToken :: Lens.Lens' DescribeLogStreams (Lude.Maybe Lude.Text)
dlssNextToken = Lens.lens (nextToken :: DescribeLogStreams -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLogStreams)
{-# DEPRECATED dlssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The prefix to match.
--
-- If @orderBy@ is @LastEventTime@ , you cannot specify this parameter.
--
-- /Note:/ Consider using 'logStreamNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssLogStreamNamePrefix :: Lens.Lens' DescribeLogStreams (Lude.Maybe Lude.Text)
dlssLogStreamNamePrefix = Lens.lens (logStreamNamePrefix :: DescribeLogStreams -> Lude.Maybe Lude.Text) (\s a -> s {logStreamNamePrefix = a} :: DescribeLogStreams)
{-# DEPRECATED dlssLogStreamNamePrefix "Use generic-lens or generic-optics with 'logStreamNamePrefix' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlssLimit :: Lens.Lens' DescribeLogStreams (Lude.Maybe Lude.Natural)
dlssLimit = Lens.lens (limit :: DescribeLogStreams -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeLogStreams)
{-# DEPRECATED dlssLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeLogStreams where
  page rq rs
    | Page.stop (rs Lens.^. dlsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlsrsLogStreams) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlssNextToken Lens..~ rs Lens.^. dlsrsNextToken

instance Lude.AWSRequest DescribeLogStreams where
  type Rs DescribeLogStreams = DescribeLogStreamsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLogStreamsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "logStreams" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLogStreams where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeLogStreams" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLogStreams where
  toJSON DescribeLogStreams' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("orderBy" Lude..=) Lude.<$> orderBy,
            ("descending" Lude..=) Lude.<$> descending,
            Lude.Just ("logGroupName" Lude..= logGroupName),
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("logStreamNamePrefix" Lude..=) Lude.<$> logStreamNamePrefix,
            ("limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeLogStreams where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLogStreams where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLogStreamsResponse' smart constructor.
data DescribeLogStreamsResponse = DescribeLogStreamsResponse'
  { nextToken :: Lude.Maybe Lude.Text,
    -- | The log streams.
    logStreams :: Lude.Maybe [LogStream],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLogStreamsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'logStreams' - The log streams.
-- * 'responseStatus' - The response status code.
mkDescribeLogStreamsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLogStreamsResponse
mkDescribeLogStreamsResponse pResponseStatus_ =
  DescribeLogStreamsResponse'
    { nextToken = Lude.Nothing,
      logStreams = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrsNextToken :: Lens.Lens' DescribeLogStreamsResponse (Lude.Maybe Lude.Text)
dlsrsNextToken = Lens.lens (nextToken :: DescribeLogStreamsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLogStreamsResponse)
{-# DEPRECATED dlsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The log streams.
--
-- /Note:/ Consider using 'logStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrsLogStreams :: Lens.Lens' DescribeLogStreamsResponse (Lude.Maybe [LogStream])
dlsrsLogStreams = Lens.lens (logStreams :: DescribeLogStreamsResponse -> Lude.Maybe [LogStream]) (\s a -> s {logStreams = a} :: DescribeLogStreamsResponse)
{-# DEPRECATED dlsrsLogStreams "Use generic-lens or generic-optics with 'logStreams' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsrsResponseStatus :: Lens.Lens' DescribeLogStreamsResponse Lude.Int
dlsrsResponseStatus = Lens.lens (responseStatus :: DescribeLogStreamsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLogStreamsResponse)
{-# DEPRECATED dlsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
