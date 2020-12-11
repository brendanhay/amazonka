{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.StartQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a query of a log group using CloudWatch Logs Insights. You specify the log group and time range to query and the query string to use.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
-- Queries time out after 15 minutes of execution. If your queries are timing out, reduce the time range being searched or partition your query into a number of queries.
module Network.AWS.CloudWatchLogs.StartQuery
  ( -- * Creating a request
    StartQuery (..),
    mkStartQuery,

    -- ** Request lenses
    sqLogGroupNames,
    sqLogGroupName,
    sqLimit,
    sqStartTime,
    sqEndTime,
    sqQueryString,

    -- * Destructuring the response
    StartQueryResponse (..),
    mkStartQueryResponse,

    -- ** Response lenses
    srsQueryId,
    srsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartQuery' smart constructor.
data StartQuery = StartQuery'
  { logGroupNames ::
      Lude.Maybe [Lude.Text],
    logGroupName :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    startTime :: Lude.Natural,
    endTime :: Lude.Natural,
    queryString :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartQuery' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time range to query. The range is inclusive, so the specified end time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
-- * 'limit' - The maximum number of log events to return in the query. If the query string uses the @fields@ command, only the specified fields and their values are returned. The default is 1000.
-- * 'logGroupName' - The log group on which to perform the query.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
-- * 'logGroupNames' - The list of log groups to be queried. You can include up to 20 log groups.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
-- * 'queryString' - The query string to use. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
-- * 'startTime' - The beginning of the time range to query. The range is inclusive, so the specified start time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
mkStartQuery ::
  -- | 'startTime'
  Lude.Natural ->
  -- | 'endTime'
  Lude.Natural ->
  -- | 'queryString'
  Lude.Text ->
  StartQuery
mkStartQuery pStartTime_ pEndTime_ pQueryString_ =
  StartQuery'
    { logGroupNames = Lude.Nothing,
      logGroupName = Lude.Nothing,
      limit = Lude.Nothing,
      startTime = pStartTime_,
      endTime = pEndTime_,
      queryString = pQueryString_
    }

-- | The list of log groups to be queried. You can include up to 20 log groups.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
--
-- /Note:/ Consider using 'logGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqLogGroupNames :: Lens.Lens' StartQuery (Lude.Maybe [Lude.Text])
sqLogGroupNames = Lens.lens (logGroupNames :: StartQuery -> Lude.Maybe [Lude.Text]) (\s a -> s {logGroupNames = a} :: StartQuery)
{-# DEPRECATED sqLogGroupNames "Use generic-lens or generic-optics with 'logGroupNames' instead." #-}

-- | The log group on which to perform the query.
--
-- A @StartQuery@ operation must include a @logGroupNames@ or a @logGroupName@ parameter, but not both.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqLogGroupName :: Lens.Lens' StartQuery (Lude.Maybe Lude.Text)
sqLogGroupName = Lens.lens (logGroupName :: StartQuery -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: StartQuery)
{-# DEPRECATED sqLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The maximum number of log events to return in the query. If the query string uses the @fields@ command, only the specified fields and their values are returned. The default is 1000.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqLimit :: Lens.Lens' StartQuery (Lude.Maybe Lude.Natural)
sqLimit = Lens.lens (limit :: StartQuery -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: StartQuery)
{-# DEPRECATED sqLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The beginning of the time range to query. The range is inclusive, so the specified start time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqStartTime :: Lens.Lens' StartQuery Lude.Natural
sqStartTime = Lens.lens (startTime :: StartQuery -> Lude.Natural) (\s a -> s {startTime = a} :: StartQuery)
{-# DEPRECATED sqStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time range to query. The range is inclusive, so the specified end time is included in the query. Specified as epoch time, the number of seconds since January 1, 1970, 00:00:00 UTC.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqEndTime :: Lens.Lens' StartQuery Lude.Natural
sqEndTime = Lens.lens (endTime :: StartQuery -> Lude.Natural) (\s a -> s {endTime = a} :: StartQuery)
{-# DEPRECATED sqEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The query string to use. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqQueryString :: Lens.Lens' StartQuery Lude.Text
sqQueryString = Lens.lens (queryString :: StartQuery -> Lude.Text) (\s a -> s {queryString = a} :: StartQuery)
{-# DEPRECATED sqQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

instance Lude.AWSRequest StartQuery where
  type Rs StartQuery = StartQueryResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartQueryResponse'
            Lude.<$> (x Lude..?> "queryId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartQuery where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.StartQuery" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartQuery where
  toJSON StartQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("logGroupNames" Lude..=) Lude.<$> logGroupNames,
            ("logGroupName" Lude..=) Lude.<$> logGroupName,
            ("limit" Lude..=) Lude.<$> limit,
            Lude.Just ("startTime" Lude..= startTime),
            Lude.Just ("endTime" Lude..= endTime),
            Lude.Just ("queryString" Lude..= queryString)
          ]
      )

instance Lude.ToPath StartQuery where
  toPath = Lude.const "/"

instance Lude.ToQuery StartQuery where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartQueryResponse' smart constructor.
data StartQueryResponse = StartQueryResponse'
  { queryId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StartQueryResponse' with the minimum fields required to make a request.
--
-- * 'queryId' - The unique ID of the query.
-- * 'responseStatus' - The response status code.
mkStartQueryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartQueryResponse
mkStartQueryResponse pResponseStatus_ =
  StartQueryResponse'
    { queryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of the query.
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsQueryId :: Lens.Lens' StartQueryResponse (Lude.Maybe Lude.Text)
srsQueryId = Lens.lens (queryId :: StartQueryResponse -> Lude.Maybe Lude.Text) (\s a -> s {queryId = a} :: StartQueryResponse)
{-# DEPRECATED srsQueryId "Use generic-lens or generic-optics with 'queryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartQueryResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartQueryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartQueryResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
