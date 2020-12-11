{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.GetQueryResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results from the specified query.
--
-- Only the fields requested in the query are returned, along with a @@ptr@ field, which is the identifier for the log record. You can use the value of @@ptr@ in a <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_GetLogRecord.html GetLogRecord> operation to get the full log record.
-- @GetQueryResults@ does not start a query execution. To run a query, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_StartQuery.html StartQuery> .
-- If the value of the @Status@ field in the output is @Running@ , this operation returns only partial results. If you see a value of @Scheduled@ or @Running@ for the status, you can retry the operation later to see the final results.
module Network.AWS.CloudWatchLogs.GetQueryResults
  ( -- * Creating a request
    GetQueryResults (..),
    mkGetQueryResults,

    -- ** Request lenses
    gqrQueryId,

    -- * Destructuring the response
    GetQueryResultsResponse (..),
    mkGetQueryResultsResponse,

    -- ** Response lenses
    gqrrsStatus,
    gqrrsResults,
    gqrrsStatistics,
    gqrrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetQueryResults' smart constructor.
newtype GetQueryResults = GetQueryResults' {queryId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQueryResults' with the minimum fields required to make a request.
--
-- * 'queryId' - The ID number of the query.
mkGetQueryResults ::
  -- | 'queryId'
  Lude.Text ->
  GetQueryResults
mkGetQueryResults pQueryId_ = GetQueryResults' {queryId = pQueryId_}

-- | The ID number of the query.
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrQueryId :: Lens.Lens' GetQueryResults Lude.Text
gqrQueryId = Lens.lens (queryId :: GetQueryResults -> Lude.Text) (\s a -> s {queryId = a} :: GetQueryResults)
{-# DEPRECATED gqrQueryId "Use generic-lens or generic-optics with 'queryId' instead." #-}

instance Lude.AWSRequest GetQueryResults where
  type Rs GetQueryResults = GetQueryResultsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetQueryResultsResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "results" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "statistics")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQueryResults where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.GetQueryResults" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetQueryResults where
  toJSON GetQueryResults' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("queryId" Lude..= queryId)])

instance Lude.ToPath GetQueryResults where
  toPath = Lude.const "/"

instance Lude.ToQuery GetQueryResults where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { status ::
      Lude.Maybe QueryStatus,
    results :: Lude.Maybe [[ResultField]],
    statistics :: Lude.Maybe QueryStatistics,
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

-- | Creates a value of 'GetQueryResultsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'results' - The log events that matched the query criteria during the most recent time it ran.
--
-- The @results@ value is an array of arrays. Each log event is one object in the top-level array. Each of these log event objects is an array of @field@ /@value@ pairs.
-- * 'statistics' - Includes the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned. These values reflect the full raw results of the query.
-- * 'status' - The status of the most recent running of the query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , @Timeout@ , and @Unknown@ .
--
-- Queries time out after 15 minutes of execution. To avoid having your queries time out, reduce the time range being searched or partition your query into a number of queries.
mkGetQueryResultsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetQueryResultsResponse
mkGetQueryResultsResponse pResponseStatus_ =
  GetQueryResultsResponse'
    { status = Lude.Nothing,
      results = Lude.Nothing,
      statistics = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the most recent running of the query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , @Timeout@ , and @Unknown@ .
--
-- Queries time out after 15 minutes of execution. To avoid having your queries time out, reduce the time range being searched or partition your query into a number of queries.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrsStatus :: Lens.Lens' GetQueryResultsResponse (Lude.Maybe QueryStatus)
gqrrsStatus = Lens.lens (status :: GetQueryResultsResponse -> Lude.Maybe QueryStatus) (\s a -> s {status = a} :: GetQueryResultsResponse)
{-# DEPRECATED gqrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The log events that matched the query criteria during the most recent time it ran.
--
-- The @results@ value is an array of arrays. Each log event is one object in the top-level array. Each of these log event objects is an array of @field@ /@value@ pairs.
--
-- /Note:/ Consider using 'results' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrsResults :: Lens.Lens' GetQueryResultsResponse (Lude.Maybe [[ResultField]])
gqrrsResults = Lens.lens (results :: GetQueryResultsResponse -> Lude.Maybe [[ResultField]]) (\s a -> s {results = a} :: GetQueryResultsResponse)
{-# DEPRECATED gqrrsResults "Use generic-lens or generic-optics with 'results' instead." #-}

-- | Includes the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned. These values reflect the full raw results of the query.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrsStatistics :: Lens.Lens' GetQueryResultsResponse (Lude.Maybe QueryStatistics)
gqrrsStatistics = Lens.lens (statistics :: GetQueryResultsResponse -> Lude.Maybe QueryStatistics) (\s a -> s {statistics = a} :: GetQueryResultsResponse)
{-# DEPRECATED gqrrsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqrrsResponseStatus :: Lens.Lens' GetQueryResultsResponse Lude.Int
gqrrsResponseStatus = Lens.lens (responseStatus :: GetQueryResultsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQueryResultsResponse)
{-# DEPRECATED gqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
