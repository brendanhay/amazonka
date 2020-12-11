{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of CloudWatch Logs Insights queries that are scheduled, executing, or have been executed recently in this account. You can request all queries or limit it to queries of a specific log group or queries with a certain status.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeQueries
  ( -- * Creating a request
    DescribeQueries (..),
    mkDescribeQueries,

    -- ** Request lenses
    dqStatus,
    dqLogGroupName,
    dqNextToken,
    dqMaxResults,

    -- * Destructuring the response
    DescribeQueriesResponse (..),
    mkDescribeQueriesResponse,

    -- ** Response lenses
    dqrsQueries,
    dqrsNextToken,
    dqrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeQueries' smart constructor.
data DescribeQueries = DescribeQueries'
  { status ::
      Lude.Maybe QueryStatus,
    logGroupName :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeQueries' with the minimum fields required to make a request.
--
-- * 'logGroupName' - Limits the returned queries to only those for the specified log group.
-- * 'maxResults' - Limits the number of returned queries to the specified number.
-- * 'nextToken' - Undocumented field.
-- * 'status' - Limits the returned queries to only those that have the specified status. Valid values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , and @Scheduled@ .
mkDescribeQueries ::
  DescribeQueries
mkDescribeQueries =
  DescribeQueries'
    { status = Lude.Nothing,
      logGroupName = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Limits the returned queries to only those that have the specified status. Valid values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , and @Scheduled@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqStatus :: Lens.Lens' DescribeQueries (Lude.Maybe QueryStatus)
dqStatus = Lens.lens (status :: DescribeQueries -> Lude.Maybe QueryStatus) (\s a -> s {status = a} :: DescribeQueries)
{-# DEPRECATED dqStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Limits the returned queries to only those for the specified log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqLogGroupName :: Lens.Lens' DescribeQueries (Lude.Maybe Lude.Text)
dqLogGroupName = Lens.lens (logGroupName :: DescribeQueries -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: DescribeQueries)
{-# DEPRECATED dqLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqNextToken :: Lens.Lens' DescribeQueries (Lude.Maybe Lude.Text)
dqNextToken = Lens.lens (nextToken :: DescribeQueries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeQueries)
{-# DEPRECATED dqNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Limits the number of returned queries to the specified number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqMaxResults :: Lens.Lens' DescribeQueries (Lude.Maybe Lude.Natural)
dqMaxResults = Lens.lens (maxResults :: DescribeQueries -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeQueries)
{-# DEPRECATED dqMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeQueries where
  page rq rs
    | Page.stop (rs Lens.^. dqrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dqrsQueries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dqNextToken Lens..~ rs Lens.^. dqrsNextToken

instance Lude.AWSRequest DescribeQueries where
  type Rs DescribeQueries = DescribeQueriesResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeQueriesResponse'
            Lude.<$> (x Lude..?> "queries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeQueries where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeQueries" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeQueries where
  toJSON DescribeQueries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("logGroupName" Lude..=) Lude.<$> logGroupName,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeQueries where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeQueries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeQueriesResponse' smart constructor.
data DescribeQueriesResponse = DescribeQueriesResponse'
  { queries ::
      Lude.Maybe [QueryInfo],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeQueriesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - Undocumented field.
-- * 'queries' - The list of queries that match the request.
-- * 'responseStatus' - The response status code.
mkDescribeQueriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeQueriesResponse
mkDescribeQueriesResponse pResponseStatus_ =
  DescribeQueriesResponse'
    { queries = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of queries that match the request.
--
-- /Note:/ Consider using 'queries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrsQueries :: Lens.Lens' DescribeQueriesResponse (Lude.Maybe [QueryInfo])
dqrsQueries = Lens.lens (queries :: DescribeQueriesResponse -> Lude.Maybe [QueryInfo]) (\s a -> s {queries = a} :: DescribeQueriesResponse)
{-# DEPRECATED dqrsQueries "Use generic-lens or generic-optics with 'queries' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrsNextToken :: Lens.Lens' DescribeQueriesResponse (Lude.Maybe Lude.Text)
dqrsNextToken = Lens.lens (nextToken :: DescribeQueriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeQueriesResponse)
{-# DEPRECATED dqrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrsResponseStatus :: Lens.Lens' DescribeQueriesResponse Lude.Int
dqrsResponseStatus = Lens.lens (responseStatus :: DescribeQueriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeQueriesResponse)
{-# DEPRECATED dqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
