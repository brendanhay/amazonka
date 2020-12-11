{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetReplicationRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the replication runs for the specified replication job.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetReplicationRuns
  ( -- * Creating a request
    GetReplicationRuns (..),
    mkGetReplicationRuns,

    -- ** Request lenses
    grrNextToken,
    grrMaxResults,
    grrReplicationJobId,

    -- * Destructuring the response
    GetReplicationRunsResponse (..),
    mkGetReplicationRunsResponse,

    -- ** Response lenses
    grrrsReplicationJob,
    grrrsNextToken,
    grrrsReplicationRunList,
    grrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetReplicationRuns' smart constructor.
data GetReplicationRuns = GetReplicationRuns'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Int,
    replicationJobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReplicationRuns' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token for the next set of results.
-- * 'replicationJobId' - The ID of the replication job.
mkGetReplicationRuns ::
  -- | 'replicationJobId'
  Lude.Text ->
  GetReplicationRuns
mkGetReplicationRuns pReplicationJobId_ =
  GetReplicationRuns'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      replicationJobId = pReplicationJobId_
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrNextToken :: Lens.Lens' GetReplicationRuns (Lude.Maybe Lude.Text)
grrNextToken = Lens.lens (nextToken :: GetReplicationRuns -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetReplicationRuns)
{-# DEPRECATED grrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrMaxResults :: Lens.Lens' GetReplicationRuns (Lude.Maybe Lude.Int)
grrMaxResults = Lens.lens (maxResults :: GetReplicationRuns -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetReplicationRuns)
{-# DEPRECATED grrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrReplicationJobId :: Lens.Lens' GetReplicationRuns Lude.Text
grrReplicationJobId = Lens.lens (replicationJobId :: GetReplicationRuns -> Lude.Text) (\s a -> s {replicationJobId = a} :: GetReplicationRuns)
{-# DEPRECATED grrReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

instance Page.AWSPager GetReplicationRuns where
  page rq rs
    | Page.stop (rs Lens.^. grrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grrrsReplicationRunList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grrNextToken Lens..~ rs Lens.^. grrrsNextToken

instance Lude.AWSRequest GetReplicationRuns where
  type Rs GetReplicationRuns = GetReplicationRunsResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetReplicationRunsResponse'
            Lude.<$> (x Lude..?> "replicationJob")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "replicationRunList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReplicationRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetReplicationRuns" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetReplicationRuns where
  toJSON GetReplicationRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("replicationJobId" Lude..= replicationJobId)
          ]
      )

instance Lude.ToPath GetReplicationRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReplicationRuns where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetReplicationRunsResponse' smart constructor.
data GetReplicationRunsResponse = GetReplicationRunsResponse'
  { replicationJob ::
      Lude.Maybe ReplicationJob,
    nextToken :: Lude.Maybe Lude.Text,
    replicationRunList ::
      Lude.Maybe [ReplicationRun],
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

-- | Creates a value of 'GetReplicationRunsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token required to retrieve the next set of results. This value is null when there are no more results to return.
-- * 'replicationJob' - Information about the replication job.
-- * 'replicationRunList' - Information about the replication runs.
-- * 'responseStatus' - The response status code.
mkGetReplicationRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReplicationRunsResponse
mkGetReplicationRunsResponse pResponseStatus_ =
  GetReplicationRunsResponse'
    { replicationJob = Lude.Nothing,
      nextToken = Lude.Nothing,
      replicationRunList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the replication job.
--
-- /Note:/ Consider using 'replicationJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsReplicationJob :: Lens.Lens' GetReplicationRunsResponse (Lude.Maybe ReplicationJob)
grrrsReplicationJob = Lens.lens (replicationJob :: GetReplicationRunsResponse -> Lude.Maybe ReplicationJob) (\s a -> s {replicationJob = a} :: GetReplicationRunsResponse)
{-# DEPRECATED grrrsReplicationJob "Use generic-lens or generic-optics with 'replicationJob' instead." #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsNextToken :: Lens.Lens' GetReplicationRunsResponse (Lude.Maybe Lude.Text)
grrrsNextToken = Lens.lens (nextToken :: GetReplicationRunsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetReplicationRunsResponse)
{-# DEPRECATED grrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the replication runs.
--
-- /Note:/ Consider using 'replicationRunList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsReplicationRunList :: Lens.Lens' GetReplicationRunsResponse (Lude.Maybe [ReplicationRun])
grrrsReplicationRunList = Lens.lens (replicationRunList :: GetReplicationRunsResponse -> Lude.Maybe [ReplicationRun]) (\s a -> s {replicationRunList = a} :: GetReplicationRunsResponse)
{-# DEPRECATED grrrsReplicationRunList "Use generic-lens or generic-optics with 'replicationRunList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrsResponseStatus :: Lens.Lens' GetReplicationRunsResponse Lude.Int
grrrsResponseStatus = Lens.lens (responseStatus :: GetReplicationRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReplicationRunsResponse)
{-# DEPRECATED grrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
