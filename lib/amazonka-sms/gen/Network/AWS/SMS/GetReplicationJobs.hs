{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetReplicationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified replication job or all of your replication jobs.
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetReplicationJobs
  ( -- * Creating a request
    GetReplicationJobs (..),
    mkGetReplicationJobs,

    -- ** Request lenses
    grjReplicationJobId,
    grjNextToken,
    grjMaxResults,

    -- * Destructuring the response
    GetReplicationJobsResponse (..),
    mkGetReplicationJobsResponse,

    -- ** Response lenses
    grjrsReplicationJobList,
    grjrsNextToken,
    grjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetReplicationJobs' smart constructor.
data GetReplicationJobs = GetReplicationJobs'
  { -- | The ID of the replication job.
    replicationJobId :: Lude.Maybe Lude.Text,
    -- | The token for the next set of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReplicationJobs' with the minimum fields required to make a request.
--
-- * 'replicationJobId' - The ID of the replication job.
-- * 'nextToken' - The token for the next set of results.
-- * 'maxResults' - The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
mkGetReplicationJobs ::
  GetReplicationJobs
mkGetReplicationJobs =
  GetReplicationJobs'
    { replicationJobId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the replication job.
--
-- /Note:/ Consider using 'replicationJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grjReplicationJobId :: Lens.Lens' GetReplicationJobs (Lude.Maybe Lude.Text)
grjReplicationJobId = Lens.lens (replicationJobId :: GetReplicationJobs -> Lude.Maybe Lude.Text) (\s a -> s {replicationJobId = a} :: GetReplicationJobs)
{-# DEPRECATED grjReplicationJobId "Use generic-lens or generic-optics with 'replicationJobId' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grjNextToken :: Lens.Lens' GetReplicationJobs (Lude.Maybe Lude.Text)
grjNextToken = Lens.lens (nextToken :: GetReplicationJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetReplicationJobs)
{-# DEPRECATED grjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grjMaxResults :: Lens.Lens' GetReplicationJobs (Lude.Maybe Lude.Int)
grjMaxResults = Lens.lens (maxResults :: GetReplicationJobs -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: GetReplicationJobs)
{-# DEPRECATED grjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetReplicationJobs where
  page rq rs
    | Page.stop (rs Lens.^. grjrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. grjrsReplicationJobList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& grjNextToken Lens..~ rs Lens.^. grjrsNextToken

instance Lude.AWSRequest GetReplicationJobs where
  type Rs GetReplicationJobs = GetReplicationJobsResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetReplicationJobsResponse'
            Lude.<$> (x Lude..?> "replicationJobList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetReplicationJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetReplicationJobs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetReplicationJobs where
  toJSON GetReplicationJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("replicationJobId" Lude..=) Lude.<$> replicationJobId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetReplicationJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery GetReplicationJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetReplicationJobsResponse' smart constructor.
data GetReplicationJobsResponse = GetReplicationJobsResponse'
  { -- | Information about the replication jobs.
    replicationJobList :: Lude.Maybe [ReplicationJob],
    -- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReplicationJobsResponse' with the minimum fields required to make a request.
--
-- * 'replicationJobList' - Information about the replication jobs.
-- * 'nextToken' - The token required to retrieve the next set of results. This value is null when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkGetReplicationJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetReplicationJobsResponse
mkGetReplicationJobsResponse pResponseStatus_ =
  GetReplicationJobsResponse'
    { replicationJobList = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the replication jobs.
--
-- /Note:/ Consider using 'replicationJobList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grjrsReplicationJobList :: Lens.Lens' GetReplicationJobsResponse (Lude.Maybe [ReplicationJob])
grjrsReplicationJobList = Lens.lens (replicationJobList :: GetReplicationJobsResponse -> Lude.Maybe [ReplicationJob]) (\s a -> s {replicationJobList = a} :: GetReplicationJobsResponse)
{-# DEPRECATED grjrsReplicationJobList "Use generic-lens or generic-optics with 'replicationJobList' instead." #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grjrsNextToken :: Lens.Lens' GetReplicationJobsResponse (Lude.Maybe Lude.Text)
grjrsNextToken = Lens.lens (nextToken :: GetReplicationJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetReplicationJobsResponse)
{-# DEPRECATED grjrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grjrsResponseStatus :: Lens.Lens' GetReplicationJobsResponse Lude.Int
grjrsResponseStatus = Lens.lens (responseStatus :: GetReplicationJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReplicationJobsResponse)
{-# DEPRECATED grjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
