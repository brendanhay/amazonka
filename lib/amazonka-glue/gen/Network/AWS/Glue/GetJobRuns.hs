{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all runs of a given job definition.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobRuns
  ( -- * Creating a request
    GetJobRuns (..),
    mkGetJobRuns,

    -- ** Request lenses
    gjrJobName,
    gjrNextToken,
    gjrMaxResults,

    -- * Destructuring the response
    GetJobRunsResponse (..),
    mkGetJobRunsResponse,

    -- ** Response lenses
    gjrrsNextToken,
    gjrrsJobRuns,
    gjrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJobRuns' smart constructor.
data GetJobRuns = GetJobRuns'
  { -- | The name of the job definition for which to retrieve all job runs.
    jobName :: Lude.Text,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of the response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobRuns' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition for which to retrieve all job runs.
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'maxResults' - The maximum size of the response.
mkGetJobRuns ::
  -- | 'jobName'
  Lude.Text ->
  GetJobRuns
mkGetJobRuns pJobName_ =
  GetJobRuns'
    { jobName = pJobName_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The name of the job definition for which to retrieve all job runs.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrJobName :: Lens.Lens' GetJobRuns Lude.Text
gjrJobName = Lens.lens (jobName :: GetJobRuns -> Lude.Text) (\s a -> s {jobName = a} :: GetJobRuns)
{-# DEPRECATED gjrJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrNextToken :: Lens.Lens' GetJobRuns (Lude.Maybe Lude.Text)
gjrNextToken = Lens.lens (nextToken :: GetJobRuns -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetJobRuns)
{-# DEPRECATED gjrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrMaxResults :: Lens.Lens' GetJobRuns (Lude.Maybe Lude.Natural)
gjrMaxResults = Lens.lens (maxResults :: GetJobRuns -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetJobRuns)
{-# DEPRECATED gjrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetJobRuns where
  page rq rs
    | Page.stop (rs Lens.^. gjrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gjrrsJobRuns) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gjrNextToken Lens..~ rs Lens.^. gjrrsNextToken

instance Lude.AWSRequest GetJobRuns where
  type Rs GetJobRuns = GetJobRunsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobRunsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "JobRuns" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetJobRuns" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJobRuns where
  toJSON GetJobRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobName" Lude..= jobName),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetJobRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJobRuns where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobRunsResponse' smart constructor.
data GetJobRunsResponse = GetJobRunsResponse'
  { -- | A continuation token, if not all requested job runs have been returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of job-run metadata objects.
    jobRuns :: Lude.Maybe [JobRun],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobRunsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if not all requested job runs have been returned.
-- * 'jobRuns' - A list of job-run metadata objects.
-- * 'responseStatus' - The response status code.
mkGetJobRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobRunsResponse
mkGetJobRunsResponse pResponseStatus_ =
  GetJobRunsResponse'
    { nextToken = Lude.Nothing,
      jobRuns = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if not all requested job runs have been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrsNextToken :: Lens.Lens' GetJobRunsResponse (Lude.Maybe Lude.Text)
gjrrsNextToken = Lens.lens (nextToken :: GetJobRunsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetJobRunsResponse)
{-# DEPRECATED gjrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of job-run metadata objects.
--
-- /Note:/ Consider using 'jobRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrsJobRuns :: Lens.Lens' GetJobRunsResponse (Lude.Maybe [JobRun])
gjrrsJobRuns = Lens.lens (jobRuns :: GetJobRunsResponse -> Lude.Maybe [JobRun]) (\s a -> s {jobRuns = a} :: GetJobRunsResponse)
{-# DEPRECATED gjrrsJobRuns "Use generic-lens or generic-optics with 'jobRuns' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrrsResponseStatus :: Lens.Lens' GetJobRunsResponse Lude.Int
gjrrsResponseStatus = Lens.lens (responseStatus :: GetJobRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobRunsResponse)
{-# DEPRECATED gjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
