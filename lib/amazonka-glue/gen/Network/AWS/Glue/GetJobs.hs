{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all current job definitions.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetJobs
  ( -- * Creating a request
    GetJobs (..),
    mkGetJobs,

    -- ** Request lenses
    gjNextToken,
    gjMaxResults,

    -- * Destructuring the response
    GetJobsResponse (..),
    mkGetJobsResponse,

    -- ** Response lenses
    gjsrsNextToken,
    gjsrsJobs,
    gjsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJobs' smart constructor.
data GetJobs = GetJobs'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum size of the response.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobs' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if this is a continuation call.
-- * 'maxResults' - The maximum size of the response.
mkGetJobs ::
  GetJobs
mkGetJobs =
  GetJobs' {nextToken = Lude.Nothing, maxResults = Lude.Nothing}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjNextToken :: Lens.Lens' GetJobs (Lude.Maybe Lude.Text)
gjNextToken = Lens.lens (nextToken :: GetJobs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetJobs)
{-# DEPRECATED gjNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum size of the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjMaxResults :: Lens.Lens' GetJobs (Lude.Maybe Lude.Natural)
gjMaxResults = Lens.lens (maxResults :: GetJobs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetJobs)
{-# DEPRECATED gjMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetJobs where
  page rq rs
    | Page.stop (rs Lens.^. gjsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gjsrsJobs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gjNextToken Lens..~ rs Lens.^. gjsrsNextToken

instance Lude.AWSRequest GetJobs where
  type Rs GetJobs = GetJobsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Jobs" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJobs where
  toJSON GetJobs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath GetJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobsResponse' smart constructor.
data GetJobsResponse = GetJobsResponse'
  { -- | A continuation token, if not all job definitions have yet been returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of job definitions.
    jobs :: Lude.Maybe [Job],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A continuation token, if not all job definitions have yet been returned.
-- * 'jobs' - A list of job definitions.
-- * 'responseStatus' - The response status code.
mkGetJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobsResponse
mkGetJobsResponse pResponseStatus_ =
  GetJobsResponse'
    { nextToken = Lude.Nothing,
      jobs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A continuation token, if not all job definitions have yet been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjsrsNextToken :: Lens.Lens' GetJobsResponse (Lude.Maybe Lude.Text)
gjsrsNextToken = Lens.lens (nextToken :: GetJobsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetJobsResponse)
{-# DEPRECATED gjsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of job definitions.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjsrsJobs :: Lens.Lens' GetJobsResponse (Lude.Maybe [Job])
gjsrsJobs = Lens.lens (jobs :: GetJobsResponse -> Lude.Maybe [Job]) (\s a -> s {jobs = a} :: GetJobsResponse)
{-# DEPRECATED gjsrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjsrsResponseStatus :: Lens.Lens' GetJobsResponse Lude.Int
gjsrsResponseStatus = Lens.lens (responseStatus :: GetJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobsResponse)
{-# DEPRECATED gjsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
