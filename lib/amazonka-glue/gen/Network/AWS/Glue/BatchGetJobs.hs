{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of job names. After calling the @ListJobs@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetJobs
  ( -- * Creating a request
    BatchGetJobs (..),
    mkBatchGetJobs,

    -- ** Request lenses
    bgjJobNames,

    -- * Destructuring the response
    BatchGetJobsResponse (..),
    mkBatchGetJobsResponse,

    -- ** Response lenses
    bgjrsJobs,
    bgjrsJobsNotFound,
    bgjrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchGetJobs' smart constructor.
newtype BatchGetJobs = BatchGetJobs'
  { -- | A list of job names, which might be the names returned from the @ListJobs@ operation.
    jobNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetJobs' with the minimum fields required to make a request.
--
-- * 'jobNames' - A list of job names, which might be the names returned from the @ListJobs@ operation.
mkBatchGetJobs ::
  BatchGetJobs
mkBatchGetJobs = BatchGetJobs' {jobNames = Lude.mempty}

-- | A list of job names, which might be the names returned from the @ListJobs@ operation.
--
-- /Note:/ Consider using 'jobNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjJobNames :: Lens.Lens' BatchGetJobs [Lude.Text]
bgjJobNames = Lens.lens (jobNames :: BatchGetJobs -> [Lude.Text]) (\s a -> s {jobNames = a} :: BatchGetJobs)
{-# DEPRECATED bgjJobNames "Use generic-lens or generic-optics with 'jobNames' instead." #-}

instance Lude.AWSRequest BatchGetJobs where
  type Rs BatchGetJobs = BatchGetJobsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          BatchGetJobsResponse'
            Lude.<$> (x Lude..?> "Jobs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "JobsNotFound" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchGetJobs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.BatchGetJobs" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON BatchGetJobs where
  toJSON BatchGetJobs' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("JobNames" Lude..= jobNames)])

instance Lude.ToPath BatchGetJobs where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchGetJobs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkBatchGetJobsResponse' smart constructor.
data BatchGetJobsResponse = BatchGetJobsResponse'
  { -- | A list of job definitions.
    jobs :: Lude.Maybe [Job],
    -- | A list of names of jobs not found.
    jobsNotFound :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetJobsResponse' with the minimum fields required to make a request.
--
-- * 'jobs' - A list of job definitions.
-- * 'jobsNotFound' - A list of names of jobs not found.
-- * 'responseStatus' - The response status code.
mkBatchGetJobsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchGetJobsResponse
mkBatchGetJobsResponse pResponseStatus_ =
  BatchGetJobsResponse'
    { jobs = Lude.Nothing,
      jobsNotFound = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of job definitions.
--
-- /Note:/ Consider using 'jobs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjrsJobs :: Lens.Lens' BatchGetJobsResponse (Lude.Maybe [Job])
bgjrsJobs = Lens.lens (jobs :: BatchGetJobsResponse -> Lude.Maybe [Job]) (\s a -> s {jobs = a} :: BatchGetJobsResponse)
{-# DEPRECATED bgjrsJobs "Use generic-lens or generic-optics with 'jobs' instead." #-}

-- | A list of names of jobs not found.
--
-- /Note:/ Consider using 'jobsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjrsJobsNotFound :: Lens.Lens' BatchGetJobsResponse (Lude.Maybe [Lude.Text])
bgjrsJobsNotFound = Lens.lens (jobsNotFound :: BatchGetJobsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {jobsNotFound = a} :: BatchGetJobsResponse)
{-# DEPRECATED bgjrsJobsNotFound "Use generic-lens or generic-optics with 'jobsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgjrsResponseStatus :: Lens.Lens' BatchGetJobsResponse Lude.Int
bgjrsResponseStatus = Lens.lens (responseStatus :: BatchGetJobsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchGetJobsResponse)
{-# DEPRECATED bgjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
