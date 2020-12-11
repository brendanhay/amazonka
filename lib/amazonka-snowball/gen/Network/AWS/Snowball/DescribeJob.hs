{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific job including shipping information, job status, and other important metadata.
module Network.AWS.Snowball.DescribeJob
  ( -- * Creating a request
    DescribeJob (..),
    mkDescribeJob,

    -- ** Request lenses
    djJobId,

    -- * Destructuring the response
    DescribeJobResponse (..),
    mkDescribeJobResponse,

    -- ** Response lenses
    djrsJobMetadata,
    djrsSubJobMetadata,
    djrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkDescribeJob' smart constructor.
newtype DescribeJob = DescribeJob' {jobId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
mkDescribeJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeJob
mkDescribeJob pJobId_ = DescribeJob' {jobId = pJobId_}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djJobId :: Lens.Lens' DescribeJob Lude.Text
djJobId = Lens.lens (jobId :: DescribeJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeJob)
{-# DEPRECATED djJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeJob where
  type Rs DescribeJob = DescribeJobResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Lude.<$> (x Lude..?> "JobMetadata")
            Lude.<*> (x Lude..?> "SubJobMetadata" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.DescribeJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeJob where
  toJSON DescribeJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { jobMetadata ::
      Lude.Maybe JobMetadata,
    subJobMetadata :: Lude.Maybe [JobMetadata],
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

-- | Creates a value of 'DescribeJobResponse' with the minimum fields required to make a request.
--
-- * 'jobMetadata' - Information about a specific job, including shipping information, job status, and other important metadata.
-- * 'responseStatus' - The response status code.
-- * 'subJobMetadata' - Information about a specific job part (in the case of an export job), including shipping information, job status, and other important metadata.
mkDescribeJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeJobResponse
mkDescribeJobResponse pResponseStatus_ =
  DescribeJobResponse'
    { jobMetadata = Lude.Nothing,
      subJobMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a specific job, including shipping information, job status, and other important metadata.
--
-- /Note:/ Consider using 'jobMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsJobMetadata :: Lens.Lens' DescribeJobResponse (Lude.Maybe JobMetadata)
djrsJobMetadata = Lens.lens (jobMetadata :: DescribeJobResponse -> Lude.Maybe JobMetadata) (\s a -> s {jobMetadata = a} :: DescribeJobResponse)
{-# DEPRECATED djrsJobMetadata "Use generic-lens or generic-optics with 'jobMetadata' instead." #-}

-- | Information about a specific job part (in the case of an export job), including shipping information, job status, and other important metadata.
--
-- /Note:/ Consider using 'subJobMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsSubJobMetadata :: Lens.Lens' DescribeJobResponse (Lude.Maybe [JobMetadata])
djrsSubJobMetadata = Lens.lens (subJobMetadata :: DescribeJobResponse -> Lude.Maybe [JobMetadata]) (\s a -> s {subJobMetadata = a} :: DescribeJobResponse)
{-# DEPRECATED djrsSubJobMetadata "Use generic-lens or generic-optics with 'subJobMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsResponseStatus :: Lens.Lens' DescribeJobResponse Lude.Int
djrsResponseStatus = Lens.lens (responseStatus :: DescribeJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeJobResponse)
{-# DEPRECATED djrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
