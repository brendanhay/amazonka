{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a job.
module Network.AWS.IoT.DescribeJob
  ( -- * Creating a request
    DescribeJob (..),
    mkDescribeJob,

    -- ** Request lenses
    dJobId,

    -- * Destructuring the response
    DescribeJobResponse (..),
    mkDescribeJobResponse,

    -- ** Response lenses
    djrsDocumentSource,
    djrsJob,
    djrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeJob' smart constructor.
newtype DescribeJob = DescribeJob'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
mkDescribeJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeJob
mkDescribeJob pJobId_ = DescribeJob' {jobId = pJobId_}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dJobId :: Lens.Lens' DescribeJob Lude.Text
dJobId = Lens.lens (jobId :: DescribeJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeJob)
{-# DEPRECATED dJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeJob where
  type Rs DescribeJob = DescribeJobResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Lude.<$> (x Lude..?> "documentSource")
            Lude.<*> (x Lude..?> "job")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeJob where
  toPath DescribeJob' {..} = Lude.mconcat ["/jobs/", Lude.toBS jobId]

instance Lude.ToQuery DescribeJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { -- | An S3 link to the job document.
    documentSource :: Lude.Maybe Lude.Text,
    -- | Information about the job.
    job :: Lude.Maybe Job,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeJobResponse' with the minimum fields required to make a request.
--
-- * 'documentSource' - An S3 link to the job document.
-- * 'job' - Information about the job.
-- * 'responseStatus' - The response status code.
mkDescribeJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeJobResponse
mkDescribeJobResponse pResponseStatus_ =
  DescribeJobResponse'
    { documentSource = Lude.Nothing,
      job = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An S3 link to the job document.
--
-- /Note:/ Consider using 'documentSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsDocumentSource :: Lens.Lens' DescribeJobResponse (Lude.Maybe Lude.Text)
djrsDocumentSource = Lens.lens (documentSource :: DescribeJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {documentSource = a} :: DescribeJobResponse)
{-# DEPRECATED djrsDocumentSource "Use generic-lens or generic-optics with 'documentSource' instead." #-}

-- | Information about the job.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsJob :: Lens.Lens' DescribeJobResponse (Lude.Maybe Job)
djrsJob = Lens.lens (job :: DescribeJobResponse -> Lude.Maybe Job) (\s a -> s {job = a} :: DescribeJobResponse)
{-# DEPRECATED djrsJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
djrsResponseStatus :: Lens.Lens' DescribeJobResponse Lude.Int
djrsResponseStatus = Lens.lens (responseStatus :: DescribeJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeJobResponse)
{-# DEPRECATED djrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
