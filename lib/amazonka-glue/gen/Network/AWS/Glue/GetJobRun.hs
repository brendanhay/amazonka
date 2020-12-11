{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJobRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given job run.
module Network.AWS.Glue.GetJobRun
  ( -- * Creating a request
    GetJobRun (..),
    mkGetJobRun,

    -- ** Request lenses
    gPredecessorsIncluded,
    gJobName,
    gRunId,

    -- * Destructuring the response
    GetJobRunResponse (..),
    mkGetJobRunResponse,

    -- ** Response lenses
    gjrjrsJobRun,
    gjrjrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJobRun' smart constructor.
data GetJobRun = GetJobRun'
  { predecessorsIncluded ::
      Lude.Maybe Lude.Bool,
    jobName :: Lude.Text,
    runId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobRun' with the minimum fields required to make a request.
--
-- * 'jobName' - Name of the job definition being run.
-- * 'predecessorsIncluded' - True if a list of predecessor runs should be returned.
-- * 'runId' - The ID of the job run.
mkGetJobRun ::
  -- | 'jobName'
  Lude.Text ->
  -- | 'runId'
  Lude.Text ->
  GetJobRun
mkGetJobRun pJobName_ pRunId_ =
  GetJobRun'
    { predecessorsIncluded = Lude.Nothing,
      jobName = pJobName_,
      runId = pRunId_
    }

-- | True if a list of predecessor runs should be returned.
--
-- /Note:/ Consider using 'predecessorsIncluded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPredecessorsIncluded :: Lens.Lens' GetJobRun (Lude.Maybe Lude.Bool)
gPredecessorsIncluded = Lens.lens (predecessorsIncluded :: GetJobRun -> Lude.Maybe Lude.Bool) (\s a -> s {predecessorsIncluded = a} :: GetJobRun)
{-# DEPRECATED gPredecessorsIncluded "Use generic-lens or generic-optics with 'predecessorsIncluded' instead." #-}

-- | Name of the job definition being run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gJobName :: Lens.Lens' GetJobRun Lude.Text
gJobName = Lens.lens (jobName :: GetJobRun -> Lude.Text) (\s a -> s {jobName = a} :: GetJobRun)
{-# DEPRECATED gJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The ID of the job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gRunId :: Lens.Lens' GetJobRun Lude.Text
gRunId = Lens.lens (runId :: GetJobRun -> Lude.Text) (\s a -> s {runId = a} :: GetJobRun)
{-# DEPRECATED gRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.AWSRequest GetJobRun where
  type Rs GetJobRun = GetJobRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobRunResponse'
            Lude.<$> (x Lude..?> "JobRun") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetJobRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJobRun where
  toJSON GetJobRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PredecessorsIncluded" Lude..=) Lude.<$> predecessorsIncluded,
            Lude.Just ("JobName" Lude..= jobName),
            Lude.Just ("RunId" Lude..= runId)
          ]
      )

instance Lude.ToPath GetJobRun where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJobRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobRunResponse' smart constructor.
data GetJobRunResponse = GetJobRunResponse'
  { jobRun ::
      Lude.Maybe JobRun,
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

-- | Creates a value of 'GetJobRunResponse' with the minimum fields required to make a request.
--
-- * 'jobRun' - The requested job-run metadata.
-- * 'responseStatus' - The response status code.
mkGetJobRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobRunResponse
mkGetJobRunResponse pResponseStatus_ =
  GetJobRunResponse'
    { jobRun = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested job-run metadata.
--
-- /Note:/ Consider using 'jobRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrjrsJobRun :: Lens.Lens' GetJobRunResponse (Lude.Maybe JobRun)
gjrjrsJobRun = Lens.lens (jobRun :: GetJobRunResponse -> Lude.Maybe JobRun) (\s a -> s {jobRun = a} :: GetJobRunResponse)
{-# DEPRECATED gjrjrsJobRun "Use generic-lens or generic-optics with 'jobRun' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrjrsResponseStatus :: Lens.Lens' GetJobRunResponse Lude.Int
gjrjrsResponseStatus = Lens.lens (responseStatus :: GetJobRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobRunResponse)
{-# DEPRECATED gjrjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
