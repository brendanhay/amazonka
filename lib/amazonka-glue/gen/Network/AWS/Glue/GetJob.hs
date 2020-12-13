{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an existing job definition.
module Network.AWS.Glue.GetJob
  ( -- * Creating a request
    GetJob (..),
    mkGetJob,

    -- ** Request lenses
    gjJobName,

    -- * Destructuring the response
    GetJobResponse (..),
    mkGetJobResponse,

    -- ** Response lenses
    gjrsJob,
    gjrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJob' smart constructor.
newtype GetJob = GetJob'
  { -- | The name of the job definition to retrieve.
    jobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJob' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition to retrieve.
mkGetJob ::
  -- | 'jobName'
  Lude.Text ->
  GetJob
mkGetJob pJobName_ = GetJob' {jobName = pJobName_}

-- | The name of the job definition to retrieve.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjJobName :: Lens.Lens' GetJob Lude.Text
gjJobName = Lens.lens (jobName :: GetJob -> Lude.Text) (\s a -> s {jobName = a} :: GetJob)
{-# DEPRECATED gjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

instance Lude.AWSRequest GetJob where
  type Rs GetJob = GetJobResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Lude.<$> (x Lude..?> "Job") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJob where
  toJSON GetJob' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("JobName" Lude..= jobName)])

instance Lude.ToPath GetJob where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { -- | The requested job definition.
    job :: Lude.Maybe Job,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobResponse' with the minimum fields required to make a request.
--
-- * 'job' - The requested job definition.
-- * 'responseStatus' - The response status code.
mkGetJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobResponse
mkGetJobResponse pResponseStatus_ =
  GetJobResponse'
    { job = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested job definition.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsJob :: Lens.Lens' GetJobResponse (Lude.Maybe Job)
gjrsJob = Lens.lens (job :: GetJobResponse -> Lude.Maybe Job) (\s a -> s {job = a} :: GetJobResponse)
{-# DEPRECATED gjrsJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjrsResponseStatus :: Lens.Lens' GetJobResponse Lude.Int
gjrsResponseStatus = Lens.lens (responseStatus :: GetJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobResponse)
{-# DEPRECATED gjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
