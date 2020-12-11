{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing job definition.
module Network.AWS.Glue.UpdateJob
  ( -- * Creating a request
    UpdateJob (..),
    mkUpdateJob,

    -- ** Request lenses
    ujJobName,
    ujJobUpdate,

    -- * Destructuring the response
    UpdateJobResponse (..),
    mkUpdateJobResponse,

    -- ** Response lenses
    ujrsJobName,
    ujrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { jobName :: Lude.Text,
    jobUpdate :: JobUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition to update.
-- * 'jobUpdate' - Specifies the values with which to update the job definition.
mkUpdateJob ::
  -- | 'jobName'
  Lude.Text ->
  -- | 'jobUpdate'
  JobUpdate ->
  UpdateJob
mkUpdateJob pJobName_ pJobUpdate_ =
  UpdateJob' {jobName = pJobName_, jobUpdate = pJobUpdate_}

-- | The name of the job definition to update.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobName :: Lens.Lens' UpdateJob Lude.Text
ujJobName = Lens.lens (jobName :: UpdateJob -> Lude.Text) (\s a -> s {jobName = a} :: UpdateJob)
{-# DEPRECATED ujJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Specifies the values with which to update the job definition.
--
-- /Note:/ Consider using 'jobUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobUpdate :: Lens.Lens' UpdateJob JobUpdate
ujJobUpdate = Lens.lens (jobUpdate :: UpdateJob -> JobUpdate) (\s a -> s {jobUpdate = a} :: UpdateJob)
{-# DEPRECATED ujJobUpdate "Use generic-lens or generic-optics with 'jobUpdate' instead." #-}

instance Lude.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateJobResponse'
            Lude.<$> (x Lude..?> "JobName") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.UpdateJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobName" Lude..= jobName),
            Lude.Just ("JobUpdate" Lude..= jobUpdate)
          ]
      )

instance Lude.ToPath UpdateJob where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { jobName ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
--
-- * 'jobName' - Returns the name of the updated job definition.
-- * 'responseStatus' - The response status code.
mkUpdateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateJobResponse
mkUpdateJobResponse pResponseStatus_ =
  UpdateJobResponse'
    { jobName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns the name of the updated job definition.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsJobName :: Lens.Lens' UpdateJobResponse (Lude.Maybe Lude.Text)
ujrsJobName = Lens.lens (jobName :: UpdateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: UpdateJobResponse)
{-# DEPRECATED ujrsJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsResponseStatus :: Lens.Lens' UpdateJobResponse Lude.Int
ujrsResponseStatus = Lens.lens (responseStatus :: UpdateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJobResponse)
{-# DEPRECATED ujrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
