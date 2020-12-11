{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a job.
module Network.AWS.DeviceFarm.GetJob
  ( -- * Creating a request
    GetJob (..),
    mkGetJob,

    -- ** Request lenses
    gjArn,

    -- * Destructuring the response
    GetJobResponse (..),
    mkGetJobResponse,

    -- ** Response lenses
    gjrsJob,
    gjrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get job operation.
--
-- /See:/ 'mkGetJob' smart constructor.
newtype GetJob = GetJob' {arn :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJob' with the minimum fields required to make a request.
--
-- * 'arn' - The job's ARN.
mkGetJob ::
  -- | 'arn'
  Lude.Text ->
  GetJob
mkGetJob pArn_ = GetJob' {arn = pArn_}

-- | The job's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjArn :: Lens.Lens' GetJob Lude.Text
gjArn = Lens.lens (arn :: GetJob -> Lude.Text) (\s a -> s {arn = a} :: GetJob)
{-# DEPRECATED gjArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetJob where
  type Rs GetJob = GetJobResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobResponse'
            Lude.<$> (x Lude..?> "job") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetJob where
  toJSON GetJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetJob where
  toPath = Lude.const "/"

instance Lude.ToQuery GetJob where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get job request.
--
-- /See:/ 'mkGetJobResponse' smart constructor.
data GetJobResponse = GetJobResponse'
  { job :: Lude.Maybe Job,
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

-- | Creates a value of 'GetJobResponse' with the minimum fields required to make a request.
--
-- * 'job' - An object that contains information about the requested job.
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

-- | An object that contains information about the requested job.
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
