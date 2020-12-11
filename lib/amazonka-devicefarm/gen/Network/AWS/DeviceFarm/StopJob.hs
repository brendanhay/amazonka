{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.StopJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current job. AWS Device Farm immediately stops the job on the device where tests have not started. You are not billed for this device. On the device where tests have started, setup suite and teardown suite tests run to completion on the device. You are billed for setup, teardown, and any tests that were in progress or already completed.
module Network.AWS.DeviceFarm.StopJob
  ( -- * Creating a request
    StopJob (..),
    mkStopJob,

    -- ** Request lenses
    sjArn,

    -- * Destructuring the response
    StopJobResponse (..),
    mkStopJobResponse,

    -- ** Response lenses
    sjrsJob,
    sjrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopJob' smart constructor.
newtype StopJob = StopJob' {arn :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopJob' with the minimum fields required to make a request.
--
-- * 'arn' - Represents the Amazon Resource Name (ARN) of the Device Farm job to stop.
mkStopJob ::
  -- | 'arn'
  Lude.Text ->
  StopJob
mkStopJob pArn_ = StopJob' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm job to stop.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjArn :: Lens.Lens' StopJob Lude.Text
sjArn = Lens.lens (arn :: StopJob -> Lude.Text) (\s a -> s {arn = a} :: StopJob)
{-# DEPRECATED sjArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest StopJob where
  type Rs StopJob = StopJobResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopJobResponse'
            Lude.<$> (x Lude..?> "job") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.StopJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopJob where
  toJSON StopJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath StopJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopJobResponse' smart constructor.
data StopJobResponse = StopJobResponse'
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

-- | Creates a value of 'StopJobResponse' with the minimum fields required to make a request.
--
-- * 'job' - The job that was stopped.
-- * 'responseStatus' - The response status code.
mkStopJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopJobResponse
mkStopJobResponse pResponseStatus_ =
  StopJobResponse'
    { job = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job that was stopped.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrsJob :: Lens.Lens' StopJobResponse (Lude.Maybe Job)
sjrsJob = Lens.lens (job :: StopJobResponse -> Lude.Maybe Job) (\s a -> s {job = a} :: StopJobResponse)
{-# DEPRECATED sjrsJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sjrsResponseStatus :: Lens.Lens' StopJobResponse Lude.Int
sjrsResponseStatus = Lens.lens (responseStatus :: StopJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopJobResponse)
{-# DEPRECATED sjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
