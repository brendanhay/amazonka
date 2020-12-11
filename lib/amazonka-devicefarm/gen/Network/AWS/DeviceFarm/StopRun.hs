{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.StopRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current test run. AWS Device Farm immediately stops the run on devices where tests have not started. You are not billed for these devices. On devices where tests have started executing, setup suite and teardown suite tests run to completion on those devices. You are billed for setup, teardown, and any tests that were in progress or already completed.
module Network.AWS.DeviceFarm.StopRun
  ( -- * Creating a request
    StopRun (..),
    mkStopRun,

    -- ** Request lenses
    srArn,

    -- * Destructuring the response
    StopRunResponse (..),
    mkStopRunResponse,

    -- ** Response lenses
    srsRun,
    srsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to stop a specific run.
--
-- /See:/ 'mkStopRun' smart constructor.
newtype StopRun = StopRun' {arn :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopRun' with the minimum fields required to make a request.
--
-- * 'arn' - Represents the Amazon Resource Name (ARN) of the Device Farm run to stop.
mkStopRun ::
  -- | 'arn'
  Lude.Text ->
  StopRun
mkStopRun pArn_ = StopRun' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm run to stop.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srArn :: Lens.Lens' StopRun Lude.Text
srArn = Lens.lens (arn :: StopRun -> Lude.Text) (\s a -> s {arn = a} :: StopRun)
{-# DEPRECATED srArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest StopRun where
  type Rs StopRun = StopRunResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopRunResponse'
            Lude.<$> (x Lude..?> "run") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.StopRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopRun where
  toJSON StopRun' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath StopRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StopRun where
  toQuery = Lude.const Lude.mempty

-- | Represents the results of your stop run attempt.
--
-- /See:/ 'mkStopRunResponse' smart constructor.
data StopRunResponse = StopRunResponse'
  { run :: Lude.Maybe Run,
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

-- | Creates a value of 'StopRunResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'run' - The run that was stopped.
mkStopRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopRunResponse
mkStopRunResponse pResponseStatus_ =
  StopRunResponse'
    { run = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The run that was stopped.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsRun :: Lens.Lens' StopRunResponse (Lude.Maybe Run)
srsRun = Lens.lens (run :: StopRunResponse -> Lude.Maybe Run) (\s a -> s {run = a} :: StopRunResponse)
{-# DEPRECATED srsRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopRunResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopRunResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
