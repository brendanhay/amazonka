{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a run.
module Network.AWS.DeviceFarm.GetRun
  ( -- * Creating a request
    GetRun (..),
    mkGetRun,

    -- ** Request lenses
    grArn,

    -- * Destructuring the response
    GetRunResponse (..),
    mkGetRunResponse,

    -- ** Response lenses
    grrsRun,
    grrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get run operation.
--
-- /See:/ 'mkGetRun' smart constructor.
newtype GetRun = GetRun'
  { -- | The run's ARN.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRun' with the minimum fields required to make a request.
--
-- * 'arn' - The run's ARN.
mkGetRun ::
  -- | 'arn'
  Lude.Text ->
  GetRun
mkGetRun pArn_ = GetRun' {arn = pArn_}

-- | The run's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grArn :: Lens.Lens' GetRun Lude.Text
grArn = Lens.lens (arn :: GetRun -> Lude.Text) (\s a -> s {arn = a} :: GetRun)
{-# DEPRECATED grArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetRun where
  type Rs GetRun = GetRunResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRunResponse'
            Lude.<$> (x Lude..?> "run") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRun where
  toJSON GetRun' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetRun where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRun where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get run request.
--
-- /See:/ 'mkGetRunResponse' smart constructor.
data GetRunResponse = GetRunResponse'
  { -- | The run to get results from.
    run :: Lude.Maybe Run,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRunResponse' with the minimum fields required to make a request.
--
-- * 'run' - The run to get results from.
-- * 'responseStatus' - The response status code.
mkGetRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRunResponse
mkGetRunResponse pResponseStatus_ =
  GetRunResponse'
    { run = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The run to get results from.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRun :: Lens.Lens' GetRunResponse (Lude.Maybe Run)
grrsRun = Lens.lens (run :: GetRunResponse -> Lude.Maybe Run) (\s a -> s {run = a} :: GetRunResponse)
{-# DEPRECATED grrsRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRunResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRunResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
