{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CancelMaintenanceWindowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a maintenance window execution that is already in progress and cancels any tasks in the window that have not already starting running. (Tasks already in progress will continue to completion.)
module Network.AWS.SSM.CancelMaintenanceWindowExecution
  ( -- * Creating a request
    CancelMaintenanceWindowExecution (..),
    mkCancelMaintenanceWindowExecution,

    -- ** Request lenses
    cmweWindowExecutionId,

    -- * Destructuring the response
    CancelMaintenanceWindowExecutionResponse (..),
    mkCancelMaintenanceWindowExecutionResponse,

    -- ** Response lenses
    cmwersWindowExecutionId,
    cmwersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCancelMaintenanceWindowExecution' smart constructor.
newtype CancelMaintenanceWindowExecution = CancelMaintenanceWindowExecution'
  { windowExecutionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelMaintenanceWindowExecution' with the minimum fields required to make a request.
--
-- * 'windowExecutionId' - The ID of the maintenance window execution to stop.
mkCancelMaintenanceWindowExecution ::
  -- | 'windowExecutionId'
  Lude.Text ->
  CancelMaintenanceWindowExecution
mkCancelMaintenanceWindowExecution pWindowExecutionId_ =
  CancelMaintenanceWindowExecution'
    { windowExecutionId =
        pWindowExecutionId_
    }

-- | The ID of the maintenance window execution to stop.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmweWindowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecution Lude.Text
cmweWindowExecutionId = Lens.lens (windowExecutionId :: CancelMaintenanceWindowExecution -> Lude.Text) (\s a -> s {windowExecutionId = a} :: CancelMaintenanceWindowExecution)
{-# DEPRECATED cmweWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

instance Lude.AWSRequest CancelMaintenanceWindowExecution where
  type
    Rs CancelMaintenanceWindowExecution =
      CancelMaintenanceWindowExecutionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelMaintenanceWindowExecutionResponse'
            Lude.<$> (x Lude..?> "WindowExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelMaintenanceWindowExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CancelMaintenanceWindowExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelMaintenanceWindowExecution where
  toJSON CancelMaintenanceWindowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WindowExecutionId" Lude..= windowExecutionId)]
      )

instance Lude.ToPath CancelMaintenanceWindowExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelMaintenanceWindowExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelMaintenanceWindowExecutionResponse' smart constructor.
data CancelMaintenanceWindowExecutionResponse = CancelMaintenanceWindowExecutionResponse'
  { windowExecutionId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelMaintenanceWindowExecutionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'windowExecutionId' - The ID of the maintenance window execution that has been stopped.
mkCancelMaintenanceWindowExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelMaintenanceWindowExecutionResponse
mkCancelMaintenanceWindowExecutionResponse pResponseStatus_ =
  CancelMaintenanceWindowExecutionResponse'
    { windowExecutionId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the maintenance window execution that has been stopped.
--
-- /Note:/ Consider using 'windowExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwersWindowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecutionResponse (Lude.Maybe Lude.Text)
cmwersWindowExecutionId = Lens.lens (windowExecutionId :: CancelMaintenanceWindowExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowExecutionId = a} :: CancelMaintenanceWindowExecutionResponse)
{-# DEPRECATED cmwersWindowExecutionId "Use generic-lens or generic-optics with 'windowExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmwersResponseStatus :: Lens.Lens' CancelMaintenanceWindowExecutionResponse Lude.Int
cmwersResponseStatus = Lens.lens (responseStatus :: CancelMaintenanceWindowExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelMaintenanceWindowExecutionResponse)
{-# DEPRECATED cmwersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
