{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.StopDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop an ongoing deployment.
module Network.AWS.CodeDeploy.StopDeployment
  ( -- * Creating a request
    StopDeployment (..),
    mkStopDeployment,

    -- ** Request lenses
    sdAutoRollbackEnabled,
    sdDeploymentId,

    -- * Destructuring the response
    StopDeploymentResponse (..),
    mkStopDeploymentResponse,

    -- ** Response lenses
    sdrsStatus,
    sdrsStatusMessage,
    sdrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @StopDeployment@ operation.
--
-- /See:/ 'mkStopDeployment' smart constructor.
data StopDeployment = StopDeployment'
  { autoRollbackEnabled ::
      Lude.Maybe Lude.Bool,
    deploymentId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopDeployment' with the minimum fields required to make a request.
--
-- * 'autoRollbackEnabled' - Indicates, when a deployment is stopped, whether instances that have been updated should be rolled back to the previous version of the application revision.
-- * 'deploymentId' - The unique ID of a deployment.
mkStopDeployment ::
  -- | 'deploymentId'
  Lude.Text ->
  StopDeployment
mkStopDeployment pDeploymentId_ =
  StopDeployment'
    { autoRollbackEnabled = Lude.Nothing,
      deploymentId = pDeploymentId_
    }

-- | Indicates, when a deployment is stopped, whether instances that have been updated should be rolled back to the previous version of the application revision.
--
-- /Note:/ Consider using 'autoRollbackEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAutoRollbackEnabled :: Lens.Lens' StopDeployment (Lude.Maybe Lude.Bool)
sdAutoRollbackEnabled = Lens.lens (autoRollbackEnabled :: StopDeployment -> Lude.Maybe Lude.Bool) (\s a -> s {autoRollbackEnabled = a} :: StopDeployment)
{-# DEPRECATED sdAutoRollbackEnabled "Use generic-lens or generic-optics with 'autoRollbackEnabled' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeploymentId :: Lens.Lens' StopDeployment Lude.Text
sdDeploymentId = Lens.lens (deploymentId :: StopDeployment -> Lude.Text) (\s a -> s {deploymentId = a} :: StopDeployment)
{-# DEPRECATED sdDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

instance Lude.AWSRequest StopDeployment where
  type Rs StopDeployment = StopDeploymentResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopDeploymentResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "statusMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.StopDeployment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopDeployment where
  toJSON StopDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("autoRollbackEnabled" Lude..=) Lude.<$> autoRollbackEnabled,
            Lude.Just ("deploymentId" Lude..= deploymentId)
          ]
      )

instance Lude.ToPath StopDeployment where
  toPath = Lude.const "/"

instance Lude.ToQuery StopDeployment where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @StopDeployment@ operation.
--
-- /See:/ 'mkStopDeploymentResponse' smart constructor.
data StopDeploymentResponse = StopDeploymentResponse'
  { status ::
      Lude.Maybe StopStatus,
    statusMessage :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'StopDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the stop deployment operation:
--
--
--     * Pending: The stop operation is pending.
--
--
--     * Succeeded: The stop operation was successful.
--
--
-- * 'statusMessage' - An accompanying status message.
mkStopDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopDeploymentResponse
mkStopDeploymentResponse pResponseStatus_ =
  StopDeploymentResponse'
    { status = Lude.Nothing,
      statusMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the stop deployment operation:
--
--
--     * Pending: The stop operation is pending.
--
--
--     * Succeeded: The stop operation was successful.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsStatus :: Lens.Lens' StopDeploymentResponse (Lude.Maybe StopStatus)
sdrsStatus = Lens.lens (status :: StopDeploymentResponse -> Lude.Maybe StopStatus) (\s a -> s {status = a} :: StopDeploymentResponse)
{-# DEPRECATED sdrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An accompanying status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsStatusMessage :: Lens.Lens' StopDeploymentResponse (Lude.Maybe Lude.Text)
sdrsStatusMessage = Lens.lens (statusMessage :: StopDeploymentResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: StopDeploymentResponse)
{-# DEPRECATED sdrsStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsResponseStatus :: Lens.Lens' StopDeploymentResponse Lude.Int
sdrsResponseStatus = Lens.lens (responseStatus :: StopDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopDeploymentResponse)
{-# DEPRECATED sdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
