{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ContinueDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a blue/green deployment, starts the process of rerouting traffic from instances in the original environment to instances in the replacement environment without waiting for a specified wait time to elapse. (Traffic rerouting, which is achieved by registering instances in the replacement environment with the load balancer, can start as soon as all instances have a status of Ready.)
module Network.AWS.CodeDeploy.ContinueDeployment
  ( -- * Creating a request
    ContinueDeployment (..),
    mkContinueDeployment,

    -- ** Request lenses
    cdDeploymentId,
    cdDeploymentWaitType,

    -- * Destructuring the response
    ContinueDeploymentResponse (..),
    mkContinueDeploymentResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkContinueDeployment' smart constructor.
data ContinueDeployment = ContinueDeployment'
  { deploymentId ::
      Lude.Maybe Lude.Text,
    deploymentWaitType :: Lude.Maybe DeploymentWaitType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinueDeployment' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The unique ID of a blue/green deployment for which you want to start rerouting traffic to the replacement environment.
-- * 'deploymentWaitType' - The status of the deployment's waiting period. @READY_WAIT@ indicates that the deployment is ready to start shifting traffic. @TERMINATION_WAIT@ indicates that the traffic is shifted, but the original target is not terminated.
mkContinueDeployment ::
  ContinueDeployment
mkContinueDeployment =
  ContinueDeployment'
    { deploymentId = Lude.Nothing,
      deploymentWaitType = Lude.Nothing
    }

-- | The unique ID of a blue/green deployment for which you want to start rerouting traffic to the replacement environment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentId :: Lens.Lens' ContinueDeployment (Lude.Maybe Lude.Text)
cdDeploymentId = Lens.lens (deploymentId :: ContinueDeployment -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: ContinueDeployment)
{-# DEPRECATED cdDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | The status of the deployment's waiting period. @READY_WAIT@ indicates that the deployment is ready to start shifting traffic. @TERMINATION_WAIT@ indicates that the traffic is shifted, but the original target is not terminated.
--
-- /Note:/ Consider using 'deploymentWaitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDeploymentWaitType :: Lens.Lens' ContinueDeployment (Lude.Maybe DeploymentWaitType)
cdDeploymentWaitType = Lens.lens (deploymentWaitType :: ContinueDeployment -> Lude.Maybe DeploymentWaitType) (\s a -> s {deploymentWaitType = a} :: ContinueDeployment)
{-# DEPRECATED cdDeploymentWaitType "Use generic-lens or generic-optics with 'deploymentWaitType' instead." #-}

instance Lude.AWSRequest ContinueDeployment where
  type Rs ContinueDeployment = ContinueDeploymentResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull ContinueDeploymentResponse'

instance Lude.ToHeaders ContinueDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ContinueDeployment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ContinueDeployment where
  toJSON ContinueDeployment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deploymentId" Lude..=) Lude.<$> deploymentId,
            ("deploymentWaitType" Lude..=) Lude.<$> deploymentWaitType
          ]
      )

instance Lude.ToPath ContinueDeployment where
  toPath = Lude.const "/"

instance Lude.ToQuery ContinueDeployment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkContinueDeploymentResponse' smart constructor.
data ContinueDeploymentResponse = ContinueDeploymentResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinueDeploymentResponse' with the minimum fields required to make a request.
mkContinueDeploymentResponse ::
  ContinueDeploymentResponse
mkContinueDeploymentResponse = ContinueDeploymentResponse'
