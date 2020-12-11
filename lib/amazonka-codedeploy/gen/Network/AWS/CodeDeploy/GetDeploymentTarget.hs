{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a deployment target.
module Network.AWS.CodeDeploy.GetDeploymentTarget
  ( -- * Creating a request
    GetDeploymentTarget (..),
    mkGetDeploymentTarget,

    -- ** Request lenses
    gdtTargetId,
    gdtDeploymentId,

    -- * Destructuring the response
    GetDeploymentTargetResponse (..),
    mkGetDeploymentTargetResponse,

    -- ** Response lenses
    gdtrsDeploymentTarget,
    gdtrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDeploymentTarget' smart constructor.
data GetDeploymentTarget = GetDeploymentTarget'
  { targetId ::
      Lude.Maybe Lude.Text,
    deploymentId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeploymentTarget' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The unique ID of a deployment.
-- * 'targetId' - The unique ID of a deployment target.
mkGetDeploymentTarget ::
  GetDeploymentTarget
mkGetDeploymentTarget =
  GetDeploymentTarget'
    { targetId = Lude.Nothing,
      deploymentId = Lude.Nothing
    }

-- | The unique ID of a deployment target.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtTargetId :: Lens.Lens' GetDeploymentTarget (Lude.Maybe Lude.Text)
gdtTargetId = Lens.lens (targetId :: GetDeploymentTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: GetDeploymentTarget)
{-# DEPRECATED gdtTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The unique ID of a deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtDeploymentId :: Lens.Lens' GetDeploymentTarget (Lude.Maybe Lude.Text)
gdtDeploymentId = Lens.lens (deploymentId :: GetDeploymentTarget -> Lude.Maybe Lude.Text) (\s a -> s {deploymentId = a} :: GetDeploymentTarget)
{-# DEPRECATED gdtDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

instance Lude.AWSRequest GetDeploymentTarget where
  type Rs GetDeploymentTarget = GetDeploymentTargetResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeploymentTargetResponse'
            Lude.<$> (x Lude..?> "deploymentTarget")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeploymentTarget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.GetDeploymentTarget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDeploymentTarget where
  toJSON GetDeploymentTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("targetId" Lude..=) Lude.<$> targetId,
            ("deploymentId" Lude..=) Lude.<$> deploymentId
          ]
      )

instance Lude.ToPath GetDeploymentTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDeploymentTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDeploymentTargetResponse' smart constructor.
data GetDeploymentTargetResponse = GetDeploymentTargetResponse'
  { deploymentTarget ::
      Lude.Maybe DeploymentTarget,
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

-- | Creates a value of 'GetDeploymentTargetResponse' with the minimum fields required to make a request.
--
-- * 'deploymentTarget' - A deployment target that contains information about a deployment such as its status, lifecycle events, and when it was last updated. It also contains metadata about the deployment target. The deployment target metadata depends on the deployment target's type (@instanceTarget@ , @lambdaTarget@ , or @ecsTarget@ ).
-- * 'responseStatus' - The response status code.
mkGetDeploymentTargetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeploymentTargetResponse
mkGetDeploymentTargetResponse pResponseStatus_ =
  GetDeploymentTargetResponse'
    { deploymentTarget = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A deployment target that contains information about a deployment such as its status, lifecycle events, and when it was last updated. It also contains metadata about the deployment target. The deployment target metadata depends on the deployment target's type (@instanceTarget@ , @lambdaTarget@ , or @ecsTarget@ ).
--
-- /Note:/ Consider using 'deploymentTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtrsDeploymentTarget :: Lens.Lens' GetDeploymentTargetResponse (Lude.Maybe DeploymentTarget)
gdtrsDeploymentTarget = Lens.lens (deploymentTarget :: GetDeploymentTargetResponse -> Lude.Maybe DeploymentTarget) (\s a -> s {deploymentTarget = a} :: GetDeploymentTargetResponse)
{-# DEPRECATED gdtrsDeploymentTarget "Use generic-lens or generic-optics with 'deploymentTarget' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdtrsResponseStatus :: Lens.Lens' GetDeploymentTargetResponse Lude.Int
gdtrsResponseStatus = Lens.lens (responseStatus :: GetDeploymentTargetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeploymentTargetResponse)
{-# DEPRECATED gdtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
