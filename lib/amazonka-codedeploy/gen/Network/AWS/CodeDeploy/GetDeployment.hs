{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment.
module Network.AWS.CodeDeploy.GetDeployment
  ( -- * Creating a request
    GetDeployment (..),
    mkGetDeployment,

    -- ** Request lenses
    gdDeploymentId,

    -- * Destructuring the response
    GetDeploymentResponse (..),
    mkGetDeploymentResponse,

    -- ** Response lenses
    gdrsDeploymentInfo,
    gdrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetDeployment@ operation.
--
-- /See:/ 'mkGetDeployment' smart constructor.
newtype GetDeployment = GetDeployment' {deploymentId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeployment' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The unique ID of a deployment associated with the IAM user or AWS account.
mkGetDeployment ::
  -- | 'deploymentId'
  Lude.Text ->
  GetDeployment
mkGetDeployment pDeploymentId_ =
  GetDeployment' {deploymentId = pDeploymentId_}

-- | The unique ID of a deployment associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDeploymentId :: Lens.Lens' GetDeployment Lude.Text
gdDeploymentId = Lens.lens (deploymentId :: GetDeployment -> Lude.Text) (\s a -> s {deploymentId = a} :: GetDeployment)
{-# DEPRECATED gdDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

instance Lude.AWSRequest GetDeployment where
  type Rs GetDeployment = GetDeploymentResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeploymentResponse'
            Lude.<$> (x Lude..?> "deploymentInfo")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeployment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.GetDeployment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDeployment where
  toJSON GetDeployment' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("deploymentId" Lude..= deploymentId)])

instance Lude.ToPath GetDeployment where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDeployment where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetDeployment@ operation.
--
-- /See:/ 'mkGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { deploymentInfo ::
      Lude.Maybe DeploymentInfo,
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

-- | Creates a value of 'GetDeploymentResponse' with the minimum fields required to make a request.
--
-- * 'deploymentInfo' - Information about the deployment.
-- * 'responseStatus' - The response status code.
mkGetDeploymentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeploymentResponse
mkGetDeploymentResponse pResponseStatus_ =
  GetDeploymentResponse'
    { deploymentInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deployment.
--
-- /Note:/ Consider using 'deploymentInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsDeploymentInfo :: Lens.Lens' GetDeploymentResponse (Lude.Maybe DeploymentInfo)
gdrsDeploymentInfo = Lens.lens (deploymentInfo :: GetDeploymentResponse -> Lude.Maybe DeploymentInfo) (\s a -> s {deploymentInfo = a} :: GetDeploymentResponse)
{-# DEPRECATED gdrsDeploymentInfo "Use generic-lens or generic-optics with 'deploymentInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDeploymentResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDeploymentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeploymentResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
