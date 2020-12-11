{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetDeploymentConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment configuration.
module Network.AWS.CodeDeploy.GetDeploymentConfig
  ( -- * Creating a request
    GetDeploymentConfig (..),
    mkGetDeploymentConfig,

    -- ** Request lenses
    gdcDeploymentConfigName,

    -- * Destructuring the response
    GetDeploymentConfigResponse (..),
    mkGetDeploymentConfigResponse,

    -- ** Response lenses
    gdcrsDeploymentConfigInfo,
    gdcrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @GetDeploymentConfig@ operation.
--
-- /See:/ 'mkGetDeploymentConfig' smart constructor.
newtype GetDeploymentConfig = GetDeploymentConfig'
  { deploymentConfigName ::
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

-- | Creates a value of 'GetDeploymentConfig' with the minimum fields required to make a request.
--
-- * 'deploymentConfigName' - The name of a deployment configuration associated with the IAM user or AWS account.
mkGetDeploymentConfig ::
  -- | 'deploymentConfigName'
  Lude.Text ->
  GetDeploymentConfig
mkGetDeploymentConfig pDeploymentConfigName_ =
  GetDeploymentConfig'
    { deploymentConfigName =
        pDeploymentConfigName_
    }

-- | The name of a deployment configuration associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcDeploymentConfigName :: Lens.Lens' GetDeploymentConfig Lude.Text
gdcDeploymentConfigName = Lens.lens (deploymentConfigName :: GetDeploymentConfig -> Lude.Text) (\s a -> s {deploymentConfigName = a} :: GetDeploymentConfig)
{-# DEPRECATED gdcDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

instance Lude.AWSRequest GetDeploymentConfig where
  type Rs GetDeploymentConfig = GetDeploymentConfigResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeploymentConfigResponse'
            Lude.<$> (x Lude..?> "deploymentConfigInfo")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeploymentConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.GetDeploymentConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDeploymentConfig where
  toJSON GetDeploymentConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("deploymentConfigName" Lude..= deploymentConfigName)]
      )

instance Lude.ToPath GetDeploymentConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDeploymentConfig where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetDeploymentConfig@ operation.
--
-- /See:/ 'mkGetDeploymentConfigResponse' smart constructor.
data GetDeploymentConfigResponse = GetDeploymentConfigResponse'
  { deploymentConfigInfo ::
      Lude.Maybe DeploymentConfigInfo,
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

-- | Creates a value of 'GetDeploymentConfigResponse' with the minimum fields required to make a request.
--
-- * 'deploymentConfigInfo' - Information about the deployment configuration.
-- * 'responseStatus' - The response status code.
mkGetDeploymentConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeploymentConfigResponse
mkGetDeploymentConfigResponse pResponseStatus_ =
  GetDeploymentConfigResponse'
    { deploymentConfigInfo = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deployment configuration.
--
-- /Note:/ Consider using 'deploymentConfigInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsDeploymentConfigInfo :: Lens.Lens' GetDeploymentConfigResponse (Lude.Maybe DeploymentConfigInfo)
gdcrsDeploymentConfigInfo = Lens.lens (deploymentConfigInfo :: GetDeploymentConfigResponse -> Lude.Maybe DeploymentConfigInfo) (\s a -> s {deploymentConfigInfo = a} :: GetDeploymentConfigResponse)
{-# DEPRECATED gdcrsDeploymentConfigInfo "Use generic-lens or generic-optics with 'deploymentConfigInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrsResponseStatus :: Lens.Lens' GetDeploymentConfigResponse Lude.Int
gdcrsResponseStatus = Lens.lens (responseStatus :: GetDeploymentConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeploymentConfigResponse)
{-# DEPRECATED gdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
