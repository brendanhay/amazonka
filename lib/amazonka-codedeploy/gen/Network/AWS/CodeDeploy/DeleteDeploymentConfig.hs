{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment configuration.
module Network.AWS.CodeDeploy.DeleteDeploymentConfig
  ( -- * Creating a request
    DeleteDeploymentConfig (..),
    mkDeleteDeploymentConfig,

    -- ** Request lenses
    ddcDeploymentConfigName,

    -- * Destructuring the response
    DeleteDeploymentConfigResponse (..),
    mkDeleteDeploymentConfigResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteDeploymentConfig@ operation.
--
-- /See:/ 'mkDeleteDeploymentConfig' smart constructor.
newtype DeleteDeploymentConfig = DeleteDeploymentConfig'
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

-- | Creates a value of 'DeleteDeploymentConfig' with the minimum fields required to make a request.
--
-- * 'deploymentConfigName' - The name of a deployment configuration associated with the IAM user or AWS account.
mkDeleteDeploymentConfig ::
  -- | 'deploymentConfigName'
  Lude.Text ->
  DeleteDeploymentConfig
mkDeleteDeploymentConfig pDeploymentConfigName_ =
  DeleteDeploymentConfig'
    { deploymentConfigName =
        pDeploymentConfigName_
    }

-- | The name of a deployment configuration associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDeploymentConfigName :: Lens.Lens' DeleteDeploymentConfig Lude.Text
ddcDeploymentConfigName = Lens.lens (deploymentConfigName :: DeleteDeploymentConfig -> Lude.Text) (\s a -> s {deploymentConfigName = a} :: DeleteDeploymentConfig)
{-# DEPRECATED ddcDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

instance Lude.AWSRequest DeleteDeploymentConfig where
  type Rs DeleteDeploymentConfig = DeleteDeploymentConfigResponse
  request = Req.postJSON codeDeployService
  response = Res.receiveNull DeleteDeploymentConfigResponse'

instance Lude.ToHeaders DeleteDeploymentConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.DeleteDeploymentConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDeploymentConfig where
  toJSON DeleteDeploymentConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("deploymentConfigName" Lude..= deploymentConfigName)]
      )

instance Lude.ToPath DeleteDeploymentConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDeploymentConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDeploymentConfigResponse' with the minimum fields required to make a request.
mkDeleteDeploymentConfigResponse ::
  DeleteDeploymentConfigResponse
mkDeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
