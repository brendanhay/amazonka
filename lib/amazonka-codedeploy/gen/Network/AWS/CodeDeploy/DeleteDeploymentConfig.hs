{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteDeploymentConfig@ operation.
--
-- /See:/ 'mkDeleteDeploymentConfig' smart constructor.
newtype DeleteDeploymentConfig = DeleteDeploymentConfig'
  { -- | The name of a deployment configuration associated with the IAM user or AWS account.
    deploymentConfigName :: Types.DeploymentConfigName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentConfig' value with any optional fields omitted.
mkDeleteDeploymentConfig ::
  -- | 'deploymentConfigName'
  Types.DeploymentConfigName ->
  DeleteDeploymentConfig
mkDeleteDeploymentConfig deploymentConfigName =
  DeleteDeploymentConfig' {deploymentConfigName}

-- | The name of a deployment configuration associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcDeploymentConfigName :: Lens.Lens' DeleteDeploymentConfig Types.DeploymentConfigName
ddcDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# DEPRECATED ddcDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

instance Core.FromJSON DeleteDeploymentConfig where
  toJSON DeleteDeploymentConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("deploymentConfigName" Core..= deploymentConfigName)]
      )

instance Core.AWSRequest DeleteDeploymentConfig where
  type Rs DeleteDeploymentConfig = DeleteDeploymentConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.DeleteDeploymentConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteDeploymentConfigResponse'

-- | /See:/ 'mkDeleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDeploymentConfigResponse' value with any optional fields omitted.
mkDeleteDeploymentConfigResponse ::
  DeleteDeploymentConfigResponse
mkDeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
