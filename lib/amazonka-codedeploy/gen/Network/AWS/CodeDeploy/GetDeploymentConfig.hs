{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gdcrrsDeploymentConfigInfo,
    gdcrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetDeploymentConfig@ operation.
--
-- /See:/ 'mkGetDeploymentConfig' smart constructor.
newtype GetDeploymentConfig = GetDeploymentConfig'
  { -- | The name of a deployment configuration associated with the IAM user or AWS account.
    deploymentConfigName :: Types.DeploymentConfigName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeploymentConfig' value with any optional fields omitted.
mkGetDeploymentConfig ::
  -- | 'deploymentConfigName'
  Types.DeploymentConfigName ->
  GetDeploymentConfig
mkGetDeploymentConfig deploymentConfigName =
  GetDeploymentConfig' {deploymentConfigName}

-- | The name of a deployment configuration associated with the IAM user or AWS account.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcDeploymentConfigName :: Lens.Lens' GetDeploymentConfig Types.DeploymentConfigName
gdcDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# DEPRECATED gdcDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

instance Core.FromJSON GetDeploymentConfig where
  toJSON GetDeploymentConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("deploymentConfigName" Core..= deploymentConfigName)]
      )

instance Core.AWSRequest GetDeploymentConfig where
  type Rs GetDeploymentConfig = GetDeploymentConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.GetDeploymentConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentConfigResponse'
            Core.<$> (x Core..:? "deploymentConfigInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetDeploymentConfig@ operation.
--
-- /See:/ 'mkGetDeploymentConfigResponse' smart constructor.
data GetDeploymentConfigResponse = GetDeploymentConfigResponse'
  { -- | Information about the deployment configuration.
    deploymentConfigInfo :: Core.Maybe Types.DeploymentConfigInfo,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDeploymentConfigResponse' value with any optional fields omitted.
mkGetDeploymentConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDeploymentConfigResponse
mkGetDeploymentConfigResponse responseStatus =
  GetDeploymentConfigResponse'
    { deploymentConfigInfo = Core.Nothing,
      responseStatus
    }

-- | Information about the deployment configuration.
--
-- /Note:/ Consider using 'deploymentConfigInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsDeploymentConfigInfo :: Lens.Lens' GetDeploymentConfigResponse (Core.Maybe Types.DeploymentConfigInfo)
gdcrrsDeploymentConfigInfo = Lens.field @"deploymentConfigInfo"
{-# DEPRECATED gdcrrsDeploymentConfigInfo "Use generic-lens or generic-optics with 'deploymentConfigInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdcrrsResponseStatus :: Lens.Lens' GetDeploymentConfigResponse Core.Int
gdcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
