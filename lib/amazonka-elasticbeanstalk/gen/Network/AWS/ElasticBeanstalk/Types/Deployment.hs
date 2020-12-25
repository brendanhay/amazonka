{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Deployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Deployment
  ( Deployment (..),

    -- * Smart constructor
    mkDeployment,

    -- * Lenses
    dDeploymentId,
    dDeploymentTime,
    dStatus,
    dVersionLabel,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an application version deployment.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
    deploymentId :: Core.Maybe Core.Integer,
    -- | For in-progress deployments, the time that the deployment started.
    --
    -- For completed deployments, the time that the deployment ended.
    deploymentTime :: Core.Maybe Core.UTCTime,
    -- | The status of the deployment:
    --
    --
    --     * @In Progress@ : The deployment is in progress.
    --
    --
    --     * @Deployed@ : The deployment succeeded.
    --
    --
    --     * @Failed@ : The deployment failed.
    status :: Core.Maybe Types.String,
    -- | The version label of the application version in the deployment.
    versionLabel :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Deployment' value with any optional fields omitted.
mkDeployment ::
  Deployment
mkDeployment =
  Deployment'
    { deploymentId = Core.Nothing,
      deploymentTime = Core.Nothing,
      status = Core.Nothing,
      versionLabel = Core.Nothing
    }

-- | The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentId :: Lens.Lens' Deployment (Core.Maybe Core.Integer)
dDeploymentId = Lens.field @"deploymentId"
{-# DEPRECATED dDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

-- | For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
--
-- /Note:/ Consider using 'deploymentTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentTime :: Lens.Lens' Deployment (Core.Maybe Core.UTCTime)
dDeploymentTime = Lens.field @"deploymentTime"
{-# DEPRECATED dDeploymentTime "Use generic-lens or generic-optics with 'deploymentTime' instead." #-}

-- | The status of the deployment:
--
--
--     * @In Progress@ : The deployment is in progress.
--
--
--     * @Deployed@ : The deployment succeeded.
--
--
--     * @Failed@ : The deployment failed.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Deployment (Core.Maybe Types.String)
dStatus = Lens.field @"status"
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version label of the application version in the deployment.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionLabel :: Lens.Lens' Deployment (Core.Maybe Types.String)
dVersionLabel = Lens.field @"versionLabel"
{-# DEPRECATED dVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Core.FromXML Deployment where
  parseXML x =
    Deployment'
      Core.<$> (x Core..@? "DeploymentId")
      Core.<*> (x Core..@? "DeploymentTime")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "VersionLabel")
