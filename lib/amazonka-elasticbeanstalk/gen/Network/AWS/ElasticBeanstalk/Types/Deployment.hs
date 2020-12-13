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
    dStatus,
    dDeploymentTime,
    dVersionLabel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an application version deployment.
--
-- /See:/ 'mkDeployment' smart constructor.
data Deployment = Deployment'
  { -- | The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
    deploymentId :: Lude.Maybe Lude.Integer,
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
    status :: Lude.Maybe Lude.Text,
    -- | For in-progress deployments, the time that the deployment started.
    --
    -- For completed deployments, the time that the deployment ended.
    deploymentTime :: Lude.Maybe Lude.DateTime,
    -- | The version label of the application version in the deployment.
    versionLabel :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- * 'deploymentId' - The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
-- * 'status' - The status of the deployment:
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
-- * 'deploymentTime' - For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
-- * 'versionLabel' - The version label of the application version in the deployment.
mkDeployment ::
  Deployment
mkDeployment =
  Deployment'
    { deploymentId = Lude.Nothing,
      status = Lude.Nothing,
      deploymentTime = Lude.Nothing,
      versionLabel = Lude.Nothing
    }

-- | The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentId :: Lens.Lens' Deployment (Lude.Maybe Lude.Integer)
dDeploymentId = Lens.lens (deploymentId :: Deployment -> Lude.Maybe Lude.Integer) (\s a -> s {deploymentId = a} :: Deployment)
{-# DEPRECATED dDeploymentId "Use generic-lens or generic-optics with 'deploymentId' instead." #-}

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
dStatus :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dStatus = Lens.lens (status :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Deployment)
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | For in-progress deployments, the time that the deployment started.
--
-- For completed deployments, the time that the deployment ended.
--
-- /Note:/ Consider using 'deploymentTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDeploymentTime :: Lens.Lens' Deployment (Lude.Maybe Lude.DateTime)
dDeploymentTime = Lens.lens (deploymentTime :: Deployment -> Lude.Maybe Lude.DateTime) (\s a -> s {deploymentTime = a} :: Deployment)
{-# DEPRECATED dDeploymentTime "Use generic-lens or generic-optics with 'deploymentTime' instead." #-}

-- | The version label of the application version in the deployment.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionLabel :: Lens.Lens' Deployment (Lude.Maybe Lude.Text)
dVersionLabel = Lens.lens (versionLabel :: Deployment -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: Deployment)
{-# DEPRECATED dVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Lude.FromXML Deployment where
  parseXML x =
    Deployment'
      Lude.<$> (x Lude..@? "DeploymentId")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DeploymentTime")
      Lude.<*> (x Lude..@? "VersionLabel")
