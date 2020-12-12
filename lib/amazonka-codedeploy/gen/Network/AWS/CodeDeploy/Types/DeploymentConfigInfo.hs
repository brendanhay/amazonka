{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
  ( DeploymentConfigInfo (..),

    -- * Smart constructor
    mkDeploymentConfigInfo,

    -- * Lenses
    dciDeploymentConfigName,
    dciComputePlatform,
    dciMinimumHealthyHosts,
    dciTrafficRoutingConfig,
    dciDeploymentConfigId,
    dciCreateTime,
  )
where

import Network.AWS.CodeDeploy.Types.ComputePlatform
import Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
import Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a deployment configuration.
--
-- /See:/ 'mkDeploymentConfigInfo' smart constructor.
data DeploymentConfigInfo = DeploymentConfigInfo'
  { deploymentConfigName ::
      Lude.Maybe Lude.Text,
    computePlatform :: Lude.Maybe ComputePlatform,
    minimumHealthyHosts ::
      Lude.Maybe MinimumHealthyHosts,
    trafficRoutingConfig ::
      Lude.Maybe TrafficRoutingConfig,
    deploymentConfigId :: Lude.Maybe Lude.Text,
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentConfigInfo' with the minimum fields required to make a request.
--
-- * 'computePlatform' - The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
-- * 'createTime' - The time at which the deployment configuration was created.
-- * 'deploymentConfigId' - The deployment configuration ID.
-- * 'deploymentConfigName' - The deployment configuration name.
-- * 'minimumHealthyHosts' - Information about the number or percentage of minimum healthy instance.
-- * 'trafficRoutingConfig' - The configuration that specifies how the deployment traffic is routed. Used for deployments with a Lambda or ECS compute platform only.
mkDeploymentConfigInfo ::
  DeploymentConfigInfo
mkDeploymentConfigInfo =
  DeploymentConfigInfo'
    { deploymentConfigName = Lude.Nothing,
      computePlatform = Lude.Nothing,
      minimumHealthyHosts = Lude.Nothing,
      trafficRoutingConfig = Lude.Nothing,
      deploymentConfigId = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | The deployment configuration name.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciDeploymentConfigName :: Lens.Lens' DeploymentConfigInfo (Lude.Maybe Lude.Text)
dciDeploymentConfigName = Lens.lens (deploymentConfigName :: DeploymentConfigInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigName = a} :: DeploymentConfigInfo)
{-# DEPRECATED dciDeploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead." #-}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciComputePlatform :: Lens.Lens' DeploymentConfigInfo (Lude.Maybe ComputePlatform)
dciComputePlatform = Lens.lens (computePlatform :: DeploymentConfigInfo -> Lude.Maybe ComputePlatform) (\s a -> s {computePlatform = a} :: DeploymentConfigInfo)
{-# DEPRECATED dciComputePlatform "Use generic-lens or generic-optics with 'computePlatform' instead." #-}

-- | Information about the number or percentage of minimum healthy instance.
--
-- /Note:/ Consider using 'minimumHealthyHosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciMinimumHealthyHosts :: Lens.Lens' DeploymentConfigInfo (Lude.Maybe MinimumHealthyHosts)
dciMinimumHealthyHosts = Lens.lens (minimumHealthyHosts :: DeploymentConfigInfo -> Lude.Maybe MinimumHealthyHosts) (\s a -> s {minimumHealthyHosts = a} :: DeploymentConfigInfo)
{-# DEPRECATED dciMinimumHealthyHosts "Use generic-lens or generic-optics with 'minimumHealthyHosts' instead." #-}

-- | The configuration that specifies how the deployment traffic is routed. Used for deployments with a Lambda or ECS compute platform only.
--
-- /Note:/ Consider using 'trafficRoutingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciTrafficRoutingConfig :: Lens.Lens' DeploymentConfigInfo (Lude.Maybe TrafficRoutingConfig)
dciTrafficRoutingConfig = Lens.lens (trafficRoutingConfig :: DeploymentConfigInfo -> Lude.Maybe TrafficRoutingConfig) (\s a -> s {trafficRoutingConfig = a} :: DeploymentConfigInfo)
{-# DEPRECATED dciTrafficRoutingConfig "Use generic-lens or generic-optics with 'trafficRoutingConfig' instead." #-}

-- | The deployment configuration ID.
--
-- /Note:/ Consider using 'deploymentConfigId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciDeploymentConfigId :: Lens.Lens' DeploymentConfigInfo (Lude.Maybe Lude.Text)
dciDeploymentConfigId = Lens.lens (deploymentConfigId :: DeploymentConfigInfo -> Lude.Maybe Lude.Text) (\s a -> s {deploymentConfigId = a} :: DeploymentConfigInfo)
{-# DEPRECATED dciDeploymentConfigId "Use generic-lens or generic-optics with 'deploymentConfigId' instead." #-}

-- | The time at which the deployment configuration was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciCreateTime :: Lens.Lens' DeploymentConfigInfo (Lude.Maybe Lude.Timestamp)
dciCreateTime = Lens.lens (createTime :: DeploymentConfigInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: DeploymentConfigInfo)
{-# DEPRECATED dciCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON DeploymentConfigInfo where
  parseJSON =
    Lude.withObject
      "DeploymentConfigInfo"
      ( \x ->
          DeploymentConfigInfo'
            Lude.<$> (x Lude..:? "deploymentConfigName")
            Lude.<*> (x Lude..:? "computePlatform")
            Lude.<*> (x Lude..:? "minimumHealthyHosts")
            Lude.<*> (x Lude..:? "trafficRoutingConfig")
            Lude.<*> (x Lude..:? "deploymentConfigId")
            Lude.<*> (x Lude..:? "createTime")
      )
