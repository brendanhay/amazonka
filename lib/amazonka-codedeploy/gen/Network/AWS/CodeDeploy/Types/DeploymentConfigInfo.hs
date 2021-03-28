{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
  ( DeploymentConfigInfo (..)
  -- * Smart constructor
  , mkDeploymentConfigInfo
  -- * Lenses
  , dciComputePlatform
  , dciCreateTime
  , dciDeploymentConfigId
  , dciDeploymentConfigName
  , dciMinimumHealthyHosts
  , dciTrafficRoutingConfig
  ) where

import qualified Network.AWS.CodeDeploy.Types.ComputePlatform as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentConfigId as Types
import qualified Network.AWS.CodeDeploy.Types.DeploymentConfigName as Types
import qualified Network.AWS.CodeDeploy.Types.MinimumHealthyHosts as Types
import qualified Network.AWS.CodeDeploy.Types.TrafficRoutingConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a deployment configuration.
--
-- /See:/ 'mkDeploymentConfigInfo' smart constructor.
data DeploymentConfigInfo = DeploymentConfigInfo'
  { computePlatform :: Core.Maybe Types.ComputePlatform
    -- ^ The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
  , createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the deployment configuration was created.
  , deploymentConfigId :: Core.Maybe Types.DeploymentConfigId
    -- ^ The deployment configuration ID.
  , deploymentConfigName :: Core.Maybe Types.DeploymentConfigName
    -- ^ The deployment configuration name.
  , minimumHealthyHosts :: Core.Maybe Types.MinimumHealthyHosts
    -- ^ Information about the number or percentage of minimum healthy instance.
  , trafficRoutingConfig :: Core.Maybe Types.TrafficRoutingConfig
    -- ^ The configuration that specifies how the deployment traffic is routed. Used for deployments with a Lambda or ECS compute platform only.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeploymentConfigInfo' value with any optional fields omitted.
mkDeploymentConfigInfo
    :: DeploymentConfigInfo
mkDeploymentConfigInfo
  = DeploymentConfigInfo'{computePlatform = Core.Nothing,
                          createTime = Core.Nothing, deploymentConfigId = Core.Nothing,
                          deploymentConfigName = Core.Nothing,
                          minimumHealthyHosts = Core.Nothing,
                          trafficRoutingConfig = Core.Nothing}

-- | The destination platform type for the deployment (@Lambda@ , @Server@ , or @ECS@ ).
--
-- /Note:/ Consider using 'computePlatform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciComputePlatform :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Types.ComputePlatform)
dciComputePlatform = Lens.field @"computePlatform"
{-# INLINEABLE dciComputePlatform #-}
{-# DEPRECATED computePlatform "Use generic-lens or generic-optics with 'computePlatform' instead"  #-}

-- | The time at which the deployment configuration was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciCreateTime :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Core.NominalDiffTime)
dciCreateTime = Lens.field @"createTime"
{-# INLINEABLE dciCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The deployment configuration ID.
--
-- /Note:/ Consider using 'deploymentConfigId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciDeploymentConfigId :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Types.DeploymentConfigId)
dciDeploymentConfigId = Lens.field @"deploymentConfigId"
{-# INLINEABLE dciDeploymentConfigId #-}
{-# DEPRECATED deploymentConfigId "Use generic-lens or generic-optics with 'deploymentConfigId' instead"  #-}

-- | The deployment configuration name.
--
-- /Note:/ Consider using 'deploymentConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciDeploymentConfigName :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Types.DeploymentConfigName)
dciDeploymentConfigName = Lens.field @"deploymentConfigName"
{-# INLINEABLE dciDeploymentConfigName #-}
{-# DEPRECATED deploymentConfigName "Use generic-lens or generic-optics with 'deploymentConfigName' instead"  #-}

-- | Information about the number or percentage of minimum healthy instance.
--
-- /Note:/ Consider using 'minimumHealthyHosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciMinimumHealthyHosts :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Types.MinimumHealthyHosts)
dciMinimumHealthyHosts = Lens.field @"minimumHealthyHosts"
{-# INLINEABLE dciMinimumHealthyHosts #-}
{-# DEPRECATED minimumHealthyHosts "Use generic-lens or generic-optics with 'minimumHealthyHosts' instead"  #-}

-- | The configuration that specifies how the deployment traffic is routed. Used for deployments with a Lambda or ECS compute platform only.
--
-- /Note:/ Consider using 'trafficRoutingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dciTrafficRoutingConfig :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Types.TrafficRoutingConfig)
dciTrafficRoutingConfig = Lens.field @"trafficRoutingConfig"
{-# INLINEABLE dciTrafficRoutingConfig #-}
{-# DEPRECATED trafficRoutingConfig "Use generic-lens or generic-optics with 'trafficRoutingConfig' instead"  #-}

instance Core.FromJSON DeploymentConfigInfo where
        parseJSON
          = Core.withObject "DeploymentConfigInfo" Core.$
              \ x ->
                DeploymentConfigInfo' Core.<$>
                  (x Core..:? "computePlatform") Core.<*> x Core..:? "createTime"
                    Core.<*> x Core..:? "deploymentConfigId"
                    Core.<*> x Core..:? "deploymentConfigName"
                    Core.<*> x Core..:? "minimumHealthyHosts"
                    Core.<*> x Core..:? "trafficRoutingConfig"
