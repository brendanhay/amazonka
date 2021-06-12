{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentConfigInfo where

import Network.AWS.CodeDeploy.Types.ComputePlatform
import Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
import Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a deployment configuration.
--
-- /See:/ 'newDeploymentConfigInfo' smart constructor.
data DeploymentConfigInfo = DeploymentConfigInfo'
  { -- | The deployment configuration name.
    deploymentConfigName :: Core.Maybe Core.Text,
    -- | The deployment configuration ID.
    deploymentConfigId :: Core.Maybe Core.Text,
    -- | The time at which the deployment configuration was created.
    createTime :: Core.Maybe Core.POSIX,
    -- | The configuration that specifies how the deployment traffic is routed.
    -- Used for deployments with a Lambda or ECS compute platform only.
    trafficRoutingConfig :: Core.Maybe TrafficRoutingConfig,
    -- | Information about the number or percentage of minimum healthy instance.
    minimumHealthyHosts :: Core.Maybe MinimumHealthyHosts,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Core.Maybe ComputePlatform
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeploymentConfigInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfigName', 'deploymentConfigInfo_deploymentConfigName' - The deployment configuration name.
--
-- 'deploymentConfigId', 'deploymentConfigInfo_deploymentConfigId' - The deployment configuration ID.
--
-- 'createTime', 'deploymentConfigInfo_createTime' - The time at which the deployment configuration was created.
--
-- 'trafficRoutingConfig', 'deploymentConfigInfo_trafficRoutingConfig' - The configuration that specifies how the deployment traffic is routed.
-- Used for deployments with a Lambda or ECS compute platform only.
--
-- 'minimumHealthyHosts', 'deploymentConfigInfo_minimumHealthyHosts' - Information about the number or percentage of minimum healthy instance.
--
-- 'computePlatform', 'deploymentConfigInfo_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
newDeploymentConfigInfo ::
  DeploymentConfigInfo
newDeploymentConfigInfo =
  DeploymentConfigInfo'
    { deploymentConfigName =
        Core.Nothing,
      deploymentConfigId = Core.Nothing,
      createTime = Core.Nothing,
      trafficRoutingConfig = Core.Nothing,
      minimumHealthyHosts = Core.Nothing,
      computePlatform = Core.Nothing
    }

-- | The deployment configuration name.
deploymentConfigInfo_deploymentConfigName :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Core.Text)
deploymentConfigInfo_deploymentConfigName = Lens.lens (\DeploymentConfigInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentConfigInfo' {} a -> s {deploymentConfigName = a} :: DeploymentConfigInfo)

-- | The deployment configuration ID.
deploymentConfigInfo_deploymentConfigId :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Core.Text)
deploymentConfigInfo_deploymentConfigId = Lens.lens (\DeploymentConfigInfo' {deploymentConfigId} -> deploymentConfigId) (\s@DeploymentConfigInfo' {} a -> s {deploymentConfigId = a} :: DeploymentConfigInfo)

-- | The time at which the deployment configuration was created.
deploymentConfigInfo_createTime :: Lens.Lens' DeploymentConfigInfo (Core.Maybe Core.UTCTime)
deploymentConfigInfo_createTime = Lens.lens (\DeploymentConfigInfo' {createTime} -> createTime) (\s@DeploymentConfigInfo' {} a -> s {createTime = a} :: DeploymentConfigInfo) Core.. Lens.mapping Core._Time

-- | The configuration that specifies how the deployment traffic is routed.
-- Used for deployments with a Lambda or ECS compute platform only.
deploymentConfigInfo_trafficRoutingConfig :: Lens.Lens' DeploymentConfigInfo (Core.Maybe TrafficRoutingConfig)
deploymentConfigInfo_trafficRoutingConfig = Lens.lens (\DeploymentConfigInfo' {trafficRoutingConfig} -> trafficRoutingConfig) (\s@DeploymentConfigInfo' {} a -> s {trafficRoutingConfig = a} :: DeploymentConfigInfo)

-- | Information about the number or percentage of minimum healthy instance.
deploymentConfigInfo_minimumHealthyHosts :: Lens.Lens' DeploymentConfigInfo (Core.Maybe MinimumHealthyHosts)
deploymentConfigInfo_minimumHealthyHosts = Lens.lens (\DeploymentConfigInfo' {minimumHealthyHosts} -> minimumHealthyHosts) (\s@DeploymentConfigInfo' {} a -> s {minimumHealthyHosts = a} :: DeploymentConfigInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentConfigInfo_computePlatform :: Lens.Lens' DeploymentConfigInfo (Core.Maybe ComputePlatform)
deploymentConfigInfo_computePlatform = Lens.lens (\DeploymentConfigInfo' {computePlatform} -> computePlatform) (\s@DeploymentConfigInfo' {} a -> s {computePlatform = a} :: DeploymentConfigInfo)

instance Core.FromJSON DeploymentConfigInfo where
  parseJSON =
    Core.withObject
      "DeploymentConfigInfo"
      ( \x ->
          DeploymentConfigInfo'
            Core.<$> (x Core..:? "deploymentConfigName")
            Core.<*> (x Core..:? "deploymentConfigId")
            Core.<*> (x Core..:? "createTime")
            Core.<*> (x Core..:? "trafficRoutingConfig")
            Core.<*> (x Core..:? "minimumHealthyHosts")
            Core.<*> (x Core..:? "computePlatform")
      )

instance Core.Hashable DeploymentConfigInfo

instance Core.NFData DeploymentConfigInfo
