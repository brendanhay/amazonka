{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a deployment configuration.
--
-- /See:/ 'newDeploymentConfigInfo' smart constructor.
data DeploymentConfigInfo = DeploymentConfigInfo'
  { -- | The deployment configuration name.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | The deployment configuration ID.
    deploymentConfigId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the deployment configuration was created.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | The configuration that specifies how the deployment traffic is routed.
    -- Used for deployments with a Lambda or ECS compute platform only.
    trafficRoutingConfig :: Prelude.Maybe TrafficRoutingConfig,
    -- | Information about the number or percentage of minimum healthy instance.
    minimumHealthyHosts :: Prelude.Maybe MinimumHealthyHosts,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      deploymentConfigId = Prelude.Nothing,
      createTime = Prelude.Nothing,
      trafficRoutingConfig = Prelude.Nothing,
      minimumHealthyHosts = Prelude.Nothing,
      computePlatform = Prelude.Nothing
    }

-- | The deployment configuration name.
deploymentConfigInfo_deploymentConfigName :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe Prelude.Text)
deploymentConfigInfo_deploymentConfigName = Lens.lens (\DeploymentConfigInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentConfigInfo' {} a -> s {deploymentConfigName = a} :: DeploymentConfigInfo)

-- | The deployment configuration ID.
deploymentConfigInfo_deploymentConfigId :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe Prelude.Text)
deploymentConfigInfo_deploymentConfigId = Lens.lens (\DeploymentConfigInfo' {deploymentConfigId} -> deploymentConfigId) (\s@DeploymentConfigInfo' {} a -> s {deploymentConfigId = a} :: DeploymentConfigInfo)

-- | The time at which the deployment configuration was created.
deploymentConfigInfo_createTime :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe Prelude.UTCTime)
deploymentConfigInfo_createTime = Lens.lens (\DeploymentConfigInfo' {createTime} -> createTime) (\s@DeploymentConfigInfo' {} a -> s {createTime = a} :: DeploymentConfigInfo) Prelude.. Lens.mapping Prelude._Time

-- | The configuration that specifies how the deployment traffic is routed.
-- Used for deployments with a Lambda or ECS compute platform only.
deploymentConfigInfo_trafficRoutingConfig :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe TrafficRoutingConfig)
deploymentConfigInfo_trafficRoutingConfig = Lens.lens (\DeploymentConfigInfo' {trafficRoutingConfig} -> trafficRoutingConfig) (\s@DeploymentConfigInfo' {} a -> s {trafficRoutingConfig = a} :: DeploymentConfigInfo)

-- | Information about the number or percentage of minimum healthy instance.
deploymentConfigInfo_minimumHealthyHosts :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe MinimumHealthyHosts)
deploymentConfigInfo_minimumHealthyHosts = Lens.lens (\DeploymentConfigInfo' {minimumHealthyHosts} -> minimumHealthyHosts) (\s@DeploymentConfigInfo' {} a -> s {minimumHealthyHosts = a} :: DeploymentConfigInfo)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentConfigInfo_computePlatform :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe ComputePlatform)
deploymentConfigInfo_computePlatform = Lens.lens (\DeploymentConfigInfo' {computePlatform} -> computePlatform) (\s@DeploymentConfigInfo' {} a -> s {computePlatform = a} :: DeploymentConfigInfo)

instance Prelude.FromJSON DeploymentConfigInfo where
  parseJSON =
    Prelude.withObject
      "DeploymentConfigInfo"
      ( \x ->
          DeploymentConfigInfo'
            Prelude.<$> (x Prelude..:? "deploymentConfigName")
            Prelude.<*> (x Prelude..:? "deploymentConfigId")
            Prelude.<*> (x Prelude..:? "createTime")
            Prelude.<*> (x Prelude..:? "trafficRoutingConfig")
            Prelude.<*> (x Prelude..:? "minimumHealthyHosts")
            Prelude.<*> (x Prelude..:? "computePlatform")
      )

instance Prelude.Hashable DeploymentConfigInfo

instance Prelude.NFData DeploymentConfigInfo
