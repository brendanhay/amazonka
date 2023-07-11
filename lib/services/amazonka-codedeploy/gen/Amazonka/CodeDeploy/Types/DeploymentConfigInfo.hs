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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentConfigInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentConfigInfo where

import Amazonka.CodeDeploy.Types.ComputePlatform
import Amazonka.CodeDeploy.Types.MinimumHealthyHosts
import Amazonka.CodeDeploy.Types.TrafficRoutingConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a deployment configuration.
--
-- /See:/ 'newDeploymentConfigInfo' smart constructor.
data DeploymentConfigInfo = DeploymentConfigInfo'
  { -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform,
    -- | The time at which the deployment configuration was created.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The deployment configuration ID.
    deploymentConfigId :: Prelude.Maybe Prelude.Text,
    -- | The deployment configuration name.
    deploymentConfigName :: Prelude.Maybe Prelude.Text,
    -- | Information about the number or percentage of minimum healthy instance.
    minimumHealthyHosts :: Prelude.Maybe MinimumHealthyHosts,
    -- | The configuration that specifies how the deployment traffic is routed.
    -- Used for deployments with a Lambda or Amazon ECS compute platform only.
    trafficRoutingConfig :: Prelude.Maybe TrafficRoutingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentConfigInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computePlatform', 'deploymentConfigInfo_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
--
-- 'createTime', 'deploymentConfigInfo_createTime' - The time at which the deployment configuration was created.
--
-- 'deploymentConfigId', 'deploymentConfigInfo_deploymentConfigId' - The deployment configuration ID.
--
-- 'deploymentConfigName', 'deploymentConfigInfo_deploymentConfigName' - The deployment configuration name.
--
-- 'minimumHealthyHosts', 'deploymentConfigInfo_minimumHealthyHosts' - Information about the number or percentage of minimum healthy instance.
--
-- 'trafficRoutingConfig', 'deploymentConfigInfo_trafficRoutingConfig' - The configuration that specifies how the deployment traffic is routed.
-- Used for deployments with a Lambda or Amazon ECS compute platform only.
newDeploymentConfigInfo ::
  DeploymentConfigInfo
newDeploymentConfigInfo =
  DeploymentConfigInfo'
    { computePlatform =
        Prelude.Nothing,
      createTime = Prelude.Nothing,
      deploymentConfigId = Prelude.Nothing,
      deploymentConfigName = Prelude.Nothing,
      minimumHealthyHosts = Prelude.Nothing,
      trafficRoutingConfig = Prelude.Nothing
    }

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
deploymentConfigInfo_computePlatform :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe ComputePlatform)
deploymentConfigInfo_computePlatform = Lens.lens (\DeploymentConfigInfo' {computePlatform} -> computePlatform) (\s@DeploymentConfigInfo' {} a -> s {computePlatform = a} :: DeploymentConfigInfo)

-- | The time at which the deployment configuration was created.
deploymentConfigInfo_createTime :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe Prelude.UTCTime)
deploymentConfigInfo_createTime = Lens.lens (\DeploymentConfigInfo' {createTime} -> createTime) (\s@DeploymentConfigInfo' {} a -> s {createTime = a} :: DeploymentConfigInfo) Prelude.. Lens.mapping Data._Time

-- | The deployment configuration ID.
deploymentConfigInfo_deploymentConfigId :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe Prelude.Text)
deploymentConfigInfo_deploymentConfigId = Lens.lens (\DeploymentConfigInfo' {deploymentConfigId} -> deploymentConfigId) (\s@DeploymentConfigInfo' {} a -> s {deploymentConfigId = a} :: DeploymentConfigInfo)

-- | The deployment configuration name.
deploymentConfigInfo_deploymentConfigName :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe Prelude.Text)
deploymentConfigInfo_deploymentConfigName = Lens.lens (\DeploymentConfigInfo' {deploymentConfigName} -> deploymentConfigName) (\s@DeploymentConfigInfo' {} a -> s {deploymentConfigName = a} :: DeploymentConfigInfo)

-- | Information about the number or percentage of minimum healthy instance.
deploymentConfigInfo_minimumHealthyHosts :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe MinimumHealthyHosts)
deploymentConfigInfo_minimumHealthyHosts = Lens.lens (\DeploymentConfigInfo' {minimumHealthyHosts} -> minimumHealthyHosts) (\s@DeploymentConfigInfo' {} a -> s {minimumHealthyHosts = a} :: DeploymentConfigInfo)

-- | The configuration that specifies how the deployment traffic is routed.
-- Used for deployments with a Lambda or Amazon ECS compute platform only.
deploymentConfigInfo_trafficRoutingConfig :: Lens.Lens' DeploymentConfigInfo (Prelude.Maybe TrafficRoutingConfig)
deploymentConfigInfo_trafficRoutingConfig = Lens.lens (\DeploymentConfigInfo' {trafficRoutingConfig} -> trafficRoutingConfig) (\s@DeploymentConfigInfo' {} a -> s {trafficRoutingConfig = a} :: DeploymentConfigInfo)

instance Data.FromJSON DeploymentConfigInfo where
  parseJSON =
    Data.withObject
      "DeploymentConfigInfo"
      ( \x ->
          DeploymentConfigInfo'
            Prelude.<$> (x Data..:? "computePlatform")
            Prelude.<*> (x Data..:? "createTime")
            Prelude.<*> (x Data..:? "deploymentConfigId")
            Prelude.<*> (x Data..:? "deploymentConfigName")
            Prelude.<*> (x Data..:? "minimumHealthyHosts")
            Prelude.<*> (x Data..:? "trafficRoutingConfig")
      )

instance Prelude.Hashable DeploymentConfigInfo where
  hashWithSalt _salt DeploymentConfigInfo' {..} =
    _salt
      `Prelude.hashWithSalt` computePlatform
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` deploymentConfigId
      `Prelude.hashWithSalt` deploymentConfigName
      `Prelude.hashWithSalt` minimumHealthyHosts
      `Prelude.hashWithSalt` trafficRoutingConfig

instance Prelude.NFData DeploymentConfigInfo where
  rnf DeploymentConfigInfo' {..} =
    Prelude.rnf computePlatform
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf deploymentConfigId
      `Prelude.seq` Prelude.rnf deploymentConfigName
      `Prelude.seq` Prelude.rnf minimumHealthyHosts
      `Prelude.seq` Prelude.rnf trafficRoutingConfig
