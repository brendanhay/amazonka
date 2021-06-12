{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment configuration.
module Network.AWS.CodeDeploy.CreateDeploymentConfig
  ( -- * Creating a Request
    CreateDeploymentConfig (..),
    newCreateDeploymentConfig,

    -- * Request Lenses
    createDeploymentConfig_trafficRoutingConfig,
    createDeploymentConfig_minimumHealthyHosts,
    createDeploymentConfig_computePlatform,
    createDeploymentConfig_deploymentConfigName,

    -- * Destructuring the Response
    CreateDeploymentConfigResponse (..),
    newCreateDeploymentConfigResponse,

    -- * Response Lenses
    createDeploymentConfigResponse_deploymentConfigId,
    createDeploymentConfigResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateDeploymentConfig@ operation.
--
-- /See:/ 'newCreateDeploymentConfig' smart constructor.
data CreateDeploymentConfig = CreateDeploymentConfig'
  { -- | The configuration that specifies how the deployment traffic is routed.
    trafficRoutingConfig :: Core.Maybe TrafficRoutingConfig,
    -- | The minimum number of healthy instances that should be available at any
    -- time during the deployment. There are two parameters expected in the
    -- input: type and value.
    --
    -- The type parameter takes either of the following values:
    --
    -- -   HOST_COUNT: The value parameter represents the minimum number of
    --     healthy instances as an absolute value.
    --
    -- -   FLEET_PERCENT: The value parameter represents the minimum number of
    --     healthy instances as a percentage of the total number of instances
    --     in the deployment. If you specify FLEET_PERCENT, at the start of the
    --     deployment, AWS CodeDeploy converts the percentage to the equivalent
    --     number of instances and rounds up fractional instances.
    --
    -- The value parameter takes an integer.
    --
    -- For example, to set a minimum of 95% healthy instance, specify a type of
    -- FLEET_PERCENT and a value of 95.
    minimumHealthyHosts :: Core.Maybe MinimumHealthyHosts,
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Core.Maybe ComputePlatform,
    -- | The name of the deployment configuration to create.
    deploymentConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficRoutingConfig', 'createDeploymentConfig_trafficRoutingConfig' - The configuration that specifies how the deployment traffic is routed.
--
-- 'minimumHealthyHosts', 'createDeploymentConfig_minimumHealthyHosts' - The minimum number of healthy instances that should be available at any
-- time during the deployment. There are two parameters expected in the
-- input: type and value.
--
-- The type parameter takes either of the following values:
--
-- -   HOST_COUNT: The value parameter represents the minimum number of
--     healthy instances as an absolute value.
--
-- -   FLEET_PERCENT: The value parameter represents the minimum number of
--     healthy instances as a percentage of the total number of instances
--     in the deployment. If you specify FLEET_PERCENT, at the start of the
--     deployment, AWS CodeDeploy converts the percentage to the equivalent
--     number of instances and rounds up fractional instances.
--
-- The value parameter takes an integer.
--
-- For example, to set a minimum of 95% healthy instance, specify a type of
-- FLEET_PERCENT and a value of 95.
--
-- 'computePlatform', 'createDeploymentConfig_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
--
-- 'deploymentConfigName', 'createDeploymentConfig_deploymentConfigName' - The name of the deployment configuration to create.
newCreateDeploymentConfig ::
  -- | 'deploymentConfigName'
  Core.Text ->
  CreateDeploymentConfig
newCreateDeploymentConfig pDeploymentConfigName_ =
  CreateDeploymentConfig'
    { trafficRoutingConfig =
        Core.Nothing,
      minimumHealthyHosts = Core.Nothing,
      computePlatform = Core.Nothing,
      deploymentConfigName = pDeploymentConfigName_
    }

-- | The configuration that specifies how the deployment traffic is routed.
createDeploymentConfig_trafficRoutingConfig :: Lens.Lens' CreateDeploymentConfig (Core.Maybe TrafficRoutingConfig)
createDeploymentConfig_trafficRoutingConfig = Lens.lens (\CreateDeploymentConfig' {trafficRoutingConfig} -> trafficRoutingConfig) (\s@CreateDeploymentConfig' {} a -> s {trafficRoutingConfig = a} :: CreateDeploymentConfig)

-- | The minimum number of healthy instances that should be available at any
-- time during the deployment. There are two parameters expected in the
-- input: type and value.
--
-- The type parameter takes either of the following values:
--
-- -   HOST_COUNT: The value parameter represents the minimum number of
--     healthy instances as an absolute value.
--
-- -   FLEET_PERCENT: The value parameter represents the minimum number of
--     healthy instances as a percentage of the total number of instances
--     in the deployment. If you specify FLEET_PERCENT, at the start of the
--     deployment, AWS CodeDeploy converts the percentage to the equivalent
--     number of instances and rounds up fractional instances.
--
-- The value parameter takes an integer.
--
-- For example, to set a minimum of 95% healthy instance, specify a type of
-- FLEET_PERCENT and a value of 95.
createDeploymentConfig_minimumHealthyHosts :: Lens.Lens' CreateDeploymentConfig (Core.Maybe MinimumHealthyHosts)
createDeploymentConfig_minimumHealthyHosts = Lens.lens (\CreateDeploymentConfig' {minimumHealthyHosts} -> minimumHealthyHosts) (\s@CreateDeploymentConfig' {} a -> s {minimumHealthyHosts = a} :: CreateDeploymentConfig)

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
createDeploymentConfig_computePlatform :: Lens.Lens' CreateDeploymentConfig (Core.Maybe ComputePlatform)
createDeploymentConfig_computePlatform = Lens.lens (\CreateDeploymentConfig' {computePlatform} -> computePlatform) (\s@CreateDeploymentConfig' {} a -> s {computePlatform = a} :: CreateDeploymentConfig)

-- | The name of the deployment configuration to create.
createDeploymentConfig_deploymentConfigName :: Lens.Lens' CreateDeploymentConfig Core.Text
createDeploymentConfig_deploymentConfigName = Lens.lens (\CreateDeploymentConfig' {deploymentConfigName} -> deploymentConfigName) (\s@CreateDeploymentConfig' {} a -> s {deploymentConfigName = a} :: CreateDeploymentConfig)

instance Core.AWSRequest CreateDeploymentConfig where
  type
    AWSResponse CreateDeploymentConfig =
      CreateDeploymentConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentConfigResponse'
            Core.<$> (x Core..?> "deploymentConfigId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDeploymentConfig

instance Core.NFData CreateDeploymentConfig

instance Core.ToHeaders CreateDeploymentConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.CreateDeploymentConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDeploymentConfig where
  toJSON CreateDeploymentConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("trafficRoutingConfig" Core..=)
              Core.<$> trafficRoutingConfig,
            ("minimumHealthyHosts" Core..=)
              Core.<$> minimumHealthyHosts,
            ("computePlatform" Core..=) Core.<$> computePlatform,
            Core.Just
              ( "deploymentConfigName"
                  Core..= deploymentConfigName
              )
          ]
      )

instance Core.ToPath CreateDeploymentConfig where
  toPath = Core.const "/"

instance Core.ToQuery CreateDeploymentConfig where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @CreateDeploymentConfig@ operation.
--
-- /See:/ 'newCreateDeploymentConfigResponse' smart constructor.
data CreateDeploymentConfigResponse = CreateDeploymentConfigResponse'
  { -- | A unique deployment configuration ID.
    deploymentConfigId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDeploymentConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfigId', 'createDeploymentConfigResponse_deploymentConfigId' - A unique deployment configuration ID.
--
-- 'httpStatus', 'createDeploymentConfigResponse_httpStatus' - The response's http status code.
newCreateDeploymentConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDeploymentConfigResponse
newCreateDeploymentConfigResponse pHttpStatus_ =
  CreateDeploymentConfigResponse'
    { deploymentConfigId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique deployment configuration ID.
createDeploymentConfigResponse_deploymentConfigId :: Lens.Lens' CreateDeploymentConfigResponse (Core.Maybe Core.Text)
createDeploymentConfigResponse_deploymentConfigId = Lens.lens (\CreateDeploymentConfigResponse' {deploymentConfigId} -> deploymentConfigId) (\s@CreateDeploymentConfigResponse' {} a -> s {deploymentConfigId = a} :: CreateDeploymentConfigResponse)

-- | The response's http status code.
createDeploymentConfigResponse_httpStatus :: Lens.Lens' CreateDeploymentConfigResponse Core.Int
createDeploymentConfigResponse_httpStatus = Lens.lens (\CreateDeploymentConfigResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentConfigResponse' {} a -> s {httpStatus = a} :: CreateDeploymentConfigResponse)

instance Core.NFData CreateDeploymentConfigResponse
