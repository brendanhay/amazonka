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
-- Module      : Amazonka.CodeDeploy.CreateDeploymentConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment configuration.
module Amazonka.CodeDeploy.CreateDeploymentConfig
  ( -- * Creating a Request
    CreateDeploymentConfig (..),
    newCreateDeploymentConfig,

    -- * Request Lenses
    createDeploymentConfig_computePlatform,
    createDeploymentConfig_minimumHealthyHosts,
    createDeploymentConfig_trafficRoutingConfig,
    createDeploymentConfig_deploymentConfigName,

    -- * Destructuring the Response
    CreateDeploymentConfigResponse (..),
    newCreateDeploymentConfigResponse,

    -- * Response Lenses
    createDeploymentConfigResponse_deploymentConfigId,
    createDeploymentConfigResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateDeploymentConfig@ operation.
--
-- /See:/ 'newCreateDeploymentConfig' smart constructor.
data CreateDeploymentConfig = CreateDeploymentConfig'
  { -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform,
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
    --     deployment, CodeDeploy converts the percentage to the equivalent
    --     number of instances and rounds up fractional instances.
    --
    -- The value parameter takes an integer.
    --
    -- For example, to set a minimum of 95% healthy instance, specify a type of
    -- FLEET_PERCENT and a value of 95.
    minimumHealthyHosts :: Prelude.Maybe MinimumHealthyHosts,
    -- | The configuration that specifies how the deployment traffic is routed.
    trafficRoutingConfig :: Prelude.Maybe TrafficRoutingConfig,
    -- | The name of the deployment configuration to create.
    deploymentConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computePlatform', 'createDeploymentConfig_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
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
--     deployment, CodeDeploy converts the percentage to the equivalent
--     number of instances and rounds up fractional instances.
--
-- The value parameter takes an integer.
--
-- For example, to set a minimum of 95% healthy instance, specify a type of
-- FLEET_PERCENT and a value of 95.
--
-- 'trafficRoutingConfig', 'createDeploymentConfig_trafficRoutingConfig' - The configuration that specifies how the deployment traffic is routed.
--
-- 'deploymentConfigName', 'createDeploymentConfig_deploymentConfigName' - The name of the deployment configuration to create.
newCreateDeploymentConfig ::
  -- | 'deploymentConfigName'
  Prelude.Text ->
  CreateDeploymentConfig
newCreateDeploymentConfig pDeploymentConfigName_ =
  CreateDeploymentConfig'
    { computePlatform =
        Prelude.Nothing,
      minimumHealthyHosts = Prelude.Nothing,
      trafficRoutingConfig = Prelude.Nothing,
      deploymentConfigName = pDeploymentConfigName_
    }

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
createDeploymentConfig_computePlatform :: Lens.Lens' CreateDeploymentConfig (Prelude.Maybe ComputePlatform)
createDeploymentConfig_computePlatform = Lens.lens (\CreateDeploymentConfig' {computePlatform} -> computePlatform) (\s@CreateDeploymentConfig' {} a -> s {computePlatform = a} :: CreateDeploymentConfig)

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
--     deployment, CodeDeploy converts the percentage to the equivalent
--     number of instances and rounds up fractional instances.
--
-- The value parameter takes an integer.
--
-- For example, to set a minimum of 95% healthy instance, specify a type of
-- FLEET_PERCENT and a value of 95.
createDeploymentConfig_minimumHealthyHosts :: Lens.Lens' CreateDeploymentConfig (Prelude.Maybe MinimumHealthyHosts)
createDeploymentConfig_minimumHealthyHosts = Lens.lens (\CreateDeploymentConfig' {minimumHealthyHosts} -> minimumHealthyHosts) (\s@CreateDeploymentConfig' {} a -> s {minimumHealthyHosts = a} :: CreateDeploymentConfig)

-- | The configuration that specifies how the deployment traffic is routed.
createDeploymentConfig_trafficRoutingConfig :: Lens.Lens' CreateDeploymentConfig (Prelude.Maybe TrafficRoutingConfig)
createDeploymentConfig_trafficRoutingConfig = Lens.lens (\CreateDeploymentConfig' {trafficRoutingConfig} -> trafficRoutingConfig) (\s@CreateDeploymentConfig' {} a -> s {trafficRoutingConfig = a} :: CreateDeploymentConfig)

-- | The name of the deployment configuration to create.
createDeploymentConfig_deploymentConfigName :: Lens.Lens' CreateDeploymentConfig Prelude.Text
createDeploymentConfig_deploymentConfigName = Lens.lens (\CreateDeploymentConfig' {deploymentConfigName} -> deploymentConfigName) (\s@CreateDeploymentConfig' {} a -> s {deploymentConfigName = a} :: CreateDeploymentConfig)

instance Core.AWSRequest CreateDeploymentConfig where
  type
    AWSResponse CreateDeploymentConfig =
      CreateDeploymentConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentConfigResponse'
            Prelude.<$> (x Data..?> "deploymentConfigId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeploymentConfig where
  hashWithSalt _salt CreateDeploymentConfig' {..} =
    _salt
      `Prelude.hashWithSalt` computePlatform
      `Prelude.hashWithSalt` minimumHealthyHosts
      `Prelude.hashWithSalt` trafficRoutingConfig
      `Prelude.hashWithSalt` deploymentConfigName

instance Prelude.NFData CreateDeploymentConfig where
  rnf CreateDeploymentConfig' {..} =
    Prelude.rnf computePlatform `Prelude.seq`
      Prelude.rnf minimumHealthyHosts `Prelude.seq`
        Prelude.rnf trafficRoutingConfig `Prelude.seq`
          Prelude.rnf deploymentConfigName

instance Data.ToHeaders CreateDeploymentConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.CreateDeploymentConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDeploymentConfig where
  toJSON CreateDeploymentConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("computePlatform" Data..=)
              Prelude.<$> computePlatform,
            ("minimumHealthyHosts" Data..=)
              Prelude.<$> minimumHealthyHosts,
            ("trafficRoutingConfig" Data..=)
              Prelude.<$> trafficRoutingConfig,
            Prelude.Just
              ( "deploymentConfigName"
                  Data..= deploymentConfigName
              )
          ]
      )

instance Data.ToPath CreateDeploymentConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDeploymentConfig where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateDeploymentConfig@ operation.
--
-- /See:/ 'newCreateDeploymentConfigResponse' smart constructor.
data CreateDeploymentConfigResponse = CreateDeploymentConfigResponse'
  { -- | A unique deployment configuration ID.
    deploymentConfigId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateDeploymentConfigResponse
newCreateDeploymentConfigResponse pHttpStatus_ =
  CreateDeploymentConfigResponse'
    { deploymentConfigId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique deployment configuration ID.
createDeploymentConfigResponse_deploymentConfigId :: Lens.Lens' CreateDeploymentConfigResponse (Prelude.Maybe Prelude.Text)
createDeploymentConfigResponse_deploymentConfigId = Lens.lens (\CreateDeploymentConfigResponse' {deploymentConfigId} -> deploymentConfigId) (\s@CreateDeploymentConfigResponse' {} a -> s {deploymentConfigId = a} :: CreateDeploymentConfigResponse)

-- | The response's http status code.
createDeploymentConfigResponse_httpStatus :: Lens.Lens' CreateDeploymentConfigResponse Prelude.Int
createDeploymentConfigResponse_httpStatus = Lens.lens (\CreateDeploymentConfigResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentConfigResponse' {} a -> s {httpStatus = a} :: CreateDeploymentConfigResponse)

instance
  Prelude.NFData
    CreateDeploymentConfigResponse
  where
  rnf CreateDeploymentConfigResponse' {..} =
    Prelude.rnf deploymentConfigId `Prelude.seq`
      Prelude.rnf httpStatus
