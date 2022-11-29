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
-- Module      : Amazonka.M2.GetEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specific runtime environment.
module Amazonka.M2.GetEnvironment
  ( -- * Creating a Request
    GetEnvironment (..),
    newGetEnvironment,

    -- * Request Lenses
    getEnvironment_environmentId,

    -- * Destructuring the Response
    GetEnvironmentResponse (..),
    newGetEnvironmentResponse,

    -- * Response Lenses
    getEnvironmentResponse_tags,
    getEnvironmentResponse_actualCapacity,
    getEnvironmentResponse_pendingMaintenance,
    getEnvironmentResponse_statusReason,
    getEnvironmentResponse_storageConfigurations,
    getEnvironmentResponse_loadBalancerArn,
    getEnvironmentResponse_description,
    getEnvironmentResponse_publiclyAccessible,
    getEnvironmentResponse_preferredMaintenanceWindow,
    getEnvironmentResponse_highAvailabilityConfig,
    getEnvironmentResponse_httpStatus,
    getEnvironmentResponse_creationTime,
    getEnvironmentResponse_engineType,
    getEnvironmentResponse_engineVersion,
    getEnvironmentResponse_environmentArn,
    getEnvironmentResponse_environmentId,
    getEnvironmentResponse_instanceType,
    getEnvironmentResponse_name,
    getEnvironmentResponse_securityGroupIds,
    getEnvironmentResponse_status,
    getEnvironmentResponse_subnetIds,
    getEnvironmentResponse_vpcId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEnvironment' smart constructor.
data GetEnvironment = GetEnvironment'
  { -- | The unique identifier of the runtime environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'getEnvironment_environmentId' - The unique identifier of the runtime environment.
newGetEnvironment ::
  -- | 'environmentId'
  Prelude.Text ->
  GetEnvironment
newGetEnvironment pEnvironmentId_ =
  GetEnvironment' {environmentId = pEnvironmentId_}

-- | The unique identifier of the runtime environment.
getEnvironment_environmentId :: Lens.Lens' GetEnvironment Prelude.Text
getEnvironment_environmentId = Lens.lens (\GetEnvironment' {environmentId} -> environmentId) (\s@GetEnvironment' {} a -> s {environmentId = a} :: GetEnvironment)

instance Core.AWSRequest GetEnvironment where
  type
    AWSResponse GetEnvironment =
      GetEnvironmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnvironmentResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "actualCapacity")
            Prelude.<*> (x Core..?> "pendingMaintenance")
            Prelude.<*> (x Core..?> "statusReason")
            Prelude.<*> ( x Core..?> "storageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "loadBalancerArn")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "publiclyAccessible")
            Prelude.<*> (x Core..?> "preferredMaintenanceWindow")
            Prelude.<*> (x Core..?> "highAvailabilityConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "creationTime")
            Prelude.<*> (x Core..:> "engineType")
            Prelude.<*> (x Core..:> "engineVersion")
            Prelude.<*> (x Core..:> "environmentArn")
            Prelude.<*> (x Core..:> "environmentId")
            Prelude.<*> (x Core..:> "instanceType")
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> ( x Core..?> "securityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..:> "status")
            Prelude.<*> (x Core..?> "subnetIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..:> "vpcId")
      )

instance Prelude.Hashable GetEnvironment where
  hashWithSalt _salt GetEnvironment' {..} =
    _salt `Prelude.hashWithSalt` environmentId

instance Prelude.NFData GetEnvironment where
  rnf GetEnvironment' {..} = Prelude.rnf environmentId

instance Core.ToHeaders GetEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEnvironment where
  toPath GetEnvironment' {..} =
    Prelude.mconcat
      ["/environments/", Core.toBS environmentId]

instance Core.ToQuery GetEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnvironmentResponse' smart constructor.
data GetEnvironmentResponse = GetEnvironmentResponse'
  { -- | The tags defined for this runtime environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of instances included in the runtime environment. A
    -- standalone runtime environment has a maxiumum of one instance.
    -- Currently, a high availability runtime environment has a maximum of two
    -- instances.
    actualCapacity :: Prelude.Maybe Prelude.Natural,
    -- | Indicates the pending maintenance scheduled on this environment.
    pendingMaintenance :: Prelude.Maybe PendingMaintenance,
    -- | The reason for the reported status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The storage configurations defined for the runtime environment.
    storageConfigurations :: Prelude.Maybe [StorageConfiguration],
    -- | The Amazon Resource Name (ARN) for the load balancer used with the
    -- runtime environment.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the runtime environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether applications running in this runtime environment are publicly
    -- accessible.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Configures a desired maintenance window for the environment. If you do
    -- not provide a value, a random system-generated value will be assigned.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The desired capacity of the high availability configuration for the
    -- runtime environment.
    highAvailabilityConfig :: Prelude.Maybe HighAvailabilityConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp when the runtime environment was created.
    creationTime :: Core.POSIX,
    -- | The target platform for the runtime environment.
    engineType :: EngineType,
    -- | The version of the runtime engine.
    engineVersion :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the runtime environment.
    environmentArn :: Prelude.Text,
    -- | The unique identifier of the runtime environment.
    environmentId :: Prelude.Text,
    -- | The type of instance underlying the runtime environment.
    instanceType :: Prelude.Text,
    -- | The name of the runtime environment.
    name :: Prelude.Text,
    -- | The unique identifiers of the security groups assigned to this runtime
    -- environment.
    securityGroupIds :: [Prelude.Text],
    -- | The status of the runtime environment.
    status :: EnvironmentLifecycle,
    -- | The unique identifiers of the subnets assigned to this runtime
    -- environment.
    subnetIds :: [Prelude.Text],
    -- | The unique identifier for the VPC used with this runtime environment.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getEnvironmentResponse_tags' - The tags defined for this runtime environment.
--
-- 'actualCapacity', 'getEnvironmentResponse_actualCapacity' - The number of instances included in the runtime environment. A
-- standalone runtime environment has a maxiumum of one instance.
-- Currently, a high availability runtime environment has a maximum of two
-- instances.
--
-- 'pendingMaintenance', 'getEnvironmentResponse_pendingMaintenance' - Indicates the pending maintenance scheduled on this environment.
--
-- 'statusReason', 'getEnvironmentResponse_statusReason' - The reason for the reported status.
--
-- 'storageConfigurations', 'getEnvironmentResponse_storageConfigurations' - The storage configurations defined for the runtime environment.
--
-- 'loadBalancerArn', 'getEnvironmentResponse_loadBalancerArn' - The Amazon Resource Name (ARN) for the load balancer used with the
-- runtime environment.
--
-- 'description', 'getEnvironmentResponse_description' - The description of the runtime environment.
--
-- 'publiclyAccessible', 'getEnvironmentResponse_publiclyAccessible' - Whether applications running in this runtime environment are publicly
-- accessible.
--
-- 'preferredMaintenanceWindow', 'getEnvironmentResponse_preferredMaintenanceWindow' - Configures a desired maintenance window for the environment. If you do
-- not provide a value, a random system-generated value will be assigned.
--
-- 'highAvailabilityConfig', 'getEnvironmentResponse_highAvailabilityConfig' - The desired capacity of the high availability configuration for the
-- runtime environment.
--
-- 'httpStatus', 'getEnvironmentResponse_httpStatus' - The response's http status code.
--
-- 'creationTime', 'getEnvironmentResponse_creationTime' - The timestamp when the runtime environment was created.
--
-- 'engineType', 'getEnvironmentResponse_engineType' - The target platform for the runtime environment.
--
-- 'engineVersion', 'getEnvironmentResponse_engineVersion' - The version of the runtime engine.
--
-- 'environmentArn', 'getEnvironmentResponse_environmentArn' - The Amazon Resource Name (ARN) of the runtime environment.
--
-- 'environmentId', 'getEnvironmentResponse_environmentId' - The unique identifier of the runtime environment.
--
-- 'instanceType', 'getEnvironmentResponse_instanceType' - The type of instance underlying the runtime environment.
--
-- 'name', 'getEnvironmentResponse_name' - The name of the runtime environment.
--
-- 'securityGroupIds', 'getEnvironmentResponse_securityGroupIds' - The unique identifiers of the security groups assigned to this runtime
-- environment.
--
-- 'status', 'getEnvironmentResponse_status' - The status of the runtime environment.
--
-- 'subnetIds', 'getEnvironmentResponse_subnetIds' - The unique identifiers of the subnets assigned to this runtime
-- environment.
--
-- 'vpcId', 'getEnvironmentResponse_vpcId' - The unique identifier for the VPC used with this runtime environment.
newGetEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'engineType'
  EngineType ->
  -- | 'engineVersion'
  Prelude.Text ->
  -- | 'environmentArn'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  EnvironmentLifecycle ->
  -- | 'vpcId'
  Prelude.Text ->
  GetEnvironmentResponse
newGetEnvironmentResponse
  pHttpStatus_
  pCreationTime_
  pEngineType_
  pEngineVersion_
  pEnvironmentArn_
  pEnvironmentId_
  pInstanceType_
  pName_
  pStatus_
  pVpcId_ =
    GetEnvironmentResponse'
      { tags = Prelude.Nothing,
        actualCapacity = Prelude.Nothing,
        pendingMaintenance = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        storageConfigurations = Prelude.Nothing,
        loadBalancerArn = Prelude.Nothing,
        description = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        highAvailabilityConfig = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        creationTime = Core._Time Lens.# pCreationTime_,
        engineType = pEngineType_,
        engineVersion = pEngineVersion_,
        environmentArn = pEnvironmentArn_,
        environmentId = pEnvironmentId_,
        instanceType = pInstanceType_,
        name = pName_,
        securityGroupIds = Prelude.mempty,
        status = pStatus_,
        subnetIds = Prelude.mempty,
        vpcId = pVpcId_
      }

-- | The tags defined for this runtime environment.
getEnvironmentResponse_tags :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getEnvironmentResponse_tags = Lens.lens (\GetEnvironmentResponse' {tags} -> tags) (\s@GetEnvironmentResponse' {} a -> s {tags = a} :: GetEnvironmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of instances included in the runtime environment. A
-- standalone runtime environment has a maxiumum of one instance.
-- Currently, a high availability runtime environment has a maximum of two
-- instances.
getEnvironmentResponse_actualCapacity :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Natural)
getEnvironmentResponse_actualCapacity = Lens.lens (\GetEnvironmentResponse' {actualCapacity} -> actualCapacity) (\s@GetEnvironmentResponse' {} a -> s {actualCapacity = a} :: GetEnvironmentResponse)

-- | Indicates the pending maintenance scheduled on this environment.
getEnvironmentResponse_pendingMaintenance :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe PendingMaintenance)
getEnvironmentResponse_pendingMaintenance = Lens.lens (\GetEnvironmentResponse' {pendingMaintenance} -> pendingMaintenance) (\s@GetEnvironmentResponse' {} a -> s {pendingMaintenance = a} :: GetEnvironmentResponse)

-- | The reason for the reported status.
getEnvironmentResponse_statusReason :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_statusReason = Lens.lens (\GetEnvironmentResponse' {statusReason} -> statusReason) (\s@GetEnvironmentResponse' {} a -> s {statusReason = a} :: GetEnvironmentResponse)

-- | The storage configurations defined for the runtime environment.
getEnvironmentResponse_storageConfigurations :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe [StorageConfiguration])
getEnvironmentResponse_storageConfigurations = Lens.lens (\GetEnvironmentResponse' {storageConfigurations} -> storageConfigurations) (\s@GetEnvironmentResponse' {} a -> s {storageConfigurations = a} :: GetEnvironmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the load balancer used with the
-- runtime environment.
getEnvironmentResponse_loadBalancerArn :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_loadBalancerArn = Lens.lens (\GetEnvironmentResponse' {loadBalancerArn} -> loadBalancerArn) (\s@GetEnvironmentResponse' {} a -> s {loadBalancerArn = a} :: GetEnvironmentResponse)

-- | The description of the runtime environment.
getEnvironmentResponse_description :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_description = Lens.lens (\GetEnvironmentResponse' {description} -> description) (\s@GetEnvironmentResponse' {} a -> s {description = a} :: GetEnvironmentResponse)

-- | Whether applications running in this runtime environment are publicly
-- accessible.
getEnvironmentResponse_publiclyAccessible :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Bool)
getEnvironmentResponse_publiclyAccessible = Lens.lens (\GetEnvironmentResponse' {publiclyAccessible} -> publiclyAccessible) (\s@GetEnvironmentResponse' {} a -> s {publiclyAccessible = a} :: GetEnvironmentResponse)

-- | Configures a desired maintenance window for the environment. If you do
-- not provide a value, a random system-generated value will be assigned.
getEnvironmentResponse_preferredMaintenanceWindow :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Prelude.Text)
getEnvironmentResponse_preferredMaintenanceWindow = Lens.lens (\GetEnvironmentResponse' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@GetEnvironmentResponse' {} a -> s {preferredMaintenanceWindow = a} :: GetEnvironmentResponse)

-- | The desired capacity of the high availability configuration for the
-- runtime environment.
getEnvironmentResponse_highAvailabilityConfig :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe HighAvailabilityConfig)
getEnvironmentResponse_highAvailabilityConfig = Lens.lens (\GetEnvironmentResponse' {highAvailabilityConfig} -> highAvailabilityConfig) (\s@GetEnvironmentResponse' {} a -> s {highAvailabilityConfig = a} :: GetEnvironmentResponse)

-- | The response's http status code.
getEnvironmentResponse_httpStatus :: Lens.Lens' GetEnvironmentResponse Prelude.Int
getEnvironmentResponse_httpStatus = Lens.lens (\GetEnvironmentResponse' {httpStatus} -> httpStatus) (\s@GetEnvironmentResponse' {} a -> s {httpStatus = a} :: GetEnvironmentResponse)

-- | The timestamp when the runtime environment was created.
getEnvironmentResponse_creationTime :: Lens.Lens' GetEnvironmentResponse Prelude.UTCTime
getEnvironmentResponse_creationTime = Lens.lens (\GetEnvironmentResponse' {creationTime} -> creationTime) (\s@GetEnvironmentResponse' {} a -> s {creationTime = a} :: GetEnvironmentResponse) Prelude.. Core._Time

-- | The target platform for the runtime environment.
getEnvironmentResponse_engineType :: Lens.Lens' GetEnvironmentResponse EngineType
getEnvironmentResponse_engineType = Lens.lens (\GetEnvironmentResponse' {engineType} -> engineType) (\s@GetEnvironmentResponse' {} a -> s {engineType = a} :: GetEnvironmentResponse)

-- | The version of the runtime engine.
getEnvironmentResponse_engineVersion :: Lens.Lens' GetEnvironmentResponse Prelude.Text
getEnvironmentResponse_engineVersion = Lens.lens (\GetEnvironmentResponse' {engineVersion} -> engineVersion) (\s@GetEnvironmentResponse' {} a -> s {engineVersion = a} :: GetEnvironmentResponse)

-- | The Amazon Resource Name (ARN) of the runtime environment.
getEnvironmentResponse_environmentArn :: Lens.Lens' GetEnvironmentResponse Prelude.Text
getEnvironmentResponse_environmentArn = Lens.lens (\GetEnvironmentResponse' {environmentArn} -> environmentArn) (\s@GetEnvironmentResponse' {} a -> s {environmentArn = a} :: GetEnvironmentResponse)

-- | The unique identifier of the runtime environment.
getEnvironmentResponse_environmentId :: Lens.Lens' GetEnvironmentResponse Prelude.Text
getEnvironmentResponse_environmentId = Lens.lens (\GetEnvironmentResponse' {environmentId} -> environmentId) (\s@GetEnvironmentResponse' {} a -> s {environmentId = a} :: GetEnvironmentResponse)

-- | The type of instance underlying the runtime environment.
getEnvironmentResponse_instanceType :: Lens.Lens' GetEnvironmentResponse Prelude.Text
getEnvironmentResponse_instanceType = Lens.lens (\GetEnvironmentResponse' {instanceType} -> instanceType) (\s@GetEnvironmentResponse' {} a -> s {instanceType = a} :: GetEnvironmentResponse)

-- | The name of the runtime environment.
getEnvironmentResponse_name :: Lens.Lens' GetEnvironmentResponse Prelude.Text
getEnvironmentResponse_name = Lens.lens (\GetEnvironmentResponse' {name} -> name) (\s@GetEnvironmentResponse' {} a -> s {name = a} :: GetEnvironmentResponse)

-- | The unique identifiers of the security groups assigned to this runtime
-- environment.
getEnvironmentResponse_securityGroupIds :: Lens.Lens' GetEnvironmentResponse [Prelude.Text]
getEnvironmentResponse_securityGroupIds = Lens.lens (\GetEnvironmentResponse' {securityGroupIds} -> securityGroupIds) (\s@GetEnvironmentResponse' {} a -> s {securityGroupIds = a} :: GetEnvironmentResponse) Prelude.. Lens.coerced

-- | The status of the runtime environment.
getEnvironmentResponse_status :: Lens.Lens' GetEnvironmentResponse EnvironmentLifecycle
getEnvironmentResponse_status = Lens.lens (\GetEnvironmentResponse' {status} -> status) (\s@GetEnvironmentResponse' {} a -> s {status = a} :: GetEnvironmentResponse)

-- | The unique identifiers of the subnets assigned to this runtime
-- environment.
getEnvironmentResponse_subnetIds :: Lens.Lens' GetEnvironmentResponse [Prelude.Text]
getEnvironmentResponse_subnetIds = Lens.lens (\GetEnvironmentResponse' {subnetIds} -> subnetIds) (\s@GetEnvironmentResponse' {} a -> s {subnetIds = a} :: GetEnvironmentResponse) Prelude.. Lens.coerced

-- | The unique identifier for the VPC used with this runtime environment.
getEnvironmentResponse_vpcId :: Lens.Lens' GetEnvironmentResponse Prelude.Text
getEnvironmentResponse_vpcId = Lens.lens (\GetEnvironmentResponse' {vpcId} -> vpcId) (\s@GetEnvironmentResponse' {} a -> s {vpcId = a} :: GetEnvironmentResponse)

instance Prelude.NFData GetEnvironmentResponse where
  rnf GetEnvironmentResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf actualCapacity
      `Prelude.seq` Prelude.rnf pendingMaintenance
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf storageConfigurations
      `Prelude.seq` Prelude.rnf loadBalancerArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf highAvailabilityConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf environmentArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId
