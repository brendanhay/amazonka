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
-- Module      : Amazonka.M2.CreateEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a runtime environment for a given runtime engine.
module Amazonka.M2.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_clientToken,
    createEnvironment_description,
    createEnvironment_engineVersion,
    createEnvironment_highAvailabilityConfig,
    createEnvironment_kmsKeyId,
    createEnvironment_preferredMaintenanceWindow,
    createEnvironment_publiclyAccessible,
    createEnvironment_securityGroupIds,
    createEnvironment_storageConfigurations,
    createEnvironment_subnetIds,
    createEnvironment_tags,
    createEnvironment_engineType,
    createEnvironment_instanceType,
    createEnvironment_name,

    -- * Destructuring the Response
    CreateEnvironmentResponse (..),
    newCreateEnvironmentResponse,

    -- * Response Lenses
    createEnvironmentResponse_httpStatus,
    createEnvironmentResponse_environmentId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request to create an environment. The service generates the
    -- clientToken when the API call is triggered. The token expires after one
    -- hour, so if you retry the API within this timeframe with the same
    -- clientToken, you will get the same response. The service also handles
    -- deleting the clientToken after it expires.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the runtime environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The version of the engine type for the runtime environment.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The details of a high availability configuration for this runtime
    -- environment.
    highAvailabilityConfig :: Prelude.Maybe HighAvailabilityConfig,
    -- | The identifier of a customer managed key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Configures the maintenance window you want for the runtime environment.
    -- If you do not provide a value, a random system-generated value will be
    -- assigned.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the runtime environment is publicly accessible.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The list of security groups for the VPC associated with this runtime
    -- environment.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Optional. The storage configurations for this runtime environment.
    storageConfigurations :: Prelude.Maybe [StorageConfiguration],
    -- | The list of subnets associated with the VPC for this runtime
    -- environment.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags for the runtime environment.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The engine type for the runtime environment.
    engineType :: EngineType,
    -- | The type of instance for the runtime environment.
    instanceType :: Prelude.Text,
    -- | The name of the runtime environment. Must be unique within the account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createEnvironment_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request to create an environment. The service generates the
-- clientToken when the API call is triggered. The token expires after one
-- hour, so if you retry the API within this timeframe with the same
-- clientToken, you will get the same response. The service also handles
-- deleting the clientToken after it expires.
--
-- 'description', 'createEnvironment_description' - The description of the runtime environment.
--
-- 'engineVersion', 'createEnvironment_engineVersion' - The version of the engine type for the runtime environment.
--
-- 'highAvailabilityConfig', 'createEnvironment_highAvailabilityConfig' - The details of a high availability configuration for this runtime
-- environment.
--
-- 'kmsKeyId', 'createEnvironment_kmsKeyId' - The identifier of a customer managed key.
--
-- 'preferredMaintenanceWindow', 'createEnvironment_preferredMaintenanceWindow' - Configures the maintenance window you want for the runtime environment.
-- If you do not provide a value, a random system-generated value will be
-- assigned.
--
-- 'publiclyAccessible', 'createEnvironment_publiclyAccessible' - Specifies whether the runtime environment is publicly accessible.
--
-- 'securityGroupIds', 'createEnvironment_securityGroupIds' - The list of security groups for the VPC associated with this runtime
-- environment.
--
-- 'storageConfigurations', 'createEnvironment_storageConfigurations' - Optional. The storage configurations for this runtime environment.
--
-- 'subnetIds', 'createEnvironment_subnetIds' - The list of subnets associated with the VPC for this runtime
-- environment.
--
-- 'tags', 'createEnvironment_tags' - The tags for the runtime environment.
--
-- 'engineType', 'createEnvironment_engineType' - The engine type for the runtime environment.
--
-- 'instanceType', 'createEnvironment_instanceType' - The type of instance for the runtime environment.
--
-- 'name', 'createEnvironment_name' - The name of the runtime environment. Must be unique within the account.
newCreateEnvironment ::
  -- | 'engineType'
  EngineType ->
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateEnvironment
newCreateEnvironment
  pEngineType_
  pInstanceType_
  pName_ =
    CreateEnvironment'
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        highAvailabilityConfig = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        storageConfigurations = Prelude.Nothing,
        subnetIds = Prelude.Nothing,
        tags = Prelude.Nothing,
        engineType = pEngineType_,
        instanceType = pInstanceType_,
        name = pName_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request to create an environment. The service generates the
-- clientToken when the API call is triggered. The token expires after one
-- hour, so if you retry the API within this timeframe with the same
-- clientToken, you will get the same response. The service also handles
-- deleting the clientToken after it expires.
createEnvironment_clientToken :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_clientToken = Lens.lens (\CreateEnvironment' {clientToken} -> clientToken) (\s@CreateEnvironment' {} a -> s {clientToken = a} :: CreateEnvironment)

-- | The description of the runtime environment.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment)

-- | The version of the engine type for the runtime environment.
createEnvironment_engineVersion :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_engineVersion = Lens.lens (\CreateEnvironment' {engineVersion} -> engineVersion) (\s@CreateEnvironment' {} a -> s {engineVersion = a} :: CreateEnvironment)

-- | The details of a high availability configuration for this runtime
-- environment.
createEnvironment_highAvailabilityConfig :: Lens.Lens' CreateEnvironment (Prelude.Maybe HighAvailabilityConfig)
createEnvironment_highAvailabilityConfig = Lens.lens (\CreateEnvironment' {highAvailabilityConfig} -> highAvailabilityConfig) (\s@CreateEnvironment' {} a -> s {highAvailabilityConfig = a} :: CreateEnvironment)

-- | The identifier of a customer managed key.
createEnvironment_kmsKeyId :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_kmsKeyId = Lens.lens (\CreateEnvironment' {kmsKeyId} -> kmsKeyId) (\s@CreateEnvironment' {} a -> s {kmsKeyId = a} :: CreateEnvironment)

-- | Configures the maintenance window you want for the runtime environment.
-- If you do not provide a value, a random system-generated value will be
-- assigned.
createEnvironment_preferredMaintenanceWindow :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_preferredMaintenanceWindow = Lens.lens (\CreateEnvironment' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateEnvironment' {} a -> s {preferredMaintenanceWindow = a} :: CreateEnvironment)

-- | Specifies whether the runtime environment is publicly accessible.
createEnvironment_publiclyAccessible :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Bool)
createEnvironment_publiclyAccessible = Lens.lens (\CreateEnvironment' {publiclyAccessible} -> publiclyAccessible) (\s@CreateEnvironment' {} a -> s {publiclyAccessible = a} :: CreateEnvironment)

-- | The list of security groups for the VPC associated with this runtime
-- environment.
createEnvironment_securityGroupIds :: Lens.Lens' CreateEnvironment (Prelude.Maybe [Prelude.Text])
createEnvironment_securityGroupIds = Lens.lens (\CreateEnvironment' {securityGroupIds} -> securityGroupIds) (\s@CreateEnvironment' {} a -> s {securityGroupIds = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | Optional. The storage configurations for this runtime environment.
createEnvironment_storageConfigurations :: Lens.Lens' CreateEnvironment (Prelude.Maybe [StorageConfiguration])
createEnvironment_storageConfigurations = Lens.lens (\CreateEnvironment' {storageConfigurations} -> storageConfigurations) (\s@CreateEnvironment' {} a -> s {storageConfigurations = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The list of subnets associated with the VPC for this runtime
-- environment.
createEnvironment_subnetIds :: Lens.Lens' CreateEnvironment (Prelude.Maybe [Prelude.Text])
createEnvironment_subnetIds = Lens.lens (\CreateEnvironment' {subnetIds} -> subnetIds) (\s@CreateEnvironment' {} a -> s {subnetIds = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The tags for the runtime environment.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The engine type for the runtime environment.
createEnvironment_engineType :: Lens.Lens' CreateEnvironment EngineType
createEnvironment_engineType = Lens.lens (\CreateEnvironment' {engineType} -> engineType) (\s@CreateEnvironment' {} a -> s {engineType = a} :: CreateEnvironment)

-- | The type of instance for the runtime environment.
createEnvironment_instanceType :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_instanceType = Lens.lens (\CreateEnvironment' {instanceType} -> instanceType) (\s@CreateEnvironment' {} a -> s {instanceType = a} :: CreateEnvironment)

-- | The name of the runtime environment. Must be unique within the account.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type
    AWSResponse CreateEnvironment =
      CreateEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentId")
      )

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt _salt CreateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` highAvailabilityConfig
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` storageConfigurations
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf highAvailabilityConfig
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf storageConfigurations
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("engineVersion" Data..=) Prelude.<$> engineVersion,
            ("highAvailabilityConfig" Data..=)
              Prelude.<$> highAvailabilityConfig,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("preferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("publiclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("storageConfigurations" Data..=)
              Prelude.<$> storageConfigurations,
            ("subnetIds" Data..=) Prelude.<$> subnetIds,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("engineType" Data..= engineType),
            Prelude.Just ("instanceType" Data..= instanceType),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateEnvironment where
  toPath = Prelude.const "/environments"

instance Data.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentResponse' smart constructor.
data CreateEnvironmentResponse = CreateEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the runtime environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEnvironmentResponse_httpStatus' - The response's http status code.
--
-- 'environmentId', 'createEnvironmentResponse_environmentId' - The unique identifier of the runtime environment.
newCreateEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentId'
  Prelude.Text ->
  CreateEnvironmentResponse
newCreateEnvironmentResponse
  pHttpStatus_
  pEnvironmentId_ =
    CreateEnvironmentResponse'
      { httpStatus =
          pHttpStatus_,
        environmentId = pEnvironmentId_
      }

-- | The response's http status code.
createEnvironmentResponse_httpStatus :: Lens.Lens' CreateEnvironmentResponse Prelude.Int
createEnvironmentResponse_httpStatus = Lens.lens (\CreateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentResponse)

-- | The unique identifier of the runtime environment.
createEnvironmentResponse_environmentId :: Lens.Lens' CreateEnvironmentResponse Prelude.Text
createEnvironmentResponse_environmentId = Lens.lens (\CreateEnvironmentResponse' {environmentId} -> environmentId) (\s@CreateEnvironmentResponse' {} a -> s {environmentId = a} :: CreateEnvironmentResponse)

instance Prelude.NFData CreateEnvironmentResponse where
  rnf CreateEnvironmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentId
