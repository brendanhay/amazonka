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
-- Module      : Amazonka.Connect.UpdateInstanceStorageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates an existing configuration for a resource type. This API is
-- idempotent.
module Amazonka.Connect.UpdateInstanceStorageConfig
  ( -- * Creating a Request
    UpdateInstanceStorageConfig (..),
    newUpdateInstanceStorageConfig,

    -- * Request Lenses
    updateInstanceStorageConfig_instanceId,
    updateInstanceStorageConfig_associationId,
    updateInstanceStorageConfig_resourceType,
    updateInstanceStorageConfig_storageConfig,

    -- * Destructuring the Response
    UpdateInstanceStorageConfigResponse (..),
    newUpdateInstanceStorageConfigResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInstanceStorageConfig' smart constructor.
data UpdateInstanceStorageConfig = UpdateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType,
    storageConfig :: InstanceStorageConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateInstanceStorageConfig_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'associationId', 'updateInstanceStorageConfig_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 'resourceType', 'updateInstanceStorageConfig_resourceType' - A valid resource type.
--
-- 'storageConfig', 'updateInstanceStorageConfig_storageConfig' - Undocumented member.
newUpdateInstanceStorageConfig ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'associationId'
  Prelude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  -- | 'storageConfig'
  InstanceStorageConfig ->
  UpdateInstanceStorageConfig
newUpdateInstanceStorageConfig
  pInstanceId_
  pAssociationId_
  pResourceType_
  pStorageConfig_ =
    UpdateInstanceStorageConfig'
      { instanceId =
          pInstanceId_,
        associationId = pAssociationId_,
        resourceType = pResourceType_,
        storageConfig = pStorageConfig_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateInstanceStorageConfig_instanceId :: Lens.Lens' UpdateInstanceStorageConfig Prelude.Text
updateInstanceStorageConfig_instanceId = Lens.lens (\UpdateInstanceStorageConfig' {instanceId} -> instanceId) (\s@UpdateInstanceStorageConfig' {} a -> s {instanceId = a} :: UpdateInstanceStorageConfig)

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
updateInstanceStorageConfig_associationId :: Lens.Lens' UpdateInstanceStorageConfig Prelude.Text
updateInstanceStorageConfig_associationId = Lens.lens (\UpdateInstanceStorageConfig' {associationId} -> associationId) (\s@UpdateInstanceStorageConfig' {} a -> s {associationId = a} :: UpdateInstanceStorageConfig)

-- | A valid resource type.
updateInstanceStorageConfig_resourceType :: Lens.Lens' UpdateInstanceStorageConfig InstanceStorageResourceType
updateInstanceStorageConfig_resourceType = Lens.lens (\UpdateInstanceStorageConfig' {resourceType} -> resourceType) (\s@UpdateInstanceStorageConfig' {} a -> s {resourceType = a} :: UpdateInstanceStorageConfig)

-- | Undocumented member.
updateInstanceStorageConfig_storageConfig :: Lens.Lens' UpdateInstanceStorageConfig InstanceStorageConfig
updateInstanceStorageConfig_storageConfig = Lens.lens (\UpdateInstanceStorageConfig' {storageConfig} -> storageConfig) (\s@UpdateInstanceStorageConfig' {} a -> s {storageConfig = a} :: UpdateInstanceStorageConfig)

instance Core.AWSRequest UpdateInstanceStorageConfig where
  type
    AWSResponse UpdateInstanceStorageConfig =
      UpdateInstanceStorageConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateInstanceStorageConfigResponse'

instance Prelude.Hashable UpdateInstanceStorageConfig where
  hashWithSalt _salt UpdateInstanceStorageConfig' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` storageConfig

instance Prelude.NFData UpdateInstanceStorageConfig where
  rnf UpdateInstanceStorageConfig' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf storageConfig

instance Data.ToHeaders UpdateInstanceStorageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInstanceStorageConfig where
  toJSON UpdateInstanceStorageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StorageConfig" Data..= storageConfig)
          ]
      )

instance Data.ToPath UpdateInstanceStorageConfig where
  toPath UpdateInstanceStorageConfig' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/storage-config/",
        Data.toBS associationId
      ]

instance Data.ToQuery UpdateInstanceStorageConfig where
  toQuery UpdateInstanceStorageConfig' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newUpdateInstanceStorageConfigResponse' smart constructor.
data UpdateInstanceStorageConfigResponse = UpdateInstanceStorageConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceStorageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateInstanceStorageConfigResponse ::
  UpdateInstanceStorageConfigResponse
newUpdateInstanceStorageConfigResponse =
  UpdateInstanceStorageConfigResponse'

instance
  Prelude.NFData
    UpdateInstanceStorageConfigResponse
  where
  rnf _ = ()
