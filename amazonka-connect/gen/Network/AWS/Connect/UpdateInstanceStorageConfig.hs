{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.UpdateInstanceStorageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates an existing configuration for a resource type. This API is
-- idempotent.
module Network.AWS.Connect.UpdateInstanceStorageConfig
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateInstanceStorageConfig' smart constructor.
data UpdateInstanceStorageConfig = UpdateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType,
    storageConfig :: InstanceStorageConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateInstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateInstanceStorageConfig_instanceId' - The identifier of the Amazon Connect instance.
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

-- | The identifier of the Amazon Connect instance.
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

instance
  Prelude.AWSRequest
    UpdateInstanceStorageConfig
  where
  type
    Rs UpdateInstanceStorageConfig =
      UpdateInstanceStorageConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateInstanceStorageConfigResponse'

instance Prelude.Hashable UpdateInstanceStorageConfig

instance Prelude.NFData UpdateInstanceStorageConfig

instance
  Prelude.ToHeaders
    UpdateInstanceStorageConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateInstanceStorageConfig where
  toJSON UpdateInstanceStorageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StorageConfig" Prelude..= storageConfig)
          ]
      )

instance Prelude.ToPath UpdateInstanceStorageConfig where
  toPath UpdateInstanceStorageConfig' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/storage-config/",
        Prelude.toBS associationId
      ]

instance Prelude.ToQuery UpdateInstanceStorageConfig where
  toQuery UpdateInstanceStorageConfig' {..} =
    Prelude.mconcat
      ["resourceType" Prelude.=: resourceType]

-- | /See:/ 'newUpdateInstanceStorageConfigResponse' smart constructor.
data UpdateInstanceStorageConfigResponse = UpdateInstanceStorageConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
