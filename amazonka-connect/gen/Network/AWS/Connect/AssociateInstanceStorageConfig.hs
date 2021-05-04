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
-- Module      : Network.AWS.Connect.AssociateInstanceStorageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Associates a storage resource type for the first time. You can only
-- associate one type of storage configuration in a single call. This
-- means, for example, that you can\'t define an instance with multiple S3
-- buckets for storing chat transcripts.
--
-- This API does not create a resource that doesn\'t exist. It only
-- associates it to the instance. Ensure that the resource being specified
-- in the storage configuration, like an S3 bucket, exists when being used
-- for association.
module Network.AWS.Connect.AssociateInstanceStorageConfig
  ( -- * Creating a Request
    AssociateInstanceStorageConfig (..),
    newAssociateInstanceStorageConfig,

    -- * Request Lenses
    associateInstanceStorageConfig_instanceId,
    associateInstanceStorageConfig_resourceType,
    associateInstanceStorageConfig_storageConfig,

    -- * Destructuring the Response
    AssociateInstanceStorageConfigResponse (..),
    newAssociateInstanceStorageConfigResponse,

    -- * Response Lenses
    associateInstanceStorageConfigResponse_associationId,
    associateInstanceStorageConfigResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateInstanceStorageConfig' smart constructor.
data AssociateInstanceStorageConfig = AssociateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType,
    -- | A valid storage type.
    storageConfig :: InstanceStorageConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateInstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'associateInstanceStorageConfig_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'resourceType', 'associateInstanceStorageConfig_resourceType' - A valid resource type.
--
-- 'storageConfig', 'associateInstanceStorageConfig_storageConfig' - A valid storage type.
newAssociateInstanceStorageConfig ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  -- | 'storageConfig'
  InstanceStorageConfig ->
  AssociateInstanceStorageConfig
newAssociateInstanceStorageConfig
  pInstanceId_
  pResourceType_
  pStorageConfig_ =
    AssociateInstanceStorageConfig'
      { instanceId =
          pInstanceId_,
        resourceType = pResourceType_,
        storageConfig = pStorageConfig_
      }

-- | The identifier of the Amazon Connect instance.
associateInstanceStorageConfig_instanceId :: Lens.Lens' AssociateInstanceStorageConfig Prelude.Text
associateInstanceStorageConfig_instanceId = Lens.lens (\AssociateInstanceStorageConfig' {instanceId} -> instanceId) (\s@AssociateInstanceStorageConfig' {} a -> s {instanceId = a} :: AssociateInstanceStorageConfig)

-- | A valid resource type.
associateInstanceStorageConfig_resourceType :: Lens.Lens' AssociateInstanceStorageConfig InstanceStorageResourceType
associateInstanceStorageConfig_resourceType = Lens.lens (\AssociateInstanceStorageConfig' {resourceType} -> resourceType) (\s@AssociateInstanceStorageConfig' {} a -> s {resourceType = a} :: AssociateInstanceStorageConfig)

-- | A valid storage type.
associateInstanceStorageConfig_storageConfig :: Lens.Lens' AssociateInstanceStorageConfig InstanceStorageConfig
associateInstanceStorageConfig_storageConfig = Lens.lens (\AssociateInstanceStorageConfig' {storageConfig} -> storageConfig) (\s@AssociateInstanceStorageConfig' {} a -> s {storageConfig = a} :: AssociateInstanceStorageConfig)

instance
  Prelude.AWSRequest
    AssociateInstanceStorageConfig
  where
  type
    Rs AssociateInstanceStorageConfig =
      AssociateInstanceStorageConfigResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateInstanceStorageConfigResponse'
            Prelude.<$> (x Prelude..?> "AssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateInstanceStorageConfig

instance
  Prelude.NFData
    AssociateInstanceStorageConfig

instance
  Prelude.ToHeaders
    AssociateInstanceStorageConfig
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

instance
  Prelude.ToJSON
    AssociateInstanceStorageConfig
  where
  toJSON AssociateInstanceStorageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceType" Prelude..= resourceType),
            Prelude.Just
              ("StorageConfig" Prelude..= storageConfig)
          ]
      )

instance
  Prelude.ToPath
    AssociateInstanceStorageConfig
  where
  toPath AssociateInstanceStorageConfig' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/storage-config"
      ]

instance
  Prelude.ToQuery
    AssociateInstanceStorageConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateInstanceStorageConfigResponse' smart constructor.
data AssociateInstanceStorageConfigResponse = AssociateInstanceStorageConfigResponse'
  { -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateInstanceStorageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'associateInstanceStorageConfigResponse_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 'httpStatus', 'associateInstanceStorageConfigResponse_httpStatus' - The response's http status code.
newAssociateInstanceStorageConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateInstanceStorageConfigResponse
newAssociateInstanceStorageConfigResponse
  pHttpStatus_ =
    AssociateInstanceStorageConfigResponse'
      { associationId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
associateInstanceStorageConfigResponse_associationId :: Lens.Lens' AssociateInstanceStorageConfigResponse (Prelude.Maybe Prelude.Text)
associateInstanceStorageConfigResponse_associationId = Lens.lens (\AssociateInstanceStorageConfigResponse' {associationId} -> associationId) (\s@AssociateInstanceStorageConfigResponse' {} a -> s {associationId = a} :: AssociateInstanceStorageConfigResponse)

-- | The response's http status code.
associateInstanceStorageConfigResponse_httpStatus :: Lens.Lens' AssociateInstanceStorageConfigResponse Prelude.Int
associateInstanceStorageConfigResponse_httpStatus = Lens.lens (\AssociateInstanceStorageConfigResponse' {httpStatus} -> httpStatus) (\s@AssociateInstanceStorageConfigResponse' {} a -> s {httpStatus = a} :: AssociateInstanceStorageConfigResponse)

instance
  Prelude.NFData
    AssociateInstanceStorageConfigResponse
