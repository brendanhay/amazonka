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
-- Module      : Network.AWS.Connect.DescribeInstanceStorageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Retrieves the current storage configurations for the specified resource
-- type, association ID, and instance ID.
module Network.AWS.Connect.DescribeInstanceStorageConfig
  ( -- * Creating a Request
    DescribeInstanceStorageConfig (..),
    newDescribeInstanceStorageConfig,

    -- * Request Lenses
    describeInstanceStorageConfig_instanceId,
    describeInstanceStorageConfig_associationId,
    describeInstanceStorageConfig_resourceType,

    -- * Destructuring the Response
    DescribeInstanceStorageConfigResponse (..),
    newDescribeInstanceStorageConfigResponse,

    -- * Response Lenses
    describeInstanceStorageConfigResponse_storageConfig,
    describeInstanceStorageConfigResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstanceStorageConfig' smart constructor.
data DescribeInstanceStorageConfig = DescribeInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeInstanceStorageConfig_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'associationId', 'describeInstanceStorageConfig_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 'resourceType', 'describeInstanceStorageConfig_resourceType' - A valid resource type.
newDescribeInstanceStorageConfig ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'associationId'
  Prelude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  DescribeInstanceStorageConfig
newDescribeInstanceStorageConfig
  pInstanceId_
  pAssociationId_
  pResourceType_ =
    DescribeInstanceStorageConfig'
      { instanceId =
          pInstanceId_,
        associationId = pAssociationId_,
        resourceType = pResourceType_
      }

-- | The identifier of the Amazon Connect instance.
describeInstanceStorageConfig_instanceId :: Lens.Lens' DescribeInstanceStorageConfig Prelude.Text
describeInstanceStorageConfig_instanceId = Lens.lens (\DescribeInstanceStorageConfig' {instanceId} -> instanceId) (\s@DescribeInstanceStorageConfig' {} a -> s {instanceId = a} :: DescribeInstanceStorageConfig)

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
describeInstanceStorageConfig_associationId :: Lens.Lens' DescribeInstanceStorageConfig Prelude.Text
describeInstanceStorageConfig_associationId = Lens.lens (\DescribeInstanceStorageConfig' {associationId} -> associationId) (\s@DescribeInstanceStorageConfig' {} a -> s {associationId = a} :: DescribeInstanceStorageConfig)

-- | A valid resource type.
describeInstanceStorageConfig_resourceType :: Lens.Lens' DescribeInstanceStorageConfig InstanceStorageResourceType
describeInstanceStorageConfig_resourceType = Lens.lens (\DescribeInstanceStorageConfig' {resourceType} -> resourceType) (\s@DescribeInstanceStorageConfig' {} a -> s {resourceType = a} :: DescribeInstanceStorageConfig)

instance
  Core.AWSRequest
    DescribeInstanceStorageConfig
  where
  type
    AWSResponse DescribeInstanceStorageConfig =
      DescribeInstanceStorageConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceStorageConfigResponse'
            Prelude.<$> (x Core..?> "StorageConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInstanceStorageConfig

instance Prelude.NFData DescribeInstanceStorageConfig

instance Core.ToHeaders DescribeInstanceStorageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeInstanceStorageConfig where
  toPath DescribeInstanceStorageConfig' {..} =
    Prelude.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/storage-config/",
        Core.toBS associationId
      ]

instance Core.ToQuery DescribeInstanceStorageConfig where
  toQuery DescribeInstanceStorageConfig' {..} =
    Prelude.mconcat
      ["resourceType" Core.=: resourceType]

-- | /See:/ 'newDescribeInstanceStorageConfigResponse' smart constructor.
data DescribeInstanceStorageConfigResponse = DescribeInstanceStorageConfigResponse'
  { -- | A valid storage type.
    storageConfig :: Prelude.Maybe InstanceStorageConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceStorageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageConfig', 'describeInstanceStorageConfigResponse_storageConfig' - A valid storage type.
--
-- 'httpStatus', 'describeInstanceStorageConfigResponse_httpStatus' - The response's http status code.
newDescribeInstanceStorageConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceStorageConfigResponse
newDescribeInstanceStorageConfigResponse pHttpStatus_ =
  DescribeInstanceStorageConfigResponse'
    { storageConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A valid storage type.
describeInstanceStorageConfigResponse_storageConfig :: Lens.Lens' DescribeInstanceStorageConfigResponse (Prelude.Maybe InstanceStorageConfig)
describeInstanceStorageConfigResponse_storageConfig = Lens.lens (\DescribeInstanceStorageConfigResponse' {storageConfig} -> storageConfig) (\s@DescribeInstanceStorageConfigResponse' {} a -> s {storageConfig = a} :: DescribeInstanceStorageConfigResponse)

-- | The response's http status code.
describeInstanceStorageConfigResponse_httpStatus :: Lens.Lens' DescribeInstanceStorageConfigResponse Prelude.Int
describeInstanceStorageConfigResponse_httpStatus = Lens.lens (\DescribeInstanceStorageConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceStorageConfigResponse' {} a -> s {httpStatus = a} :: DescribeInstanceStorageConfigResponse)

instance
  Prelude.NFData
    DescribeInstanceStorageConfigResponse
