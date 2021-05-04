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
-- Module      : Network.AWS.Connect.DisassociateInstanceStorageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Removes the storage type configurations for the specified resource type
-- and association ID.
module Network.AWS.Connect.DisassociateInstanceStorageConfig
  ( -- * Creating a Request
    DisassociateInstanceStorageConfig (..),
    newDisassociateInstanceStorageConfig,

    -- * Request Lenses
    disassociateInstanceStorageConfig_instanceId,
    disassociateInstanceStorageConfig_associationId,
    disassociateInstanceStorageConfig_resourceType,

    -- * Destructuring the Response
    DisassociateInstanceStorageConfigResponse (..),
    newDisassociateInstanceStorageConfigResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateInstanceStorageConfig' smart constructor.
data DisassociateInstanceStorageConfig = DisassociateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateInstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateInstanceStorageConfig_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'associationId', 'disassociateInstanceStorageConfig_associationId' - The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
--
-- 'resourceType', 'disassociateInstanceStorageConfig_resourceType' - A valid resource type.
newDisassociateInstanceStorageConfig ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'associationId'
  Prelude.Text ->
  -- | 'resourceType'
  InstanceStorageResourceType ->
  DisassociateInstanceStorageConfig
newDisassociateInstanceStorageConfig
  pInstanceId_
  pAssociationId_
  pResourceType_ =
    DisassociateInstanceStorageConfig'
      { instanceId =
          pInstanceId_,
        associationId = pAssociationId_,
        resourceType = pResourceType_
      }

-- | The identifier of the Amazon Connect instance.
disassociateInstanceStorageConfig_instanceId :: Lens.Lens' DisassociateInstanceStorageConfig Prelude.Text
disassociateInstanceStorageConfig_instanceId = Lens.lens (\DisassociateInstanceStorageConfig' {instanceId} -> instanceId) (\s@DisassociateInstanceStorageConfig' {} a -> s {instanceId = a} :: DisassociateInstanceStorageConfig)

-- | The existing association identifier that uniquely identifies the
-- resource type and storage config for the given instance ID.
disassociateInstanceStorageConfig_associationId :: Lens.Lens' DisassociateInstanceStorageConfig Prelude.Text
disassociateInstanceStorageConfig_associationId = Lens.lens (\DisassociateInstanceStorageConfig' {associationId} -> associationId) (\s@DisassociateInstanceStorageConfig' {} a -> s {associationId = a} :: DisassociateInstanceStorageConfig)

-- | A valid resource type.
disassociateInstanceStorageConfig_resourceType :: Lens.Lens' DisassociateInstanceStorageConfig InstanceStorageResourceType
disassociateInstanceStorageConfig_resourceType = Lens.lens (\DisassociateInstanceStorageConfig' {resourceType} -> resourceType) (\s@DisassociateInstanceStorageConfig' {} a -> s {resourceType = a} :: DisassociateInstanceStorageConfig)

instance
  Prelude.AWSRequest
    DisassociateInstanceStorageConfig
  where
  type
    Rs DisassociateInstanceStorageConfig =
      DisassociateInstanceStorageConfigResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DisassociateInstanceStorageConfigResponse'

instance
  Prelude.Hashable
    DisassociateInstanceStorageConfig

instance
  Prelude.NFData
    DisassociateInstanceStorageConfig

instance
  Prelude.ToHeaders
    DisassociateInstanceStorageConfig
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
  Prelude.ToPath
    DisassociateInstanceStorageConfig
  where
  toPath DisassociateInstanceStorageConfig' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/storage-config/",
        Prelude.toBS associationId
      ]

instance
  Prelude.ToQuery
    DisassociateInstanceStorageConfig
  where
  toQuery DisassociateInstanceStorageConfig' {..} =
    Prelude.mconcat
      ["resourceType" Prelude.=: resourceType]

-- | /See:/ 'newDisassociateInstanceStorageConfigResponse' smart constructor.
data DisassociateInstanceStorageConfigResponse = DisassociateInstanceStorageConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateInstanceStorageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateInstanceStorageConfigResponse ::
  DisassociateInstanceStorageConfigResponse
newDisassociateInstanceStorageConfigResponse =
  DisassociateInstanceStorageConfigResponse'

instance
  Prelude.NFData
    DisassociateInstanceStorageConfigResponse
