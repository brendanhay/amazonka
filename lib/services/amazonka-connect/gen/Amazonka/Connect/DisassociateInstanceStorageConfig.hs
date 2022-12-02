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
-- Module      : Amazonka.Connect.DisassociateInstanceStorageConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Connect.DisassociateInstanceStorageConfig
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateInstanceStorageConfig' smart constructor.
data DisassociateInstanceStorageConfig = DisassociateInstanceStorageConfig'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The existing association identifier that uniquely identifies the
    -- resource type and storage config for the given instance ID.
    associationId :: Prelude.Text,
    -- | A valid resource type.
    resourceType :: InstanceStorageResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateInstanceStorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'disassociateInstanceStorageConfig_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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
  Core.AWSRequest
    DisassociateInstanceStorageConfig
  where
  type
    AWSResponse DisassociateInstanceStorageConfig =
      DisassociateInstanceStorageConfigResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DisassociateInstanceStorageConfigResponse'

instance
  Prelude.Hashable
    DisassociateInstanceStorageConfig
  where
  hashWithSalt
    _salt
    DisassociateInstanceStorageConfig' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` associationId
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    DisassociateInstanceStorageConfig
  where
  rnf DisassociateInstanceStorageConfig' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf resourceType

instance
  Data.ToHeaders
    DisassociateInstanceStorageConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DisassociateInstanceStorageConfig
  where
  toPath DisassociateInstanceStorageConfig' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/storage-config/",
        Data.toBS associationId
      ]

instance
  Data.ToQuery
    DisassociateInstanceStorageConfig
  where
  toQuery DisassociateInstanceStorageConfig' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newDisassociateInstanceStorageConfigResponse' smart constructor.
data DisassociateInstanceStorageConfigResponse = DisassociateInstanceStorageConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
