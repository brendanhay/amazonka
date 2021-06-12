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
-- Module      : Network.AWS.Glue.GetSchemaByDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a schema by the @SchemaDefinition@. The schema definition is
-- sent to the Schema Registry, canonicalized, and hashed. If the hash is
-- matched within the scope of the @SchemaName@ or ARN (or the default
-- registry, if none is supplied), that schema’s metadata is returned.
-- Otherwise, a 404 or NotFound error is returned. Schema versions in
-- @Deleted@ statuses will not be included in the results.
module Network.AWS.Glue.GetSchemaByDefinition
  ( -- * Creating a Request
    GetSchemaByDefinition (..),
    newGetSchemaByDefinition,

    -- * Request Lenses
    getSchemaByDefinition_schemaId,
    getSchemaByDefinition_schemaDefinition,

    -- * Destructuring the Response
    GetSchemaByDefinitionResponse (..),
    newGetSchemaByDefinitionResponse,

    -- * Response Lenses
    getSchemaByDefinitionResponse_schemaArn,
    getSchemaByDefinitionResponse_status,
    getSchemaByDefinitionResponse_schemaVersionId,
    getSchemaByDefinitionResponse_dataFormat,
    getSchemaByDefinitionResponse_createdTime,
    getSchemaByDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSchemaByDefinition' smart constructor.
data GetSchemaByDefinition = GetSchemaByDefinition'
  { -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
    --     @SchemaName@ has to be provided.
    schemaId :: SchemaId,
    -- | The definition of the schema for which schema details are required.
    schemaDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaByDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaId', 'getSchemaByDefinition_schemaId' - This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
--     @SchemaName@ has to be provided.
--
-- 'schemaDefinition', 'getSchemaByDefinition_schemaDefinition' - The definition of the schema for which schema details are required.
newGetSchemaByDefinition ::
  -- | 'schemaId'
  SchemaId ->
  -- | 'schemaDefinition'
  Core.Text ->
  GetSchemaByDefinition
newGetSchemaByDefinition
  pSchemaId_
  pSchemaDefinition_ =
    GetSchemaByDefinition'
      { schemaId = pSchemaId_,
        schemaDefinition = pSchemaDefinition_
      }

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
--     @SchemaName@ has to be provided.
getSchemaByDefinition_schemaId :: Lens.Lens' GetSchemaByDefinition SchemaId
getSchemaByDefinition_schemaId = Lens.lens (\GetSchemaByDefinition' {schemaId} -> schemaId) (\s@GetSchemaByDefinition' {} a -> s {schemaId = a} :: GetSchemaByDefinition)

-- | The definition of the schema for which schema details are required.
getSchemaByDefinition_schemaDefinition :: Lens.Lens' GetSchemaByDefinition Core.Text
getSchemaByDefinition_schemaDefinition = Lens.lens (\GetSchemaByDefinition' {schemaDefinition} -> schemaDefinition) (\s@GetSchemaByDefinition' {} a -> s {schemaDefinition = a} :: GetSchemaByDefinition)

instance Core.AWSRequest GetSchemaByDefinition where
  type
    AWSResponse GetSchemaByDefinition =
      GetSchemaByDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaByDefinitionResponse'
            Core.<$> (x Core..?> "SchemaArn")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "SchemaVersionId")
            Core.<*> (x Core..?> "DataFormat")
            Core.<*> (x Core..?> "CreatedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSchemaByDefinition

instance Core.NFData GetSchemaByDefinition

instance Core.ToHeaders GetSchemaByDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetSchemaByDefinition" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSchemaByDefinition where
  toJSON GetSchemaByDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaId" Core..= schemaId),
            Core.Just
              ("SchemaDefinition" Core..= schemaDefinition)
          ]
      )

instance Core.ToPath GetSchemaByDefinition where
  toPath = Core.const "/"

instance Core.ToQuery GetSchemaByDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSchemaByDefinitionResponse' smart constructor.
data GetSchemaByDefinitionResponse = GetSchemaByDefinitionResponse'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Core.Text,
    -- | The status of the schema version.
    status :: Core.Maybe SchemaVersionStatus,
    -- | The schema ID of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The data format of the schema definition. Currently only @AVRO@ is
    -- supported.
    dataFormat :: Core.Maybe DataFormat,
    -- | The date and time the schema was created.
    createdTime :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaByDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'getSchemaByDefinitionResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'status', 'getSchemaByDefinitionResponse_status' - The status of the schema version.
--
-- 'schemaVersionId', 'getSchemaByDefinitionResponse_schemaVersionId' - The schema ID of the schema version.
--
-- 'dataFormat', 'getSchemaByDefinitionResponse_dataFormat' - The data format of the schema definition. Currently only @AVRO@ is
-- supported.
--
-- 'createdTime', 'getSchemaByDefinitionResponse_createdTime' - The date and time the schema was created.
--
-- 'httpStatus', 'getSchemaByDefinitionResponse_httpStatus' - The response's http status code.
newGetSchemaByDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSchemaByDefinitionResponse
newGetSchemaByDefinitionResponse pHttpStatus_ =
  GetSchemaByDefinitionResponse'
    { schemaArn =
        Core.Nothing,
      status = Core.Nothing,
      schemaVersionId = Core.Nothing,
      dataFormat = Core.Nothing,
      createdTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schema.
getSchemaByDefinitionResponse_schemaArn :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Core.Text)
getSchemaByDefinitionResponse_schemaArn = Lens.lens (\GetSchemaByDefinitionResponse' {schemaArn} -> schemaArn) (\s@GetSchemaByDefinitionResponse' {} a -> s {schemaArn = a} :: GetSchemaByDefinitionResponse)

-- | The status of the schema version.
getSchemaByDefinitionResponse_status :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe SchemaVersionStatus)
getSchemaByDefinitionResponse_status = Lens.lens (\GetSchemaByDefinitionResponse' {status} -> status) (\s@GetSchemaByDefinitionResponse' {} a -> s {status = a} :: GetSchemaByDefinitionResponse)

-- | The schema ID of the schema version.
getSchemaByDefinitionResponse_schemaVersionId :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Core.Text)
getSchemaByDefinitionResponse_schemaVersionId = Lens.lens (\GetSchemaByDefinitionResponse' {schemaVersionId} -> schemaVersionId) (\s@GetSchemaByDefinitionResponse' {} a -> s {schemaVersionId = a} :: GetSchemaByDefinitionResponse)

-- | The data format of the schema definition. Currently only @AVRO@ is
-- supported.
getSchemaByDefinitionResponse_dataFormat :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe DataFormat)
getSchemaByDefinitionResponse_dataFormat = Lens.lens (\GetSchemaByDefinitionResponse' {dataFormat} -> dataFormat) (\s@GetSchemaByDefinitionResponse' {} a -> s {dataFormat = a} :: GetSchemaByDefinitionResponse)

-- | The date and time the schema was created.
getSchemaByDefinitionResponse_createdTime :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Core.Text)
getSchemaByDefinitionResponse_createdTime = Lens.lens (\GetSchemaByDefinitionResponse' {createdTime} -> createdTime) (\s@GetSchemaByDefinitionResponse' {} a -> s {createdTime = a} :: GetSchemaByDefinitionResponse)

-- | The response's http status code.
getSchemaByDefinitionResponse_httpStatus :: Lens.Lens' GetSchemaByDefinitionResponse Core.Int
getSchemaByDefinitionResponse_httpStatus = Lens.lens (\GetSchemaByDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetSchemaByDefinitionResponse' {} a -> s {httpStatus = a} :: GetSchemaByDefinitionResponse)

instance Core.NFData GetSchemaByDefinitionResponse
