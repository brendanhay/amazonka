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
-- Module      : Amazonka.Glue.GetSchemaByDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a schema by the @SchemaDefinition@. The schema definition is
-- sent to the Schema Registry, canonicalized, and hashed. If the hash is
-- matched within the scope of the @SchemaName@ or ARN (or the default
-- registry, if none is supplied), that schema’s metadata is returned.
-- Otherwise, a 404 or NotFound error is returned. Schema versions in
-- @Deleted@ statuses will not be included in the results.
module Amazonka.Glue.GetSchemaByDefinition
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
    getSchemaByDefinitionResponse_createdTime,
    getSchemaByDefinitionResponse_dataFormat,
    getSchemaByDefinitionResponse_schemaArn,
    getSchemaByDefinitionResponse_schemaVersionId,
    getSchemaByDefinitionResponse_status,
    getSchemaByDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    schemaDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
getSchemaByDefinition_schemaDefinition :: Lens.Lens' GetSchemaByDefinition Prelude.Text
getSchemaByDefinition_schemaDefinition = Lens.lens (\GetSchemaByDefinition' {schemaDefinition} -> schemaDefinition) (\s@GetSchemaByDefinition' {} a -> s {schemaDefinition = a} :: GetSchemaByDefinition)

instance Core.AWSRequest GetSchemaByDefinition where
  type
    AWSResponse GetSchemaByDefinition =
      GetSchemaByDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaByDefinitionResponse'
            Prelude.<$> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "DataFormat")
            Prelude.<*> (x Data..?> "SchemaArn")
            Prelude.<*> (x Data..?> "SchemaVersionId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSchemaByDefinition where
  hashWithSalt _salt GetSchemaByDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` schemaId
      `Prelude.hashWithSalt` schemaDefinition

instance Prelude.NFData GetSchemaByDefinition where
  rnf GetSchemaByDefinition' {..} =
    Prelude.rnf schemaId `Prelude.seq`
      Prelude.rnf schemaDefinition

instance Data.ToHeaders GetSchemaByDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetSchemaByDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSchemaByDefinition where
  toJSON GetSchemaByDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaId" Data..= schemaId),
            Prelude.Just
              ("SchemaDefinition" Data..= schemaDefinition)
          ]
      )

instance Data.ToPath GetSchemaByDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSchemaByDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaByDefinitionResponse' smart constructor.
data GetSchemaByDefinitionResponse = GetSchemaByDefinitionResponse'
  { -- | The date and time the schema was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
    -- @PROTOBUF@ are supported.
    dataFormat :: Prelude.Maybe DataFormat,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The schema ID of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the schema version.
    status :: Prelude.Maybe SchemaVersionStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaByDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'getSchemaByDefinitionResponse_createdTime' - The date and time the schema was created.
--
-- 'dataFormat', 'getSchemaByDefinitionResponse_dataFormat' - The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
--
-- 'schemaArn', 'getSchemaByDefinitionResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'schemaVersionId', 'getSchemaByDefinitionResponse_schemaVersionId' - The schema ID of the schema version.
--
-- 'status', 'getSchemaByDefinitionResponse_status' - The status of the schema version.
--
-- 'httpStatus', 'getSchemaByDefinitionResponse_httpStatus' - The response's http status code.
newGetSchemaByDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSchemaByDefinitionResponse
newGetSchemaByDefinitionResponse pHttpStatus_ =
  GetSchemaByDefinitionResponse'
    { createdTime =
        Prelude.Nothing,
      dataFormat = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time the schema was created.
getSchemaByDefinitionResponse_createdTime :: Lens.Lens' GetSchemaByDefinitionResponse (Prelude.Maybe Prelude.Text)
getSchemaByDefinitionResponse_createdTime = Lens.lens (\GetSchemaByDefinitionResponse' {createdTime} -> createdTime) (\s@GetSchemaByDefinitionResponse' {} a -> s {createdTime = a} :: GetSchemaByDefinitionResponse)

-- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
getSchemaByDefinitionResponse_dataFormat :: Lens.Lens' GetSchemaByDefinitionResponse (Prelude.Maybe DataFormat)
getSchemaByDefinitionResponse_dataFormat = Lens.lens (\GetSchemaByDefinitionResponse' {dataFormat} -> dataFormat) (\s@GetSchemaByDefinitionResponse' {} a -> s {dataFormat = a} :: GetSchemaByDefinitionResponse)

-- | The Amazon Resource Name (ARN) of the schema.
getSchemaByDefinitionResponse_schemaArn :: Lens.Lens' GetSchemaByDefinitionResponse (Prelude.Maybe Prelude.Text)
getSchemaByDefinitionResponse_schemaArn = Lens.lens (\GetSchemaByDefinitionResponse' {schemaArn} -> schemaArn) (\s@GetSchemaByDefinitionResponse' {} a -> s {schemaArn = a} :: GetSchemaByDefinitionResponse)

-- | The schema ID of the schema version.
getSchemaByDefinitionResponse_schemaVersionId :: Lens.Lens' GetSchemaByDefinitionResponse (Prelude.Maybe Prelude.Text)
getSchemaByDefinitionResponse_schemaVersionId = Lens.lens (\GetSchemaByDefinitionResponse' {schemaVersionId} -> schemaVersionId) (\s@GetSchemaByDefinitionResponse' {} a -> s {schemaVersionId = a} :: GetSchemaByDefinitionResponse)

-- | The status of the schema version.
getSchemaByDefinitionResponse_status :: Lens.Lens' GetSchemaByDefinitionResponse (Prelude.Maybe SchemaVersionStatus)
getSchemaByDefinitionResponse_status = Lens.lens (\GetSchemaByDefinitionResponse' {status} -> status) (\s@GetSchemaByDefinitionResponse' {} a -> s {status = a} :: GetSchemaByDefinitionResponse)

-- | The response's http status code.
getSchemaByDefinitionResponse_httpStatus :: Lens.Lens' GetSchemaByDefinitionResponse Prelude.Int
getSchemaByDefinitionResponse_httpStatus = Lens.lens (\GetSchemaByDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetSchemaByDefinitionResponse' {} a -> s {httpStatus = a} :: GetSchemaByDefinitionResponse)

instance Prelude.NFData GetSchemaByDefinitionResponse where
  rnf GetSchemaByDefinitionResponse' {..} =
    Prelude.rnf createdTime `Prelude.seq`
      Prelude.rnf dataFormat `Prelude.seq`
        Prelude.rnf schemaArn `Prelude.seq`
          Prelude.rnf schemaVersionId `Prelude.seq`
            Prelude.rnf status `Prelude.seq`
              Prelude.rnf httpStatus
