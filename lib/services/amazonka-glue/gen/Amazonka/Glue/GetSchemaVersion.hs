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
-- Module      : Amazonka.Glue.GetSchemaVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the specified schema by its unique ID assigned when a version of the
-- schema is created or registered. Schema versions in Deleted status will
-- not be included in the results.
module Amazonka.Glue.GetSchemaVersion
  ( -- * Creating a Request
    GetSchemaVersion (..),
    newGetSchemaVersion,

    -- * Request Lenses
    getSchemaVersion_schemaVersionNumber,
    getSchemaVersion_schemaVersionId,
    getSchemaVersion_schemaId,

    -- * Destructuring the Response
    GetSchemaVersionResponse (..),
    newGetSchemaVersionResponse,

    -- * Response Lenses
    getSchemaVersionResponse_createdTime,
    getSchemaVersionResponse_dataFormat,
    getSchemaVersionResponse_status,
    getSchemaVersionResponse_schemaArn,
    getSchemaVersionResponse_versionNumber,
    getSchemaVersionResponse_schemaVersionId,
    getSchemaVersionResponse_schemaDefinition,
    getSchemaVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSchemaVersion' smart constructor.
data GetSchemaVersion = GetSchemaVersion'
  { -- | The version number of the schema.
    schemaVersionNumber :: Prelude.Maybe SchemaVersionNumber,
    -- | The @SchemaVersionId@ of the schema version. This field is required for
    -- fetching by schema ID. Either this or the @SchemaId@ wrapper has to be
    -- provided.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
    --     provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
    --     @SchemaName@ and @RegistryName@ has to be provided.
    schemaId :: Prelude.Maybe SchemaId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersionNumber', 'getSchemaVersion_schemaVersionNumber' - The version number of the schema.
--
-- 'schemaVersionId', 'getSchemaVersion_schemaVersionId' - The @SchemaVersionId@ of the schema version. This field is required for
-- fetching by schema ID. Either this or the @SchemaId@ wrapper has to be
-- provided.
--
-- 'schemaId', 'getSchemaVersion_schemaId' - This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
newGetSchemaVersion ::
  GetSchemaVersion
newGetSchemaVersion =
  GetSchemaVersion'
    { schemaVersionNumber =
        Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      schemaId = Prelude.Nothing
    }

-- | The version number of the schema.
getSchemaVersion_schemaVersionNumber :: Lens.Lens' GetSchemaVersion (Prelude.Maybe SchemaVersionNumber)
getSchemaVersion_schemaVersionNumber = Lens.lens (\GetSchemaVersion' {schemaVersionNumber} -> schemaVersionNumber) (\s@GetSchemaVersion' {} a -> s {schemaVersionNumber = a} :: GetSchemaVersion)

-- | The @SchemaVersionId@ of the schema version. This field is required for
-- fetching by schema ID. Either this or the @SchemaId@ wrapper has to be
-- provided.
getSchemaVersion_schemaVersionId :: Lens.Lens' GetSchemaVersion (Prelude.Maybe Prelude.Text)
getSchemaVersion_schemaVersionId = Lens.lens (\GetSchemaVersion' {schemaVersionId} -> schemaVersionId) (\s@GetSchemaVersion' {} a -> s {schemaVersionId = a} :: GetSchemaVersion)

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
getSchemaVersion_schemaId :: Lens.Lens' GetSchemaVersion (Prelude.Maybe SchemaId)
getSchemaVersion_schemaId = Lens.lens (\GetSchemaVersion' {schemaId} -> schemaId) (\s@GetSchemaVersion' {} a -> s {schemaId = a} :: GetSchemaVersion)

instance Core.AWSRequest GetSchemaVersion where
  type
    AWSResponse GetSchemaVersion =
      GetSchemaVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaVersionResponse'
            Prelude.<$> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "DataFormat")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "SchemaArn")
            Prelude.<*> (x Data..?> "VersionNumber")
            Prelude.<*> (x Data..?> "SchemaVersionId")
            Prelude.<*> (x Data..?> "SchemaDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSchemaVersion where
  hashWithSalt _salt GetSchemaVersion' {..} =
    _salt `Prelude.hashWithSalt` schemaVersionNumber
      `Prelude.hashWithSalt` schemaVersionId
      `Prelude.hashWithSalt` schemaId

instance Prelude.NFData GetSchemaVersion where
  rnf GetSchemaVersion' {..} =
    Prelude.rnf schemaVersionNumber
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf schemaId

instance Data.ToHeaders GetSchemaVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetSchemaVersion" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSchemaVersion where
  toJSON GetSchemaVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SchemaVersionNumber" Data..=)
              Prelude.<$> schemaVersionNumber,
            ("SchemaVersionId" Data..=)
              Prelude.<$> schemaVersionId,
            ("SchemaId" Data..=) Prelude.<$> schemaId
          ]
      )

instance Data.ToPath GetSchemaVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSchemaVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaVersionResponse' smart constructor.
data GetSchemaVersionResponse = GetSchemaVersionResponse'
  { -- | The date and time the schema version was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
    -- @PROTOBUF@ are supported.
    dataFormat :: Prelude.Maybe DataFormat,
    -- | The status of the schema version.
    status :: Prelude.Maybe SchemaVersionStatus,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The @SchemaVersionId@ of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The schema definition for the schema ID.
    schemaDefinition :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'getSchemaVersionResponse_createdTime' - The date and time the schema version was created.
--
-- 'dataFormat', 'getSchemaVersionResponse_dataFormat' - The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
--
-- 'status', 'getSchemaVersionResponse_status' - The status of the schema version.
--
-- 'schemaArn', 'getSchemaVersionResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'versionNumber', 'getSchemaVersionResponse_versionNumber' - The version number of the schema.
--
-- 'schemaVersionId', 'getSchemaVersionResponse_schemaVersionId' - The @SchemaVersionId@ of the schema version.
--
-- 'schemaDefinition', 'getSchemaVersionResponse_schemaDefinition' - The schema definition for the schema ID.
--
-- 'httpStatus', 'getSchemaVersionResponse_httpStatus' - The response's http status code.
newGetSchemaVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSchemaVersionResponse
newGetSchemaVersionResponse pHttpStatus_ =
  GetSchemaVersionResponse'
    { createdTime =
        Prelude.Nothing,
      dataFormat = Prelude.Nothing,
      status = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      schemaDefinition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time the schema version was created.
getSchemaVersionResponse_createdTime :: Lens.Lens' GetSchemaVersionResponse (Prelude.Maybe Prelude.Text)
getSchemaVersionResponse_createdTime = Lens.lens (\GetSchemaVersionResponse' {createdTime} -> createdTime) (\s@GetSchemaVersionResponse' {} a -> s {createdTime = a} :: GetSchemaVersionResponse)

-- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
getSchemaVersionResponse_dataFormat :: Lens.Lens' GetSchemaVersionResponse (Prelude.Maybe DataFormat)
getSchemaVersionResponse_dataFormat = Lens.lens (\GetSchemaVersionResponse' {dataFormat} -> dataFormat) (\s@GetSchemaVersionResponse' {} a -> s {dataFormat = a} :: GetSchemaVersionResponse)

-- | The status of the schema version.
getSchemaVersionResponse_status :: Lens.Lens' GetSchemaVersionResponse (Prelude.Maybe SchemaVersionStatus)
getSchemaVersionResponse_status = Lens.lens (\GetSchemaVersionResponse' {status} -> status) (\s@GetSchemaVersionResponse' {} a -> s {status = a} :: GetSchemaVersionResponse)

-- | The Amazon Resource Name (ARN) of the schema.
getSchemaVersionResponse_schemaArn :: Lens.Lens' GetSchemaVersionResponse (Prelude.Maybe Prelude.Text)
getSchemaVersionResponse_schemaArn = Lens.lens (\GetSchemaVersionResponse' {schemaArn} -> schemaArn) (\s@GetSchemaVersionResponse' {} a -> s {schemaArn = a} :: GetSchemaVersionResponse)

-- | The version number of the schema.
getSchemaVersionResponse_versionNumber :: Lens.Lens' GetSchemaVersionResponse (Prelude.Maybe Prelude.Natural)
getSchemaVersionResponse_versionNumber = Lens.lens (\GetSchemaVersionResponse' {versionNumber} -> versionNumber) (\s@GetSchemaVersionResponse' {} a -> s {versionNumber = a} :: GetSchemaVersionResponse)

-- | The @SchemaVersionId@ of the schema version.
getSchemaVersionResponse_schemaVersionId :: Lens.Lens' GetSchemaVersionResponse (Prelude.Maybe Prelude.Text)
getSchemaVersionResponse_schemaVersionId = Lens.lens (\GetSchemaVersionResponse' {schemaVersionId} -> schemaVersionId) (\s@GetSchemaVersionResponse' {} a -> s {schemaVersionId = a} :: GetSchemaVersionResponse)

-- | The schema definition for the schema ID.
getSchemaVersionResponse_schemaDefinition :: Lens.Lens' GetSchemaVersionResponse (Prelude.Maybe Prelude.Text)
getSchemaVersionResponse_schemaDefinition = Lens.lens (\GetSchemaVersionResponse' {schemaDefinition} -> schemaDefinition) (\s@GetSchemaVersionResponse' {} a -> s {schemaDefinition = a} :: GetSchemaVersionResponse)

-- | The response's http status code.
getSchemaVersionResponse_httpStatus :: Lens.Lens' GetSchemaVersionResponse Prelude.Int
getSchemaVersionResponse_httpStatus = Lens.lens (\GetSchemaVersionResponse' {httpStatus} -> httpStatus) (\s@GetSchemaVersionResponse' {} a -> s {httpStatus = a} :: GetSchemaVersionResponse)

instance Prelude.NFData GetSchemaVersionResponse where
  rnf GetSchemaVersionResponse' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dataFormat
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf schemaDefinition
      `Prelude.seq` Prelude.rnf httpStatus
