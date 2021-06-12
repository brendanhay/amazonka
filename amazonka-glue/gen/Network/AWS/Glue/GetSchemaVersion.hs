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
-- Module      : Network.AWS.Glue.GetSchemaVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the specified schema by its unique ID assigned when a version of the
-- schema is created or registered. Schema versions in Deleted status will
-- not be included in the results.
module Network.AWS.Glue.GetSchemaVersion
  ( -- * Creating a Request
    GetSchemaVersion (..),
    newGetSchemaVersion,

    -- * Request Lenses
    getSchemaVersion_schemaVersionId,
    getSchemaVersion_schemaVersionNumber,
    getSchemaVersion_schemaId,

    -- * Destructuring the Response
    GetSchemaVersionResponse (..),
    newGetSchemaVersionResponse,

    -- * Response Lenses
    getSchemaVersionResponse_schemaArn,
    getSchemaVersionResponse_status,
    getSchemaVersionResponse_schemaDefinition,
    getSchemaVersionResponse_schemaVersionId,
    getSchemaVersionResponse_dataFormat,
    getSchemaVersionResponse_createdTime,
    getSchemaVersionResponse_versionNumber,
    getSchemaVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSchemaVersion' smart constructor.
data GetSchemaVersion = GetSchemaVersion'
  { -- | The @SchemaVersionId@ of the schema version. This field is required for
    -- fetching by schema ID. Either this or the @SchemaId@ wrapper has to be
    -- provided.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe SchemaVersionNumber,
    -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
    --     provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
    --     @SchemaName@ and @RegistryName@ has to be provided.
    schemaId :: Core.Maybe SchemaId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersionId', 'getSchemaVersion_schemaVersionId' - The @SchemaVersionId@ of the schema version. This field is required for
-- fetching by schema ID. Either this or the @SchemaId@ wrapper has to be
-- provided.
--
-- 'schemaVersionNumber', 'getSchemaVersion_schemaVersionNumber' - The version number of the schema.
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
    { schemaVersionId = Core.Nothing,
      schemaVersionNumber = Core.Nothing,
      schemaId = Core.Nothing
    }

-- | The @SchemaVersionId@ of the schema version. This field is required for
-- fetching by schema ID. Either this or the @SchemaId@ wrapper has to be
-- provided.
getSchemaVersion_schemaVersionId :: Lens.Lens' GetSchemaVersion (Core.Maybe Core.Text)
getSchemaVersion_schemaVersionId = Lens.lens (\GetSchemaVersion' {schemaVersionId} -> schemaVersionId) (\s@GetSchemaVersion' {} a -> s {schemaVersionId = a} :: GetSchemaVersion)

-- | The version number of the schema.
getSchemaVersion_schemaVersionNumber :: Lens.Lens' GetSchemaVersion (Core.Maybe SchemaVersionNumber)
getSchemaVersion_schemaVersionNumber = Lens.lens (\GetSchemaVersion' {schemaVersionNumber} -> schemaVersionNumber) (\s@GetSchemaVersion' {} a -> s {schemaVersionNumber = a} :: GetSchemaVersion)

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
getSchemaVersion_schemaId :: Lens.Lens' GetSchemaVersion (Core.Maybe SchemaId)
getSchemaVersion_schemaId = Lens.lens (\GetSchemaVersion' {schemaId} -> schemaId) (\s@GetSchemaVersion' {} a -> s {schemaId = a} :: GetSchemaVersion)

instance Core.AWSRequest GetSchemaVersion where
  type
    AWSResponse GetSchemaVersion =
      GetSchemaVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaVersionResponse'
            Core.<$> (x Core..?> "SchemaArn")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "SchemaDefinition")
            Core.<*> (x Core..?> "SchemaVersionId")
            Core.<*> (x Core..?> "DataFormat")
            Core.<*> (x Core..?> "CreatedTime")
            Core.<*> (x Core..?> "VersionNumber")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSchemaVersion

instance Core.NFData GetSchemaVersion

instance Core.ToHeaders GetSchemaVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetSchemaVersion" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSchemaVersion where
  toJSON GetSchemaVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaVersionId" Core..=)
              Core.<$> schemaVersionId,
            ("SchemaVersionNumber" Core..=)
              Core.<$> schemaVersionNumber,
            ("SchemaId" Core..=) Core.<$> schemaId
          ]
      )

instance Core.ToPath GetSchemaVersion where
  toPath = Core.const "/"

instance Core.ToQuery GetSchemaVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSchemaVersionResponse' smart constructor.
data GetSchemaVersionResponse = GetSchemaVersionResponse'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Core.Text,
    -- | The status of the schema version.
    status :: Core.Maybe SchemaVersionStatus,
    -- | The schema definition for the schema ID.
    schemaDefinition :: Core.Maybe Core.Text,
    -- | The @SchemaVersionId@ of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The data format of the schema definition. Currently only @AVRO@ is
    -- supported.
    dataFormat :: Core.Maybe DataFormat,
    -- | The date and time the schema version was created.
    createdTime :: Core.Maybe Core.Text,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'getSchemaVersionResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'status', 'getSchemaVersionResponse_status' - The status of the schema version.
--
-- 'schemaDefinition', 'getSchemaVersionResponse_schemaDefinition' - The schema definition for the schema ID.
--
-- 'schemaVersionId', 'getSchemaVersionResponse_schemaVersionId' - The @SchemaVersionId@ of the schema version.
--
-- 'dataFormat', 'getSchemaVersionResponse_dataFormat' - The data format of the schema definition. Currently only @AVRO@ is
-- supported.
--
-- 'createdTime', 'getSchemaVersionResponse_createdTime' - The date and time the schema version was created.
--
-- 'versionNumber', 'getSchemaVersionResponse_versionNumber' - The version number of the schema.
--
-- 'httpStatus', 'getSchemaVersionResponse_httpStatus' - The response's http status code.
newGetSchemaVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSchemaVersionResponse
newGetSchemaVersionResponse pHttpStatus_ =
  GetSchemaVersionResponse'
    { schemaArn = Core.Nothing,
      status = Core.Nothing,
      schemaDefinition = Core.Nothing,
      schemaVersionId = Core.Nothing,
      dataFormat = Core.Nothing,
      createdTime = Core.Nothing,
      versionNumber = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schema.
getSchemaVersionResponse_schemaArn :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Core.Text)
getSchemaVersionResponse_schemaArn = Lens.lens (\GetSchemaVersionResponse' {schemaArn} -> schemaArn) (\s@GetSchemaVersionResponse' {} a -> s {schemaArn = a} :: GetSchemaVersionResponse)

-- | The status of the schema version.
getSchemaVersionResponse_status :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe SchemaVersionStatus)
getSchemaVersionResponse_status = Lens.lens (\GetSchemaVersionResponse' {status} -> status) (\s@GetSchemaVersionResponse' {} a -> s {status = a} :: GetSchemaVersionResponse)

-- | The schema definition for the schema ID.
getSchemaVersionResponse_schemaDefinition :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Core.Text)
getSchemaVersionResponse_schemaDefinition = Lens.lens (\GetSchemaVersionResponse' {schemaDefinition} -> schemaDefinition) (\s@GetSchemaVersionResponse' {} a -> s {schemaDefinition = a} :: GetSchemaVersionResponse)

-- | The @SchemaVersionId@ of the schema version.
getSchemaVersionResponse_schemaVersionId :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Core.Text)
getSchemaVersionResponse_schemaVersionId = Lens.lens (\GetSchemaVersionResponse' {schemaVersionId} -> schemaVersionId) (\s@GetSchemaVersionResponse' {} a -> s {schemaVersionId = a} :: GetSchemaVersionResponse)

-- | The data format of the schema definition. Currently only @AVRO@ is
-- supported.
getSchemaVersionResponse_dataFormat :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe DataFormat)
getSchemaVersionResponse_dataFormat = Lens.lens (\GetSchemaVersionResponse' {dataFormat} -> dataFormat) (\s@GetSchemaVersionResponse' {} a -> s {dataFormat = a} :: GetSchemaVersionResponse)

-- | The date and time the schema version was created.
getSchemaVersionResponse_createdTime :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Core.Text)
getSchemaVersionResponse_createdTime = Lens.lens (\GetSchemaVersionResponse' {createdTime} -> createdTime) (\s@GetSchemaVersionResponse' {} a -> s {createdTime = a} :: GetSchemaVersionResponse)

-- | The version number of the schema.
getSchemaVersionResponse_versionNumber :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Core.Natural)
getSchemaVersionResponse_versionNumber = Lens.lens (\GetSchemaVersionResponse' {versionNumber} -> versionNumber) (\s@GetSchemaVersionResponse' {} a -> s {versionNumber = a} :: GetSchemaVersionResponse)

-- | The response's http status code.
getSchemaVersionResponse_httpStatus :: Lens.Lens' GetSchemaVersionResponse Core.Int
getSchemaVersionResponse_httpStatus = Lens.lens (\GetSchemaVersionResponse' {httpStatus} -> httpStatus) (\s@GetSchemaVersionResponse' {} a -> s {httpStatus = a} :: GetSchemaVersionResponse)

instance Core.NFData GetSchemaVersionResponse
