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
-- Module      : Network.AWS.Glue.PutSchemaVersionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the metadata key value pair for a specified schema version ID. A
-- maximum of 10 key value pairs will be allowed per schema version. They
-- can be added over one or more calls.
module Network.AWS.Glue.PutSchemaVersionMetadata
  ( -- * Creating a Request
    PutSchemaVersionMetadata (..),
    newPutSchemaVersionMetadata,

    -- * Request Lenses
    putSchemaVersionMetadata_schemaVersionId,
    putSchemaVersionMetadata_schemaVersionNumber,
    putSchemaVersionMetadata_schemaId,
    putSchemaVersionMetadata_metadataKeyValue,

    -- * Destructuring the Response
    PutSchemaVersionMetadataResponse (..),
    newPutSchemaVersionMetadataResponse,

    -- * Response Lenses
    putSchemaVersionMetadataResponse_schemaArn,
    putSchemaVersionMetadataResponse_latestVersion,
    putSchemaVersionMetadataResponse_schemaVersionId,
    putSchemaVersionMetadataResponse_metadataKey,
    putSchemaVersionMetadataResponse_registryName,
    putSchemaVersionMetadataResponse_versionNumber,
    putSchemaVersionMetadataResponse_schemaName,
    putSchemaVersionMetadataResponse_metadataValue,
    putSchemaVersionMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutSchemaVersionMetadata' smart constructor.
data PutSchemaVersionMetadata = PutSchemaVersionMetadata'
  { -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe SchemaVersionNumber,
    -- | The unique ID for the schema.
    schemaId :: Core.Maybe SchemaId,
    -- | The metadata key\'s corresponding value.
    metadataKeyValue :: MetadataKeyValuePair
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutSchemaVersionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersionId', 'putSchemaVersionMetadata_schemaVersionId' - The unique version ID of the schema version.
--
-- 'schemaVersionNumber', 'putSchemaVersionMetadata_schemaVersionNumber' - The version number of the schema.
--
-- 'schemaId', 'putSchemaVersionMetadata_schemaId' - The unique ID for the schema.
--
-- 'metadataKeyValue', 'putSchemaVersionMetadata_metadataKeyValue' - The metadata key\'s corresponding value.
newPutSchemaVersionMetadata ::
  -- | 'metadataKeyValue'
  MetadataKeyValuePair ->
  PutSchemaVersionMetadata
newPutSchemaVersionMetadata pMetadataKeyValue_ =
  PutSchemaVersionMetadata'
    { schemaVersionId =
        Core.Nothing,
      schemaVersionNumber = Core.Nothing,
      schemaId = Core.Nothing,
      metadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
putSchemaVersionMetadata_schemaVersionId :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe Core.Text)
putSchemaVersionMetadata_schemaVersionId = Lens.lens (\PutSchemaVersionMetadata' {schemaVersionId} -> schemaVersionId) (\s@PutSchemaVersionMetadata' {} a -> s {schemaVersionId = a} :: PutSchemaVersionMetadata)

-- | The version number of the schema.
putSchemaVersionMetadata_schemaVersionNumber :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe SchemaVersionNumber)
putSchemaVersionMetadata_schemaVersionNumber = Lens.lens (\PutSchemaVersionMetadata' {schemaVersionNumber} -> schemaVersionNumber) (\s@PutSchemaVersionMetadata' {} a -> s {schemaVersionNumber = a} :: PutSchemaVersionMetadata)

-- | The unique ID for the schema.
putSchemaVersionMetadata_schemaId :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe SchemaId)
putSchemaVersionMetadata_schemaId = Lens.lens (\PutSchemaVersionMetadata' {schemaId} -> schemaId) (\s@PutSchemaVersionMetadata' {} a -> s {schemaId = a} :: PutSchemaVersionMetadata)

-- | The metadata key\'s corresponding value.
putSchemaVersionMetadata_metadataKeyValue :: Lens.Lens' PutSchemaVersionMetadata MetadataKeyValuePair
putSchemaVersionMetadata_metadataKeyValue = Lens.lens (\PutSchemaVersionMetadata' {metadataKeyValue} -> metadataKeyValue) (\s@PutSchemaVersionMetadata' {} a -> s {metadataKeyValue = a} :: PutSchemaVersionMetadata)

instance Core.AWSRequest PutSchemaVersionMetadata where
  type
    AWSResponse PutSchemaVersionMetadata =
      PutSchemaVersionMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSchemaVersionMetadataResponse'
            Core.<$> (x Core..?> "SchemaArn")
            Core.<*> (x Core..?> "LatestVersion")
            Core.<*> (x Core..?> "SchemaVersionId")
            Core.<*> (x Core..?> "MetadataKey")
            Core.<*> (x Core..?> "RegistryName")
            Core.<*> (x Core..?> "VersionNumber")
            Core.<*> (x Core..?> "SchemaName")
            Core.<*> (x Core..?> "MetadataValue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutSchemaVersionMetadata

instance Core.NFData PutSchemaVersionMetadata

instance Core.ToHeaders PutSchemaVersionMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.PutSchemaVersionMetadata" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutSchemaVersionMetadata where
  toJSON PutSchemaVersionMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaVersionId" Core..=)
              Core.<$> schemaVersionId,
            ("SchemaVersionNumber" Core..=)
              Core.<$> schemaVersionNumber,
            ("SchemaId" Core..=) Core.<$> schemaId,
            Core.Just
              ("MetadataKeyValue" Core..= metadataKeyValue)
          ]
      )

instance Core.ToPath PutSchemaVersionMetadata where
  toPath = Core.const "/"

instance Core.ToQuery PutSchemaVersionMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutSchemaVersionMetadataResponse' smart constructor.
data PutSchemaVersionMetadataResponse = PutSchemaVersionMetadataResponse'
  { -- | The Amazon Resource Name (ARN) for the schema.
    schemaArn :: Core.Maybe Core.Text,
    -- | The latest version of the schema.
    latestVersion :: Core.Maybe Core.Bool,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The metadata key.
    metadataKey :: Core.Maybe Core.Text,
    -- | The name for the registry.
    registryName :: Core.Maybe Core.Text,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural,
    -- | The name for the schema.
    schemaName :: Core.Maybe Core.Text,
    -- | The value of the metadata key.
    metadataValue :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutSchemaVersionMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'putSchemaVersionMetadataResponse_schemaArn' - The Amazon Resource Name (ARN) for the schema.
--
-- 'latestVersion', 'putSchemaVersionMetadataResponse_latestVersion' - The latest version of the schema.
--
-- 'schemaVersionId', 'putSchemaVersionMetadataResponse_schemaVersionId' - The unique version ID of the schema version.
--
-- 'metadataKey', 'putSchemaVersionMetadataResponse_metadataKey' - The metadata key.
--
-- 'registryName', 'putSchemaVersionMetadataResponse_registryName' - The name for the registry.
--
-- 'versionNumber', 'putSchemaVersionMetadataResponse_versionNumber' - The version number of the schema.
--
-- 'schemaName', 'putSchemaVersionMetadataResponse_schemaName' - The name for the schema.
--
-- 'metadataValue', 'putSchemaVersionMetadataResponse_metadataValue' - The value of the metadata key.
--
-- 'httpStatus', 'putSchemaVersionMetadataResponse_httpStatus' - The response's http status code.
newPutSchemaVersionMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutSchemaVersionMetadataResponse
newPutSchemaVersionMetadataResponse pHttpStatus_ =
  PutSchemaVersionMetadataResponse'
    { schemaArn =
        Core.Nothing,
      latestVersion = Core.Nothing,
      schemaVersionId = Core.Nothing,
      metadataKey = Core.Nothing,
      registryName = Core.Nothing,
      versionNumber = Core.Nothing,
      schemaName = Core.Nothing,
      metadataValue = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the schema.
putSchemaVersionMetadataResponse_schemaArn :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Text)
putSchemaVersionMetadataResponse_schemaArn = Lens.lens (\PutSchemaVersionMetadataResponse' {schemaArn} -> schemaArn) (\s@PutSchemaVersionMetadataResponse' {} a -> s {schemaArn = a} :: PutSchemaVersionMetadataResponse)

-- | The latest version of the schema.
putSchemaVersionMetadataResponse_latestVersion :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Bool)
putSchemaVersionMetadataResponse_latestVersion = Lens.lens (\PutSchemaVersionMetadataResponse' {latestVersion} -> latestVersion) (\s@PutSchemaVersionMetadataResponse' {} a -> s {latestVersion = a} :: PutSchemaVersionMetadataResponse)

-- | The unique version ID of the schema version.
putSchemaVersionMetadataResponse_schemaVersionId :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Text)
putSchemaVersionMetadataResponse_schemaVersionId = Lens.lens (\PutSchemaVersionMetadataResponse' {schemaVersionId} -> schemaVersionId) (\s@PutSchemaVersionMetadataResponse' {} a -> s {schemaVersionId = a} :: PutSchemaVersionMetadataResponse)

-- | The metadata key.
putSchemaVersionMetadataResponse_metadataKey :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Text)
putSchemaVersionMetadataResponse_metadataKey = Lens.lens (\PutSchemaVersionMetadataResponse' {metadataKey} -> metadataKey) (\s@PutSchemaVersionMetadataResponse' {} a -> s {metadataKey = a} :: PutSchemaVersionMetadataResponse)

-- | The name for the registry.
putSchemaVersionMetadataResponse_registryName :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Text)
putSchemaVersionMetadataResponse_registryName = Lens.lens (\PutSchemaVersionMetadataResponse' {registryName} -> registryName) (\s@PutSchemaVersionMetadataResponse' {} a -> s {registryName = a} :: PutSchemaVersionMetadataResponse)

-- | The version number of the schema.
putSchemaVersionMetadataResponse_versionNumber :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Natural)
putSchemaVersionMetadataResponse_versionNumber = Lens.lens (\PutSchemaVersionMetadataResponse' {versionNumber} -> versionNumber) (\s@PutSchemaVersionMetadataResponse' {} a -> s {versionNumber = a} :: PutSchemaVersionMetadataResponse)

-- | The name for the schema.
putSchemaVersionMetadataResponse_schemaName :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Text)
putSchemaVersionMetadataResponse_schemaName = Lens.lens (\PutSchemaVersionMetadataResponse' {schemaName} -> schemaName) (\s@PutSchemaVersionMetadataResponse' {} a -> s {schemaName = a} :: PutSchemaVersionMetadataResponse)

-- | The value of the metadata key.
putSchemaVersionMetadataResponse_metadataValue :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Text)
putSchemaVersionMetadataResponse_metadataValue = Lens.lens (\PutSchemaVersionMetadataResponse' {metadataValue} -> metadataValue) (\s@PutSchemaVersionMetadataResponse' {} a -> s {metadataValue = a} :: PutSchemaVersionMetadataResponse)

-- | The response's http status code.
putSchemaVersionMetadataResponse_httpStatus :: Lens.Lens' PutSchemaVersionMetadataResponse Core.Int
putSchemaVersionMetadataResponse_httpStatus = Lens.lens (\PutSchemaVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@PutSchemaVersionMetadataResponse' {} a -> s {httpStatus = a} :: PutSchemaVersionMetadataResponse)

instance Core.NFData PutSchemaVersionMetadataResponse
