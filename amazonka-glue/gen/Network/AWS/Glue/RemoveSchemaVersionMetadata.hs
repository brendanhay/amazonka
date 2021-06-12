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
-- Module      : Network.AWS.Glue.RemoveSchemaVersionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a key value pair from the schema version metadata for the
-- specified schema version ID.
module Network.AWS.Glue.RemoveSchemaVersionMetadata
  ( -- * Creating a Request
    RemoveSchemaVersionMetadata (..),
    newRemoveSchemaVersionMetadata,

    -- * Request Lenses
    removeSchemaVersionMetadata_schemaVersionId,
    removeSchemaVersionMetadata_schemaVersionNumber,
    removeSchemaVersionMetadata_schemaId,
    removeSchemaVersionMetadata_metadataKeyValue,

    -- * Destructuring the Response
    RemoveSchemaVersionMetadataResponse (..),
    newRemoveSchemaVersionMetadataResponse,

    -- * Response Lenses
    removeSchemaVersionMetadataResponse_schemaArn,
    removeSchemaVersionMetadataResponse_latestVersion,
    removeSchemaVersionMetadataResponse_schemaVersionId,
    removeSchemaVersionMetadataResponse_metadataKey,
    removeSchemaVersionMetadataResponse_registryName,
    removeSchemaVersionMetadataResponse_versionNumber,
    removeSchemaVersionMetadataResponse_schemaName,
    removeSchemaVersionMetadataResponse_metadataValue,
    removeSchemaVersionMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveSchemaVersionMetadata' smart constructor.
data RemoveSchemaVersionMetadata = RemoveSchemaVersionMetadata'
  { -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe SchemaVersionNumber,
    -- | A wrapper structure that may contain the schema name and Amazon Resource
    -- Name (ARN).
    schemaId :: Core.Maybe SchemaId,
    -- | The value of the metadata key.
    metadataKeyValue :: MetadataKeyValuePair
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveSchemaVersionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersionId', 'removeSchemaVersionMetadata_schemaVersionId' - The unique version ID of the schema version.
--
-- 'schemaVersionNumber', 'removeSchemaVersionMetadata_schemaVersionNumber' - The version number of the schema.
--
-- 'schemaId', 'removeSchemaVersionMetadata_schemaId' - A wrapper structure that may contain the schema name and Amazon Resource
-- Name (ARN).
--
-- 'metadataKeyValue', 'removeSchemaVersionMetadata_metadataKeyValue' - The value of the metadata key.
newRemoveSchemaVersionMetadata ::
  -- | 'metadataKeyValue'
  MetadataKeyValuePair ->
  RemoveSchemaVersionMetadata
newRemoveSchemaVersionMetadata pMetadataKeyValue_ =
  RemoveSchemaVersionMetadata'
    { schemaVersionId =
        Core.Nothing,
      schemaVersionNumber = Core.Nothing,
      schemaId = Core.Nothing,
      metadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
removeSchemaVersionMetadata_schemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe Core.Text)
removeSchemaVersionMetadata_schemaVersionId = Lens.lens (\RemoveSchemaVersionMetadata' {schemaVersionId} -> schemaVersionId) (\s@RemoveSchemaVersionMetadata' {} a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadata)

-- | The version number of the schema.
removeSchemaVersionMetadata_schemaVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe SchemaVersionNumber)
removeSchemaVersionMetadata_schemaVersionNumber = Lens.lens (\RemoveSchemaVersionMetadata' {schemaVersionNumber} -> schemaVersionNumber) (\s@RemoveSchemaVersionMetadata' {} a -> s {schemaVersionNumber = a} :: RemoveSchemaVersionMetadata)

-- | A wrapper structure that may contain the schema name and Amazon Resource
-- Name (ARN).
removeSchemaVersionMetadata_schemaId :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe SchemaId)
removeSchemaVersionMetadata_schemaId = Lens.lens (\RemoveSchemaVersionMetadata' {schemaId} -> schemaId) (\s@RemoveSchemaVersionMetadata' {} a -> s {schemaId = a} :: RemoveSchemaVersionMetadata)

-- | The value of the metadata key.
removeSchemaVersionMetadata_metadataKeyValue :: Lens.Lens' RemoveSchemaVersionMetadata MetadataKeyValuePair
removeSchemaVersionMetadata_metadataKeyValue = Lens.lens (\RemoveSchemaVersionMetadata' {metadataKeyValue} -> metadataKeyValue) (\s@RemoveSchemaVersionMetadata' {} a -> s {metadataKeyValue = a} :: RemoveSchemaVersionMetadata)

instance Core.AWSRequest RemoveSchemaVersionMetadata where
  type
    AWSResponse RemoveSchemaVersionMetadata =
      RemoveSchemaVersionMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveSchemaVersionMetadataResponse'
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

instance Core.Hashable RemoveSchemaVersionMetadata

instance Core.NFData RemoveSchemaVersionMetadata

instance Core.ToHeaders RemoveSchemaVersionMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.RemoveSchemaVersionMetadata" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RemoveSchemaVersionMetadata where
  toJSON RemoveSchemaVersionMetadata' {..} =
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

instance Core.ToPath RemoveSchemaVersionMetadata where
  toPath = Core.const "/"

instance Core.ToQuery RemoveSchemaVersionMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRemoveSchemaVersionMetadataResponse' smart constructor.
data RemoveSchemaVersionMetadataResponse = RemoveSchemaVersionMetadataResponse'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Core.Text,
    -- | The latest version of the schema.
    latestVersion :: Core.Maybe Core.Bool,
    -- | The version ID for the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The metadata key.
    metadataKey :: Core.Maybe Core.Text,
    -- | The name of the registry.
    registryName :: Core.Maybe Core.Text,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural,
    -- | The name of the schema.
    schemaName :: Core.Maybe Core.Text,
    -- | The value of the metadata key.
    metadataValue :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveSchemaVersionMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'removeSchemaVersionMetadataResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'latestVersion', 'removeSchemaVersionMetadataResponse_latestVersion' - The latest version of the schema.
--
-- 'schemaVersionId', 'removeSchemaVersionMetadataResponse_schemaVersionId' - The version ID for the schema version.
--
-- 'metadataKey', 'removeSchemaVersionMetadataResponse_metadataKey' - The metadata key.
--
-- 'registryName', 'removeSchemaVersionMetadataResponse_registryName' - The name of the registry.
--
-- 'versionNumber', 'removeSchemaVersionMetadataResponse_versionNumber' - The version number of the schema.
--
-- 'schemaName', 'removeSchemaVersionMetadataResponse_schemaName' - The name of the schema.
--
-- 'metadataValue', 'removeSchemaVersionMetadataResponse_metadataValue' - The value of the metadata key.
--
-- 'httpStatus', 'removeSchemaVersionMetadataResponse_httpStatus' - The response's http status code.
newRemoveSchemaVersionMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RemoveSchemaVersionMetadataResponse
newRemoveSchemaVersionMetadataResponse pHttpStatus_ =
  RemoveSchemaVersionMetadataResponse'
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

-- | The Amazon Resource Name (ARN) of the schema.
removeSchemaVersionMetadataResponse_schemaArn :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Text)
removeSchemaVersionMetadataResponse_schemaArn = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaArn} -> schemaArn) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaArn = a} :: RemoveSchemaVersionMetadataResponse)

-- | The latest version of the schema.
removeSchemaVersionMetadataResponse_latestVersion :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Bool)
removeSchemaVersionMetadataResponse_latestVersion = Lens.lens (\RemoveSchemaVersionMetadataResponse' {latestVersion} -> latestVersion) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {latestVersion = a} :: RemoveSchemaVersionMetadataResponse)

-- | The version ID for the schema version.
removeSchemaVersionMetadataResponse_schemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Text)
removeSchemaVersionMetadataResponse_schemaVersionId = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaVersionId} -> schemaVersionId) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadataResponse)

-- | The metadata key.
removeSchemaVersionMetadataResponse_metadataKey :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Text)
removeSchemaVersionMetadataResponse_metadataKey = Lens.lens (\RemoveSchemaVersionMetadataResponse' {metadataKey} -> metadataKey) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {metadataKey = a} :: RemoveSchemaVersionMetadataResponse)

-- | The name of the registry.
removeSchemaVersionMetadataResponse_registryName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Text)
removeSchemaVersionMetadataResponse_registryName = Lens.lens (\RemoveSchemaVersionMetadataResponse' {registryName} -> registryName) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {registryName = a} :: RemoveSchemaVersionMetadataResponse)

-- | The version number of the schema.
removeSchemaVersionMetadataResponse_versionNumber :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Natural)
removeSchemaVersionMetadataResponse_versionNumber = Lens.lens (\RemoveSchemaVersionMetadataResponse' {versionNumber} -> versionNumber) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {versionNumber = a} :: RemoveSchemaVersionMetadataResponse)

-- | The name of the schema.
removeSchemaVersionMetadataResponse_schemaName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Text)
removeSchemaVersionMetadataResponse_schemaName = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaName} -> schemaName) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaName = a} :: RemoveSchemaVersionMetadataResponse)

-- | The value of the metadata key.
removeSchemaVersionMetadataResponse_metadataValue :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Text)
removeSchemaVersionMetadataResponse_metadataValue = Lens.lens (\RemoveSchemaVersionMetadataResponse' {metadataValue} -> metadataValue) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {metadataValue = a} :: RemoveSchemaVersionMetadataResponse)

-- | The response's http status code.
removeSchemaVersionMetadataResponse_httpStatus :: Lens.Lens' RemoveSchemaVersionMetadataResponse Core.Int
removeSchemaVersionMetadataResponse_httpStatus = Lens.lens (\RemoveSchemaVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {httpStatus = a} :: RemoveSchemaVersionMetadataResponse)

instance
  Core.NFData
    RemoveSchemaVersionMetadataResponse
