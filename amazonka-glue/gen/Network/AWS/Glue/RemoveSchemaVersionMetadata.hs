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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveSchemaVersionMetadata' smart constructor.
data RemoveSchemaVersionMetadata = RemoveSchemaVersionMetadata'
  { -- | The unique version ID of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    schemaVersionNumber :: Prelude.Maybe SchemaVersionNumber,
    -- | A wrapper structure that may contain the schema name and Amazon Resource
    -- Name (ARN).
    schemaId :: Prelude.Maybe SchemaId,
    -- | The value of the metadata key.
    metadataKeyValue :: MetadataKeyValuePair
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      schemaVersionNumber = Prelude.Nothing,
      schemaId = Prelude.Nothing,
      metadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
removeSchemaVersionMetadata_schemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadata (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadata_schemaVersionId = Lens.lens (\RemoveSchemaVersionMetadata' {schemaVersionId} -> schemaVersionId) (\s@RemoveSchemaVersionMetadata' {} a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadata)

-- | The version number of the schema.
removeSchemaVersionMetadata_schemaVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadata (Prelude.Maybe SchemaVersionNumber)
removeSchemaVersionMetadata_schemaVersionNumber = Lens.lens (\RemoveSchemaVersionMetadata' {schemaVersionNumber} -> schemaVersionNumber) (\s@RemoveSchemaVersionMetadata' {} a -> s {schemaVersionNumber = a} :: RemoveSchemaVersionMetadata)

-- | A wrapper structure that may contain the schema name and Amazon Resource
-- Name (ARN).
removeSchemaVersionMetadata_schemaId :: Lens.Lens' RemoveSchemaVersionMetadata (Prelude.Maybe SchemaId)
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
            Prelude.<$> (x Core..?> "SchemaArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "SchemaVersionId")
            Prelude.<*> (x Core..?> "MetadataKey")
            Prelude.<*> (x Core..?> "RegistryName")
            Prelude.<*> (x Core..?> "VersionNumber")
            Prelude.<*> (x Core..?> "SchemaName")
            Prelude.<*> (x Core..?> "MetadataValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveSchemaVersionMetadata

instance Prelude.NFData RemoveSchemaVersionMetadata

instance Core.ToHeaders RemoveSchemaVersionMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.RemoveSchemaVersionMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveSchemaVersionMetadata where
  toJSON RemoveSchemaVersionMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SchemaVersionId" Core..=)
              Prelude.<$> schemaVersionId,
            ("SchemaVersionNumber" Core..=)
              Prelude.<$> schemaVersionNumber,
            ("SchemaId" Core..=) Prelude.<$> schemaId,
            Prelude.Just
              ("MetadataKeyValue" Core..= metadataKeyValue)
          ]
      )

instance Core.ToPath RemoveSchemaVersionMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveSchemaVersionMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveSchemaVersionMetadataResponse' smart constructor.
data RemoveSchemaVersionMetadataResponse = RemoveSchemaVersionMetadataResponse'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the schema.
    latestVersion :: Prelude.Maybe Prelude.Bool,
    -- | The version ID for the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The metadata key.
    metadataKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The value of the metadata key.
    metadataValue :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RemoveSchemaVersionMetadataResponse
newRemoveSchemaVersionMetadataResponse pHttpStatus_ =
  RemoveSchemaVersionMetadataResponse'
    { schemaArn =
        Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      metadataKey = Prelude.Nothing,
      registryName = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      metadataValue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schema.
removeSchemaVersionMetadataResponse_schemaArn :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_schemaArn = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaArn} -> schemaArn) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaArn = a} :: RemoveSchemaVersionMetadataResponse)

-- | The latest version of the schema.
removeSchemaVersionMetadataResponse_latestVersion :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Bool)
removeSchemaVersionMetadataResponse_latestVersion = Lens.lens (\RemoveSchemaVersionMetadataResponse' {latestVersion} -> latestVersion) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {latestVersion = a} :: RemoveSchemaVersionMetadataResponse)

-- | The version ID for the schema version.
removeSchemaVersionMetadataResponse_schemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_schemaVersionId = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaVersionId} -> schemaVersionId) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadataResponse)

-- | The metadata key.
removeSchemaVersionMetadataResponse_metadataKey :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_metadataKey = Lens.lens (\RemoveSchemaVersionMetadataResponse' {metadataKey} -> metadataKey) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {metadataKey = a} :: RemoveSchemaVersionMetadataResponse)

-- | The name of the registry.
removeSchemaVersionMetadataResponse_registryName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_registryName = Lens.lens (\RemoveSchemaVersionMetadataResponse' {registryName} -> registryName) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {registryName = a} :: RemoveSchemaVersionMetadataResponse)

-- | The version number of the schema.
removeSchemaVersionMetadataResponse_versionNumber :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Natural)
removeSchemaVersionMetadataResponse_versionNumber = Lens.lens (\RemoveSchemaVersionMetadataResponse' {versionNumber} -> versionNumber) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {versionNumber = a} :: RemoveSchemaVersionMetadataResponse)

-- | The name of the schema.
removeSchemaVersionMetadataResponse_schemaName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_schemaName = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaName} -> schemaName) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaName = a} :: RemoveSchemaVersionMetadataResponse)

-- | The value of the metadata key.
removeSchemaVersionMetadataResponse_metadataValue :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_metadataValue = Lens.lens (\RemoveSchemaVersionMetadataResponse' {metadataValue} -> metadataValue) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {metadataValue = a} :: RemoveSchemaVersionMetadataResponse)

-- | The response's http status code.
removeSchemaVersionMetadataResponse_httpStatus :: Lens.Lens' RemoveSchemaVersionMetadataResponse Prelude.Int
removeSchemaVersionMetadataResponse_httpStatus = Lens.lens (\RemoveSchemaVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {httpStatus = a} :: RemoveSchemaVersionMetadataResponse)

instance
  Prelude.NFData
    RemoveSchemaVersionMetadataResponse
