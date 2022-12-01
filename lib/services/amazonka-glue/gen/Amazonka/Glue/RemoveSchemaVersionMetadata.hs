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
-- Module      : Amazonka.Glue.RemoveSchemaVersionMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a key value pair from the schema version metadata for the
-- specified schema version ID.
module Amazonka.Glue.RemoveSchemaVersionMetadata
  ( -- * Creating a Request
    RemoveSchemaVersionMetadata (..),
    newRemoveSchemaVersionMetadata,

    -- * Request Lenses
    removeSchemaVersionMetadata_schemaVersionNumber,
    removeSchemaVersionMetadata_schemaVersionId,
    removeSchemaVersionMetadata_schemaId,
    removeSchemaVersionMetadata_metadataKeyValue,

    -- * Destructuring the Response
    RemoveSchemaVersionMetadataResponse (..),
    newRemoveSchemaVersionMetadataResponse,

    -- * Response Lenses
    removeSchemaVersionMetadataResponse_registryName,
    removeSchemaVersionMetadataResponse_schemaName,
    removeSchemaVersionMetadataResponse_latestVersion,
    removeSchemaVersionMetadataResponse_metadataValue,
    removeSchemaVersionMetadataResponse_schemaArn,
    removeSchemaVersionMetadataResponse_versionNumber,
    removeSchemaVersionMetadataResponse_metadataKey,
    removeSchemaVersionMetadataResponse_schemaVersionId,
    removeSchemaVersionMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveSchemaVersionMetadata' smart constructor.
data RemoveSchemaVersionMetadata = RemoveSchemaVersionMetadata'
  { -- | The version number of the schema.
    schemaVersionNumber :: Prelude.Maybe SchemaVersionNumber,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
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
-- 'schemaVersionNumber', 'removeSchemaVersionMetadata_schemaVersionNumber' - The version number of the schema.
--
-- 'schemaVersionId', 'removeSchemaVersionMetadata_schemaVersionId' - The unique version ID of the schema version.
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
    { schemaVersionNumber =
        Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      schemaId = Prelude.Nothing,
      metadataKeyValue = pMetadataKeyValue_
    }

-- | The version number of the schema.
removeSchemaVersionMetadata_schemaVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadata (Prelude.Maybe SchemaVersionNumber)
removeSchemaVersionMetadata_schemaVersionNumber = Lens.lens (\RemoveSchemaVersionMetadata' {schemaVersionNumber} -> schemaVersionNumber) (\s@RemoveSchemaVersionMetadata' {} a -> s {schemaVersionNumber = a} :: RemoveSchemaVersionMetadata)

-- | The unique version ID of the schema version.
removeSchemaVersionMetadata_schemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadata (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadata_schemaVersionId = Lens.lens (\RemoveSchemaVersionMetadata' {schemaVersionId} -> schemaVersionId) (\s@RemoveSchemaVersionMetadata' {} a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadata)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveSchemaVersionMetadataResponse'
            Prelude.<$> (x Core..?> "RegistryName")
            Prelude.<*> (x Core..?> "SchemaName")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "MetadataValue")
            Prelude.<*> (x Core..?> "SchemaArn")
            Prelude.<*> (x Core..?> "VersionNumber")
            Prelude.<*> (x Core..?> "MetadataKey")
            Prelude.<*> (x Core..?> "SchemaVersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveSchemaVersionMetadata where
  hashWithSalt _salt RemoveSchemaVersionMetadata' {..} =
    _salt `Prelude.hashWithSalt` schemaVersionNumber
      `Prelude.hashWithSalt` schemaVersionId
      `Prelude.hashWithSalt` schemaId
      `Prelude.hashWithSalt` metadataKeyValue

instance Prelude.NFData RemoveSchemaVersionMetadata where
  rnf RemoveSchemaVersionMetadata' {..} =
    Prelude.rnf schemaVersionNumber
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf schemaId
      `Prelude.seq` Prelude.rnf metadataKeyValue

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
          [ ("SchemaVersionNumber" Core..=)
              Prelude.<$> schemaVersionNumber,
            ("SchemaVersionId" Core..=)
              Prelude.<$> schemaVersionId,
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
  { -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the schema.
    latestVersion :: Prelude.Maybe Prelude.Bool,
    -- | The value of the metadata key.
    metadataValue :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The metadata key.
    metadataKey :: Prelude.Maybe Prelude.Text,
    -- | The version ID for the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
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
-- 'registryName', 'removeSchemaVersionMetadataResponse_registryName' - The name of the registry.
--
-- 'schemaName', 'removeSchemaVersionMetadataResponse_schemaName' - The name of the schema.
--
-- 'latestVersion', 'removeSchemaVersionMetadataResponse_latestVersion' - The latest version of the schema.
--
-- 'metadataValue', 'removeSchemaVersionMetadataResponse_metadataValue' - The value of the metadata key.
--
-- 'schemaArn', 'removeSchemaVersionMetadataResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'versionNumber', 'removeSchemaVersionMetadataResponse_versionNumber' - The version number of the schema.
--
-- 'metadataKey', 'removeSchemaVersionMetadataResponse_metadataKey' - The metadata key.
--
-- 'schemaVersionId', 'removeSchemaVersionMetadataResponse_schemaVersionId' - The version ID for the schema version.
--
-- 'httpStatus', 'removeSchemaVersionMetadataResponse_httpStatus' - The response's http status code.
newRemoveSchemaVersionMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveSchemaVersionMetadataResponse
newRemoveSchemaVersionMetadataResponse pHttpStatus_ =
  RemoveSchemaVersionMetadataResponse'
    { registryName =
        Prelude.Nothing,
      schemaName = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      metadataValue = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      metadataKey = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the registry.
removeSchemaVersionMetadataResponse_registryName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_registryName = Lens.lens (\RemoveSchemaVersionMetadataResponse' {registryName} -> registryName) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {registryName = a} :: RemoveSchemaVersionMetadataResponse)

-- | The name of the schema.
removeSchemaVersionMetadataResponse_schemaName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_schemaName = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaName} -> schemaName) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaName = a} :: RemoveSchemaVersionMetadataResponse)

-- | The latest version of the schema.
removeSchemaVersionMetadataResponse_latestVersion :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Bool)
removeSchemaVersionMetadataResponse_latestVersion = Lens.lens (\RemoveSchemaVersionMetadataResponse' {latestVersion} -> latestVersion) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {latestVersion = a} :: RemoveSchemaVersionMetadataResponse)

-- | The value of the metadata key.
removeSchemaVersionMetadataResponse_metadataValue :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_metadataValue = Lens.lens (\RemoveSchemaVersionMetadataResponse' {metadataValue} -> metadataValue) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {metadataValue = a} :: RemoveSchemaVersionMetadataResponse)

-- | The Amazon Resource Name (ARN) of the schema.
removeSchemaVersionMetadataResponse_schemaArn :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_schemaArn = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaArn} -> schemaArn) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaArn = a} :: RemoveSchemaVersionMetadataResponse)

-- | The version number of the schema.
removeSchemaVersionMetadataResponse_versionNumber :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Natural)
removeSchemaVersionMetadataResponse_versionNumber = Lens.lens (\RemoveSchemaVersionMetadataResponse' {versionNumber} -> versionNumber) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {versionNumber = a} :: RemoveSchemaVersionMetadataResponse)

-- | The metadata key.
removeSchemaVersionMetadataResponse_metadataKey :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_metadataKey = Lens.lens (\RemoveSchemaVersionMetadataResponse' {metadataKey} -> metadataKey) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {metadataKey = a} :: RemoveSchemaVersionMetadataResponse)

-- | The version ID for the schema version.
removeSchemaVersionMetadataResponse_schemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
removeSchemaVersionMetadataResponse_schemaVersionId = Lens.lens (\RemoveSchemaVersionMetadataResponse' {schemaVersionId} -> schemaVersionId) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {schemaVersionId = a} :: RemoveSchemaVersionMetadataResponse)

-- | The response's http status code.
removeSchemaVersionMetadataResponse_httpStatus :: Lens.Lens' RemoveSchemaVersionMetadataResponse Prelude.Int
removeSchemaVersionMetadataResponse_httpStatus = Lens.lens (\RemoveSchemaVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@RemoveSchemaVersionMetadataResponse' {} a -> s {httpStatus = a} :: RemoveSchemaVersionMetadataResponse)

instance
  Prelude.NFData
    RemoveSchemaVersionMetadataResponse
  where
  rnf RemoveSchemaVersionMetadataResponse' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf metadataValue
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf metadataKey
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf httpStatus
