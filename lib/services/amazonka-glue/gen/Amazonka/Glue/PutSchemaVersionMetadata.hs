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
-- Module      : Amazonka.Glue.PutSchemaVersionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the metadata key value pair for a specified schema version ID. A
-- maximum of 10 key value pairs will be allowed per schema version. They
-- can be added over one or more calls.
module Amazonka.Glue.PutSchemaVersionMetadata
  ( -- * Creating a Request
    PutSchemaVersionMetadata (..),
    newPutSchemaVersionMetadata,

    -- * Request Lenses
    putSchemaVersionMetadata_schemaVersionId,
    putSchemaVersionMetadata_schemaId,
    putSchemaVersionMetadata_schemaVersionNumber,
    putSchemaVersionMetadata_metadataKeyValue,

    -- * Destructuring the Response
    PutSchemaVersionMetadataResponse (..),
    newPutSchemaVersionMetadataResponse,

    -- * Response Lenses
    putSchemaVersionMetadataResponse_registryName,
    putSchemaVersionMetadataResponse_schemaName,
    putSchemaVersionMetadataResponse_schemaVersionId,
    putSchemaVersionMetadataResponse_versionNumber,
    putSchemaVersionMetadataResponse_schemaArn,
    putSchemaVersionMetadataResponse_metadataKey,
    putSchemaVersionMetadataResponse_metadataValue,
    putSchemaVersionMetadataResponse_latestVersion,
    putSchemaVersionMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSchemaVersionMetadata' smart constructor.
data PutSchemaVersionMetadata = PutSchemaVersionMetadata'
  { -- | The unique version ID of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the schema.
    schemaId :: Prelude.Maybe SchemaId,
    -- | The version number of the schema.
    schemaVersionNumber :: Prelude.Maybe SchemaVersionNumber,
    -- | The metadata key\'s corresponding value.
    metadataKeyValue :: MetadataKeyValuePair
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'schemaId', 'putSchemaVersionMetadata_schemaId' - The unique ID for the schema.
--
-- 'schemaVersionNumber', 'putSchemaVersionMetadata_schemaVersionNumber' - The version number of the schema.
--
-- 'metadataKeyValue', 'putSchemaVersionMetadata_metadataKeyValue' - The metadata key\'s corresponding value.
newPutSchemaVersionMetadata ::
  -- | 'metadataKeyValue'
  MetadataKeyValuePair ->
  PutSchemaVersionMetadata
newPutSchemaVersionMetadata pMetadataKeyValue_ =
  PutSchemaVersionMetadata'
    { schemaVersionId =
        Prelude.Nothing,
      schemaId = Prelude.Nothing,
      schemaVersionNumber = Prelude.Nothing,
      metadataKeyValue = pMetadataKeyValue_
    }

-- | The unique version ID of the schema version.
putSchemaVersionMetadata_schemaVersionId :: Lens.Lens' PutSchemaVersionMetadata (Prelude.Maybe Prelude.Text)
putSchemaVersionMetadata_schemaVersionId = Lens.lens (\PutSchemaVersionMetadata' {schemaVersionId} -> schemaVersionId) (\s@PutSchemaVersionMetadata' {} a -> s {schemaVersionId = a} :: PutSchemaVersionMetadata)

-- | The unique ID for the schema.
putSchemaVersionMetadata_schemaId :: Lens.Lens' PutSchemaVersionMetadata (Prelude.Maybe SchemaId)
putSchemaVersionMetadata_schemaId = Lens.lens (\PutSchemaVersionMetadata' {schemaId} -> schemaId) (\s@PutSchemaVersionMetadata' {} a -> s {schemaId = a} :: PutSchemaVersionMetadata)

-- | The version number of the schema.
putSchemaVersionMetadata_schemaVersionNumber :: Lens.Lens' PutSchemaVersionMetadata (Prelude.Maybe SchemaVersionNumber)
putSchemaVersionMetadata_schemaVersionNumber = Lens.lens (\PutSchemaVersionMetadata' {schemaVersionNumber} -> schemaVersionNumber) (\s@PutSchemaVersionMetadata' {} a -> s {schemaVersionNumber = a} :: PutSchemaVersionMetadata)

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
            Prelude.<$> (x Core..?> "RegistryName")
            Prelude.<*> (x Core..?> "SchemaName")
            Prelude.<*> (x Core..?> "SchemaVersionId")
            Prelude.<*> (x Core..?> "VersionNumber")
            Prelude.<*> (x Core..?> "SchemaArn")
            Prelude.<*> (x Core..?> "MetadataKey")
            Prelude.<*> (x Core..?> "MetadataValue")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSchemaVersionMetadata where
  hashWithSalt salt' PutSchemaVersionMetadata' {..} =
    salt' `Prelude.hashWithSalt` metadataKeyValue
      `Prelude.hashWithSalt` schemaVersionNumber
      `Prelude.hashWithSalt` schemaId
      `Prelude.hashWithSalt` schemaVersionId

instance Prelude.NFData PutSchemaVersionMetadata where
  rnf PutSchemaVersionMetadata' {..} =
    Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf metadataKeyValue
      `Prelude.seq` Prelude.rnf schemaVersionNumber
      `Prelude.seq` Prelude.rnf schemaId

instance Core.ToHeaders PutSchemaVersionMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.PutSchemaVersionMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutSchemaVersionMetadata where
  toJSON PutSchemaVersionMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SchemaVersionId" Core..=)
              Prelude.<$> schemaVersionId,
            ("SchemaId" Core..=) Prelude.<$> schemaId,
            ("SchemaVersionNumber" Core..=)
              Prelude.<$> schemaVersionNumber,
            Prelude.Just
              ("MetadataKeyValue" Core..= metadataKeyValue)
          ]
      )

instance Core.ToPath PutSchemaVersionMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery PutSchemaVersionMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSchemaVersionMetadataResponse' smart constructor.
data PutSchemaVersionMetadataResponse = PutSchemaVersionMetadataResponse'
  { -- | The name for the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The name for the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) for the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The metadata key.
    metadataKey :: Prelude.Maybe Prelude.Text,
    -- | The value of the metadata key.
    metadataValue :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the schema.
    latestVersion :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSchemaVersionMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'putSchemaVersionMetadataResponse_registryName' - The name for the registry.
--
-- 'schemaName', 'putSchemaVersionMetadataResponse_schemaName' - The name for the schema.
--
-- 'schemaVersionId', 'putSchemaVersionMetadataResponse_schemaVersionId' - The unique version ID of the schema version.
--
-- 'versionNumber', 'putSchemaVersionMetadataResponse_versionNumber' - The version number of the schema.
--
-- 'schemaArn', 'putSchemaVersionMetadataResponse_schemaArn' - The Amazon Resource Name (ARN) for the schema.
--
-- 'metadataKey', 'putSchemaVersionMetadataResponse_metadataKey' - The metadata key.
--
-- 'metadataValue', 'putSchemaVersionMetadataResponse_metadataValue' - The value of the metadata key.
--
-- 'latestVersion', 'putSchemaVersionMetadataResponse_latestVersion' - The latest version of the schema.
--
-- 'httpStatus', 'putSchemaVersionMetadataResponse_httpStatus' - The response's http status code.
newPutSchemaVersionMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSchemaVersionMetadataResponse
newPutSchemaVersionMetadataResponse pHttpStatus_ =
  PutSchemaVersionMetadataResponse'
    { registryName =
        Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      metadataKey = Prelude.Nothing,
      metadataValue = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name for the registry.
putSchemaVersionMetadataResponse_registryName :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
putSchemaVersionMetadataResponse_registryName = Lens.lens (\PutSchemaVersionMetadataResponse' {registryName} -> registryName) (\s@PutSchemaVersionMetadataResponse' {} a -> s {registryName = a} :: PutSchemaVersionMetadataResponse)

-- | The name for the schema.
putSchemaVersionMetadataResponse_schemaName :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
putSchemaVersionMetadataResponse_schemaName = Lens.lens (\PutSchemaVersionMetadataResponse' {schemaName} -> schemaName) (\s@PutSchemaVersionMetadataResponse' {} a -> s {schemaName = a} :: PutSchemaVersionMetadataResponse)

-- | The unique version ID of the schema version.
putSchemaVersionMetadataResponse_schemaVersionId :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
putSchemaVersionMetadataResponse_schemaVersionId = Lens.lens (\PutSchemaVersionMetadataResponse' {schemaVersionId} -> schemaVersionId) (\s@PutSchemaVersionMetadataResponse' {} a -> s {schemaVersionId = a} :: PutSchemaVersionMetadataResponse)

-- | The version number of the schema.
putSchemaVersionMetadataResponse_versionNumber :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Natural)
putSchemaVersionMetadataResponse_versionNumber = Lens.lens (\PutSchemaVersionMetadataResponse' {versionNumber} -> versionNumber) (\s@PutSchemaVersionMetadataResponse' {} a -> s {versionNumber = a} :: PutSchemaVersionMetadataResponse)

-- | The Amazon Resource Name (ARN) for the schema.
putSchemaVersionMetadataResponse_schemaArn :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
putSchemaVersionMetadataResponse_schemaArn = Lens.lens (\PutSchemaVersionMetadataResponse' {schemaArn} -> schemaArn) (\s@PutSchemaVersionMetadataResponse' {} a -> s {schemaArn = a} :: PutSchemaVersionMetadataResponse)

-- | The metadata key.
putSchemaVersionMetadataResponse_metadataKey :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
putSchemaVersionMetadataResponse_metadataKey = Lens.lens (\PutSchemaVersionMetadataResponse' {metadataKey} -> metadataKey) (\s@PutSchemaVersionMetadataResponse' {} a -> s {metadataKey = a} :: PutSchemaVersionMetadataResponse)

-- | The value of the metadata key.
putSchemaVersionMetadataResponse_metadataValue :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
putSchemaVersionMetadataResponse_metadataValue = Lens.lens (\PutSchemaVersionMetadataResponse' {metadataValue} -> metadataValue) (\s@PutSchemaVersionMetadataResponse' {} a -> s {metadataValue = a} :: PutSchemaVersionMetadataResponse)

-- | The latest version of the schema.
putSchemaVersionMetadataResponse_latestVersion :: Lens.Lens' PutSchemaVersionMetadataResponse (Prelude.Maybe Prelude.Bool)
putSchemaVersionMetadataResponse_latestVersion = Lens.lens (\PutSchemaVersionMetadataResponse' {latestVersion} -> latestVersion) (\s@PutSchemaVersionMetadataResponse' {} a -> s {latestVersion = a} :: PutSchemaVersionMetadataResponse)

-- | The response's http status code.
putSchemaVersionMetadataResponse_httpStatus :: Lens.Lens' PutSchemaVersionMetadataResponse Prelude.Int
putSchemaVersionMetadataResponse_httpStatus = Lens.lens (\PutSchemaVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@PutSchemaVersionMetadataResponse' {} a -> s {httpStatus = a} :: PutSchemaVersionMetadataResponse)

instance
  Prelude.NFData
    PutSchemaVersionMetadataResponse
  where
  rnf PutSchemaVersionMetadataResponse' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf latestVersion
      `Prelude.seq` Prelude.rnf metadataValue
      `Prelude.seq` Prelude.rnf metadataKey
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf schemaName
