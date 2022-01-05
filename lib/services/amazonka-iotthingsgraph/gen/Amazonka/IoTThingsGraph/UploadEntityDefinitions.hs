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
-- Module      : Amazonka.IoTThingsGraph.UploadEntityDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Asynchronously uploads one or more entity definitions to the user\'s
-- namespace. The @document@ parameter is required if
-- @syncWithPublicNamespace@ and @deleteExistingEntites@ are false. If the
-- @syncWithPublicNamespace@ parameter is set to @true@, the user\'s
-- namespace will synchronize with the latest version of the public
-- namespace. If @deprecateExistingEntities@ is set to true, all entities
-- in the latest version will be deleted before the new
-- @DefinitionDocument@ is uploaded.
--
-- When a user uploads entity definitions for the first time, the service
-- creates a new namespace for the user. The new namespace tracks the
-- public namespace. Currently users can have only one namespace. The
-- namespace version increments whenever a user uploads entity definitions
-- that are backwards-incompatible and whenever a user sets the
-- @syncWithPublicNamespace@ parameter or the @deprecateExistingEntities@
-- parameter to @true@.
--
-- The IDs for all of the entities should be in URN format. Each entity
-- must be in the user\'s namespace. Users can\'t create entities in the
-- public namespace, but entity definitions can refer to entities in the
-- public namespace.
--
-- Valid entities are @Device@, @DeviceModel@, @Service@, @Capability@,
-- @State@, @Action@, @Event@, @Property@, @Mapping@, @Enum@.
module Amazonka.IoTThingsGraph.UploadEntityDefinitions
  ( -- * Creating a Request
    UploadEntityDefinitions (..),
    newUploadEntityDefinitions,

    -- * Request Lenses
    uploadEntityDefinitions_syncWithPublicNamespace,
    uploadEntityDefinitions_deprecateExistingEntities,
    uploadEntityDefinitions_document,

    -- * Destructuring the Response
    UploadEntityDefinitionsResponse (..),
    newUploadEntityDefinitionsResponse,

    -- * Response Lenses
    uploadEntityDefinitionsResponse_httpStatus,
    uploadEntityDefinitionsResponse_uploadId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUploadEntityDefinitions' smart constructor.
data UploadEntityDefinitions = UploadEntityDefinitions'
  { -- | A Boolean that specifies whether to synchronize with the latest version
    -- of the public namespace. If set to @true@, the upload will create a new
    -- namespace version.
    syncWithPublicNamespace :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean that specifies whether to deprecate all entities in the latest
    -- version before uploading the new @DefinitionDocument@. If set to @true@,
    -- the upload will create a new namespace version.
    deprecateExistingEntities :: Prelude.Maybe Prelude.Bool,
    -- | The @DefinitionDocument@ that defines the updated entities.
    document :: Prelude.Maybe DefinitionDocument
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadEntityDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncWithPublicNamespace', 'uploadEntityDefinitions_syncWithPublicNamespace' - A Boolean that specifies whether to synchronize with the latest version
-- of the public namespace. If set to @true@, the upload will create a new
-- namespace version.
--
-- 'deprecateExistingEntities', 'uploadEntityDefinitions_deprecateExistingEntities' - A Boolean that specifies whether to deprecate all entities in the latest
-- version before uploading the new @DefinitionDocument@. If set to @true@,
-- the upload will create a new namespace version.
--
-- 'document', 'uploadEntityDefinitions_document' - The @DefinitionDocument@ that defines the updated entities.
newUploadEntityDefinitions ::
  UploadEntityDefinitions
newUploadEntityDefinitions =
  UploadEntityDefinitions'
    { syncWithPublicNamespace =
        Prelude.Nothing,
      deprecateExistingEntities = Prelude.Nothing,
      document = Prelude.Nothing
    }

-- | A Boolean that specifies whether to synchronize with the latest version
-- of the public namespace. If set to @true@, the upload will create a new
-- namespace version.
uploadEntityDefinitions_syncWithPublicNamespace :: Lens.Lens' UploadEntityDefinitions (Prelude.Maybe Prelude.Bool)
uploadEntityDefinitions_syncWithPublicNamespace = Lens.lens (\UploadEntityDefinitions' {syncWithPublicNamespace} -> syncWithPublicNamespace) (\s@UploadEntityDefinitions' {} a -> s {syncWithPublicNamespace = a} :: UploadEntityDefinitions)

-- | A Boolean that specifies whether to deprecate all entities in the latest
-- version before uploading the new @DefinitionDocument@. If set to @true@,
-- the upload will create a new namespace version.
uploadEntityDefinitions_deprecateExistingEntities :: Lens.Lens' UploadEntityDefinitions (Prelude.Maybe Prelude.Bool)
uploadEntityDefinitions_deprecateExistingEntities = Lens.lens (\UploadEntityDefinitions' {deprecateExistingEntities} -> deprecateExistingEntities) (\s@UploadEntityDefinitions' {} a -> s {deprecateExistingEntities = a} :: UploadEntityDefinitions)

-- | The @DefinitionDocument@ that defines the updated entities.
uploadEntityDefinitions_document :: Lens.Lens' UploadEntityDefinitions (Prelude.Maybe DefinitionDocument)
uploadEntityDefinitions_document = Lens.lens (\UploadEntityDefinitions' {document} -> document) (\s@UploadEntityDefinitions' {} a -> s {document = a} :: UploadEntityDefinitions)

instance Core.AWSRequest UploadEntityDefinitions where
  type
    AWSResponse UploadEntityDefinitions =
      UploadEntityDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UploadEntityDefinitionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "uploadId")
      )

instance Prelude.Hashable UploadEntityDefinitions where
  hashWithSalt _salt UploadEntityDefinitions' {..} =
    _salt
      `Prelude.hashWithSalt` syncWithPublicNamespace
      `Prelude.hashWithSalt` deprecateExistingEntities
      `Prelude.hashWithSalt` document

instance Prelude.NFData UploadEntityDefinitions where
  rnf UploadEntityDefinitions' {..} =
    Prelude.rnf syncWithPublicNamespace
      `Prelude.seq` Prelude.rnf deprecateExistingEntities
      `Prelude.seq` Prelude.rnf document

instance Core.ToHeaders UploadEntityDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.UploadEntityDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UploadEntityDefinitions where
  toJSON UploadEntityDefinitions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("syncWithPublicNamespace" Core..=)
              Prelude.<$> syncWithPublicNamespace,
            ("deprecateExistingEntities" Core..=)
              Prelude.<$> deprecateExistingEntities,
            ("document" Core..=) Prelude.<$> document
          ]
      )

instance Core.ToPath UploadEntityDefinitions where
  toPath = Prelude.const "/"

instance Core.ToQuery UploadEntityDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUploadEntityDefinitionsResponse' smart constructor.
data UploadEntityDefinitionsResponse = UploadEntityDefinitionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID that specifies the upload action. You can use this to track the
    -- status of the upload.
    uploadId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadEntityDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'uploadEntityDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'uploadId', 'uploadEntityDefinitionsResponse_uploadId' - The ID that specifies the upload action. You can use this to track the
-- status of the upload.
newUploadEntityDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'uploadId'
  Prelude.Text ->
  UploadEntityDefinitionsResponse
newUploadEntityDefinitionsResponse
  pHttpStatus_
  pUploadId_ =
    UploadEntityDefinitionsResponse'
      { httpStatus =
          pHttpStatus_,
        uploadId = pUploadId_
      }

-- | The response's http status code.
uploadEntityDefinitionsResponse_httpStatus :: Lens.Lens' UploadEntityDefinitionsResponse Prelude.Int
uploadEntityDefinitionsResponse_httpStatus = Lens.lens (\UploadEntityDefinitionsResponse' {httpStatus} -> httpStatus) (\s@UploadEntityDefinitionsResponse' {} a -> s {httpStatus = a} :: UploadEntityDefinitionsResponse)

-- | The ID that specifies the upload action. You can use this to track the
-- status of the upload.
uploadEntityDefinitionsResponse_uploadId :: Lens.Lens' UploadEntityDefinitionsResponse Prelude.Text
uploadEntityDefinitionsResponse_uploadId = Lens.lens (\UploadEntityDefinitionsResponse' {uploadId} -> uploadId) (\s@UploadEntityDefinitionsResponse' {} a -> s {uploadId = a} :: UploadEntityDefinitionsResponse)

instance
  Prelude.NFData
    UploadEntityDefinitionsResponse
  where
  rnf UploadEntityDefinitionsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf uploadId
