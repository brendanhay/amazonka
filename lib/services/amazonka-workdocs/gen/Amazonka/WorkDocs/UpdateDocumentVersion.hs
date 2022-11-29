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
-- Module      : Amazonka.WorkDocs.UpdateDocumentVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the document version to ACTIVE.
--
-- Amazon WorkDocs also sets its document container to ACTIVE. This is the
-- last step in a document upload, after the client uploads the document to
-- an S3-presigned URL returned by InitiateDocumentVersionUpload.
module Amazonka.WorkDocs.UpdateDocumentVersion
  ( -- * Creating a Request
    UpdateDocumentVersion (..),
    newUpdateDocumentVersion,

    -- * Request Lenses
    updateDocumentVersion_versionStatus,
    updateDocumentVersion_authenticationToken,
    updateDocumentVersion_documentId,
    updateDocumentVersion_versionId,

    -- * Destructuring the Response
    UpdateDocumentVersionResponse (..),
    newUpdateDocumentVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newUpdateDocumentVersion' smart constructor.
data UpdateDocumentVersion = UpdateDocumentVersion'
  { -- | The status of the version.
    versionStatus :: Prelude.Maybe DocumentVersionStatus,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the document.
    documentId :: Prelude.Text,
    -- | The version ID of the document.
    versionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionStatus', 'updateDocumentVersion_versionStatus' - The status of the version.
--
-- 'authenticationToken', 'updateDocumentVersion_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'documentId', 'updateDocumentVersion_documentId' - The ID of the document.
--
-- 'versionId', 'updateDocumentVersion_versionId' - The version ID of the document.
newUpdateDocumentVersion ::
  -- | 'documentId'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  UpdateDocumentVersion
newUpdateDocumentVersion pDocumentId_ pVersionId_ =
  UpdateDocumentVersion'
    { versionStatus =
        Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_
    }

-- | The status of the version.
updateDocumentVersion_versionStatus :: Lens.Lens' UpdateDocumentVersion (Prelude.Maybe DocumentVersionStatus)
updateDocumentVersion_versionStatus = Lens.lens (\UpdateDocumentVersion' {versionStatus} -> versionStatus) (\s@UpdateDocumentVersion' {} a -> s {versionStatus = a} :: UpdateDocumentVersion)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
updateDocumentVersion_authenticationToken :: Lens.Lens' UpdateDocumentVersion (Prelude.Maybe Prelude.Text)
updateDocumentVersion_authenticationToken = Lens.lens (\UpdateDocumentVersion' {authenticationToken} -> authenticationToken) (\s@UpdateDocumentVersion' {} a -> s {authenticationToken = a} :: UpdateDocumentVersion) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the document.
updateDocumentVersion_documentId :: Lens.Lens' UpdateDocumentVersion Prelude.Text
updateDocumentVersion_documentId = Lens.lens (\UpdateDocumentVersion' {documentId} -> documentId) (\s@UpdateDocumentVersion' {} a -> s {documentId = a} :: UpdateDocumentVersion)

-- | The version ID of the document.
updateDocumentVersion_versionId :: Lens.Lens' UpdateDocumentVersion Prelude.Text
updateDocumentVersion_versionId = Lens.lens (\UpdateDocumentVersion' {versionId} -> versionId) (\s@UpdateDocumentVersion' {} a -> s {versionId = a} :: UpdateDocumentVersion)

instance Core.AWSRequest UpdateDocumentVersion where
  type
    AWSResponse UpdateDocumentVersion =
      UpdateDocumentVersionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateDocumentVersionResponse'

instance Prelude.Hashable UpdateDocumentVersion where
  hashWithSalt _salt UpdateDocumentVersion' {..} =
    _salt `Prelude.hashWithSalt` versionStatus
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData UpdateDocumentVersion where
  rnf UpdateDocumentVersion' {..} =
    Prelude.rnf versionStatus
      `Prelude.seq` Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf versionId

instance Core.ToHeaders UpdateDocumentVersion where
  toHeaders UpdateDocumentVersion' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON UpdateDocumentVersion where
  toJSON UpdateDocumentVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VersionStatus" Core..=)
              Prelude.<$> versionStatus
          ]
      )

instance Core.ToPath UpdateDocumentVersion where
  toPath UpdateDocumentVersion' {..} =
    Prelude.mconcat
      [ "/api/v1/documents/",
        Core.toBS documentId,
        "/versions/",
        Core.toBS versionId
      ]

instance Core.ToQuery UpdateDocumentVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDocumentVersionResponse' smart constructor.
data UpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDocumentVersionResponse ::
  UpdateDocumentVersionResponse
newUpdateDocumentVersionResponse =
  UpdateDocumentVersionResponse'

instance Prelude.NFData UpdateDocumentVersionResponse where
  rnf _ = ()
