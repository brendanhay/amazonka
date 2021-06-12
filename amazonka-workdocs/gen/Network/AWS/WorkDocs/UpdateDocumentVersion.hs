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
-- Module      : Network.AWS.WorkDocs.UpdateDocumentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.WorkDocs.UpdateDocumentVersion
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newUpdateDocumentVersion' smart constructor.
data UpdateDocumentVersion = UpdateDocumentVersion'
  { -- | The status of the version.
    versionStatus :: Core.Maybe DocumentVersionStatus,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the document.
    documentId :: Core.Text,
    -- | The version ID of the document.
    versionId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'versionId'
  Core.Text ->
  UpdateDocumentVersion
newUpdateDocumentVersion pDocumentId_ pVersionId_ =
  UpdateDocumentVersion'
    { versionStatus =
        Core.Nothing,
      authenticationToken = Core.Nothing,
      documentId = pDocumentId_,
      versionId = pVersionId_
    }

-- | The status of the version.
updateDocumentVersion_versionStatus :: Lens.Lens' UpdateDocumentVersion (Core.Maybe DocumentVersionStatus)
updateDocumentVersion_versionStatus = Lens.lens (\UpdateDocumentVersion' {versionStatus} -> versionStatus) (\s@UpdateDocumentVersion' {} a -> s {versionStatus = a} :: UpdateDocumentVersion)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
updateDocumentVersion_authenticationToken :: Lens.Lens' UpdateDocumentVersion (Core.Maybe Core.Text)
updateDocumentVersion_authenticationToken = Lens.lens (\UpdateDocumentVersion' {authenticationToken} -> authenticationToken) (\s@UpdateDocumentVersion' {} a -> s {authenticationToken = a} :: UpdateDocumentVersion) Core.. Lens.mapping Core._Sensitive

-- | The ID of the document.
updateDocumentVersion_documentId :: Lens.Lens' UpdateDocumentVersion Core.Text
updateDocumentVersion_documentId = Lens.lens (\UpdateDocumentVersion' {documentId} -> documentId) (\s@UpdateDocumentVersion' {} a -> s {documentId = a} :: UpdateDocumentVersion)

-- | The version ID of the document.
updateDocumentVersion_versionId :: Lens.Lens' UpdateDocumentVersion Core.Text
updateDocumentVersion_versionId = Lens.lens (\UpdateDocumentVersion' {versionId} -> versionId) (\s@UpdateDocumentVersion' {} a -> s {versionId = a} :: UpdateDocumentVersion)

instance Core.AWSRequest UpdateDocumentVersion where
  type
    AWSResponse UpdateDocumentVersion =
      UpdateDocumentVersionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveNull UpdateDocumentVersionResponse'

instance Core.Hashable UpdateDocumentVersion

instance Core.NFData UpdateDocumentVersion

instance Core.ToHeaders UpdateDocumentVersion where
  toHeaders UpdateDocumentVersion' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON UpdateDocumentVersion where
  toJSON UpdateDocumentVersion' {..} =
    Core.object
      ( Core.catMaybes
          [("VersionStatus" Core..=) Core.<$> versionStatus]
      )

instance Core.ToPath UpdateDocumentVersion where
  toPath UpdateDocumentVersion' {..} =
    Core.mconcat
      [ "/api/v1/documents/",
        Core.toBS documentId,
        "/versions/",
        Core.toBS versionId
      ]

instance Core.ToQuery UpdateDocumentVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDocumentVersionResponse' smart constructor.
data UpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDocumentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDocumentVersionResponse ::
  UpdateDocumentVersionResponse
newUpdateDocumentVersionResponse =
  UpdateDocumentVersionResponse'

instance Core.NFData UpdateDocumentVersionResponse
