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
-- Module      : Amazonka.WorkDocs.DeleteDocumentVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a document.
module Amazonka.WorkDocs.DeleteDocumentVersion
  ( -- * Creating a Request
    DeleteDocumentVersion (..),
    newDeleteDocumentVersion,

    -- * Request Lenses
    deleteDocumentVersion_authenticationToken,
    deleteDocumentVersion_documentId,
    deleteDocumentVersion_versionId,
    deleteDocumentVersion_deletePriorVersions,

    -- * Destructuring the Response
    DeleteDocumentVersionResponse (..),
    newDeleteDocumentVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDeleteDocumentVersion' smart constructor.
data DeleteDocumentVersion = DeleteDocumentVersion'
  { -- | Amazon WorkDocs authentication token. Not required when using Amazon Web
    -- Services administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the document associated with the version being deleted.
    documentId :: Prelude.Text,
    -- | The ID of the version being deleted.
    versionId :: Prelude.Text,
    -- | Deletes all versions of a document prior to the current version.
    deletePriorVersions :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deleteDocumentVersion_authenticationToken' - Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
--
-- 'documentId', 'deleteDocumentVersion_documentId' - The ID of the document associated with the version being deleted.
--
-- 'versionId', 'deleteDocumentVersion_versionId' - The ID of the version being deleted.
--
-- 'deletePriorVersions', 'deleteDocumentVersion_deletePriorVersions' - Deletes all versions of a document prior to the current version.
newDeleteDocumentVersion ::
  -- | 'documentId'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  -- | 'deletePriorVersions'
  Prelude.Bool ->
  DeleteDocumentVersion
newDeleteDocumentVersion
  pDocumentId_
  pVersionId_
  pDeletePriorVersions_ =
    DeleteDocumentVersion'
      { authenticationToken =
          Prelude.Nothing,
        documentId = pDocumentId_,
        versionId = pVersionId_,
        deletePriorVersions = pDeletePriorVersions_
      }

-- | Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
deleteDocumentVersion_authenticationToken :: Lens.Lens' DeleteDocumentVersion (Prelude.Maybe Prelude.Text)
deleteDocumentVersion_authenticationToken = Lens.lens (\DeleteDocumentVersion' {authenticationToken} -> authenticationToken) (\s@DeleteDocumentVersion' {} a -> s {authenticationToken = a} :: DeleteDocumentVersion) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the document associated with the version being deleted.
deleteDocumentVersion_documentId :: Lens.Lens' DeleteDocumentVersion Prelude.Text
deleteDocumentVersion_documentId = Lens.lens (\DeleteDocumentVersion' {documentId} -> documentId) (\s@DeleteDocumentVersion' {} a -> s {documentId = a} :: DeleteDocumentVersion)

-- | The ID of the version being deleted.
deleteDocumentVersion_versionId :: Lens.Lens' DeleteDocumentVersion Prelude.Text
deleteDocumentVersion_versionId = Lens.lens (\DeleteDocumentVersion' {versionId} -> versionId) (\s@DeleteDocumentVersion' {} a -> s {versionId = a} :: DeleteDocumentVersion)

-- | Deletes all versions of a document prior to the current version.
deleteDocumentVersion_deletePriorVersions :: Lens.Lens' DeleteDocumentVersion Prelude.Bool
deleteDocumentVersion_deletePriorVersions = Lens.lens (\DeleteDocumentVersion' {deletePriorVersions} -> deletePriorVersions) (\s@DeleteDocumentVersion' {} a -> s {deletePriorVersions = a} :: DeleteDocumentVersion)

instance Core.AWSRequest DeleteDocumentVersion where
  type
    AWSResponse DeleteDocumentVersion =
      DeleteDocumentVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteDocumentVersionResponse'

instance Prelude.Hashable DeleteDocumentVersion where
  hashWithSalt _salt DeleteDocumentVersion' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` documentId
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` deletePriorVersions

instance Prelude.NFData DeleteDocumentVersion where
  rnf DeleteDocumentVersion' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf documentId
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf deletePriorVersions

instance Data.ToHeaders DeleteDocumentVersion where
  toHeaders DeleteDocumentVersion' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteDocumentVersion where
  toPath DeleteDocumentVersion' {..} =
    Prelude.mconcat
      [ "/api/v1/documentVersions/",
        Data.toBS documentId,
        "/versions/",
        Data.toBS versionId
      ]

instance Data.ToQuery DeleteDocumentVersion where
  toQuery DeleteDocumentVersion' {..} =
    Prelude.mconcat
      ["deletePriorVersions" Data.=: deletePriorVersions]

-- | /See:/ 'newDeleteDocumentVersionResponse' smart constructor.
data DeleteDocumentVersionResponse = DeleteDocumentVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDocumentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDocumentVersionResponse ::
  DeleteDocumentVersionResponse
newDeleteDocumentVersionResponse =
  DeleteDocumentVersionResponse'

instance Prelude.NFData DeleteDocumentVersionResponse where
  rnf _ = ()
