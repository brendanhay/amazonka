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
-- Module      : Amazonka.WorkDocs.RestoreDocumentVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Recovers a deleted version of an Amazon WorkDocs document.
module Amazonka.WorkDocs.RestoreDocumentVersions
  ( -- * Creating a Request
    RestoreDocumentVersions (..),
    newRestoreDocumentVersions,

    -- * Request Lenses
    restoreDocumentVersions_authenticationToken,
    restoreDocumentVersions_documentId,

    -- * Destructuring the Response
    RestoreDocumentVersionsResponse (..),
    newRestoreDocumentVersionsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newRestoreDocumentVersions' smart constructor.
data RestoreDocumentVersions = RestoreDocumentVersions'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the document.
    documentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDocumentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'restoreDocumentVersions_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'documentId', 'restoreDocumentVersions_documentId' - The ID of the document.
newRestoreDocumentVersions ::
  -- | 'documentId'
  Prelude.Text ->
  RestoreDocumentVersions
newRestoreDocumentVersions pDocumentId_ =
  RestoreDocumentVersions'
    { authenticationToken =
        Prelude.Nothing,
      documentId = pDocumentId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
restoreDocumentVersions_authenticationToken :: Lens.Lens' RestoreDocumentVersions (Prelude.Maybe Prelude.Text)
restoreDocumentVersions_authenticationToken = Lens.lens (\RestoreDocumentVersions' {authenticationToken} -> authenticationToken) (\s@RestoreDocumentVersions' {} a -> s {authenticationToken = a} :: RestoreDocumentVersions) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the document.
restoreDocumentVersions_documentId :: Lens.Lens' RestoreDocumentVersions Prelude.Text
restoreDocumentVersions_documentId = Lens.lens (\RestoreDocumentVersions' {documentId} -> documentId) (\s@RestoreDocumentVersions' {} a -> s {documentId = a} :: RestoreDocumentVersions)

instance Core.AWSRequest RestoreDocumentVersions where
  type
    AWSResponse RestoreDocumentVersions =
      RestoreDocumentVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RestoreDocumentVersionsResponse'

instance Prelude.Hashable RestoreDocumentVersions where
  hashWithSalt _salt RestoreDocumentVersions' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` documentId

instance Prelude.NFData RestoreDocumentVersions where
  rnf RestoreDocumentVersions' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf documentId

instance Data.ToHeaders RestoreDocumentVersions where
  toHeaders RestoreDocumentVersions' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON RestoreDocumentVersions where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RestoreDocumentVersions where
  toPath RestoreDocumentVersions' {..} =
    Prelude.mconcat
      [ "/api/v1/documentVersions/restore/",
        Data.toBS documentId
      ]

instance Data.ToQuery RestoreDocumentVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreDocumentVersionsResponse' smart constructor.
data RestoreDocumentVersionsResponse = RestoreDocumentVersionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDocumentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRestoreDocumentVersionsResponse ::
  RestoreDocumentVersionsResponse
newRestoreDocumentVersionsResponse =
  RestoreDocumentVersionsResponse'

instance
  Prelude.NFData
    RestoreDocumentVersionsResponse
  where
  rnf _ = ()
