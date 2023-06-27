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
-- Module      : Amazonka.WorkDocs.DeleteFolderContents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the contents of the specified folder.
module Amazonka.WorkDocs.DeleteFolderContents
  ( -- * Creating a Request
    DeleteFolderContents (..),
    newDeleteFolderContents,

    -- * Request Lenses
    deleteFolderContents_authenticationToken,
    deleteFolderContents_folderId,

    -- * Destructuring the Response
    DeleteFolderContentsResponse (..),
    newDeleteFolderContentsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDeleteFolderContents' smart constructor.
data DeleteFolderContents = DeleteFolderContents'
  { -- | Amazon WorkDocs authentication token. Not required when using Amazon Web
    -- Services administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderContents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deleteFolderContents_authenticationToken' - Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
--
-- 'folderId', 'deleteFolderContents_folderId' - The ID of the folder.
newDeleteFolderContents ::
  -- | 'folderId'
  Prelude.Text ->
  DeleteFolderContents
newDeleteFolderContents pFolderId_ =
  DeleteFolderContents'
    { authenticationToken =
        Prelude.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using Amazon Web
-- Services administrator credentials to access the API.
deleteFolderContents_authenticationToken :: Lens.Lens' DeleteFolderContents (Prelude.Maybe Prelude.Text)
deleteFolderContents_authenticationToken = Lens.lens (\DeleteFolderContents' {authenticationToken} -> authenticationToken) (\s@DeleteFolderContents' {} a -> s {authenticationToken = a} :: DeleteFolderContents) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the folder.
deleteFolderContents_folderId :: Lens.Lens' DeleteFolderContents Prelude.Text
deleteFolderContents_folderId = Lens.lens (\DeleteFolderContents' {folderId} -> folderId) (\s@DeleteFolderContents' {} a -> s {folderId = a} :: DeleteFolderContents)

instance Core.AWSRequest DeleteFolderContents where
  type
    AWSResponse DeleteFolderContents =
      DeleteFolderContentsResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteFolderContentsResponse'

instance Prelude.Hashable DeleteFolderContents where
  hashWithSalt _salt DeleteFolderContents' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData DeleteFolderContents where
  rnf DeleteFolderContents' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders DeleteFolderContents where
  toHeaders DeleteFolderContents' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteFolderContents where
  toPath DeleteFolderContents' {..} =
    Prelude.mconcat
      ["/api/v1/folders/", Data.toBS folderId, "/contents"]

instance Data.ToQuery DeleteFolderContents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFolderContentsResponse' smart constructor.
data DeleteFolderContentsResponse = DeleteFolderContentsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderContentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFolderContentsResponse ::
  DeleteFolderContentsResponse
newDeleteFolderContentsResponse =
  DeleteFolderContentsResponse'

instance Prelude.NFData DeleteFolderContentsResponse where
  rnf _ = ()
