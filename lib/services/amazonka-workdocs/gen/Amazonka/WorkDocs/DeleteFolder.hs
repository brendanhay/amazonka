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
-- Module      : Amazonka.WorkDocs.DeleteFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified folder and its contents.
module Amazonka.WorkDocs.DeleteFolder
  ( -- * Creating a Request
    DeleteFolder (..),
    newDeleteFolder,

    -- * Request Lenses
    deleteFolder_authenticationToken,
    deleteFolder_folderId,

    -- * Destructuring the Response
    DeleteFolderResponse (..),
    newDeleteFolderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDeleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deleteFolder_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'folderId', 'deleteFolder_folderId' - The ID of the folder.
newDeleteFolder ::
  -- | 'folderId'
  Prelude.Text ->
  DeleteFolder
newDeleteFolder pFolderId_ =
  DeleteFolder'
    { authenticationToken =
        Prelude.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteFolder_authenticationToken :: Lens.Lens' DeleteFolder (Prelude.Maybe Prelude.Text)
deleteFolder_authenticationToken = Lens.lens (\DeleteFolder' {authenticationToken} -> authenticationToken) (\s@DeleteFolder' {} a -> s {authenticationToken = a} :: DeleteFolder) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the folder.
deleteFolder_folderId :: Lens.Lens' DeleteFolder Prelude.Text
deleteFolder_folderId = Lens.lens (\DeleteFolder' {folderId} -> folderId) (\s@DeleteFolder' {} a -> s {folderId = a} :: DeleteFolder)

instance Core.AWSRequest DeleteFolder where
  type AWSResponse DeleteFolder = DeleteFolderResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteFolderResponse'

instance Prelude.Hashable DeleteFolder where
  hashWithSalt _salt DeleteFolder' {..} =
    _salt `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData DeleteFolder where
  rnf DeleteFolder' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders DeleteFolder where
  toHeaders DeleteFolder' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteFolder where
  toPath DeleteFolder' {..} =
    Prelude.mconcat
      ["/api/v1/folders/", Data.toBS folderId]

instance Data.ToQuery DeleteFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFolderResponse' smart constructor.
data DeleteFolderResponse = DeleteFolderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFolderResponse ::
  DeleteFolderResponse
newDeleteFolderResponse = DeleteFolderResponse'

instance Prelude.NFData DeleteFolderResponse where
  rnf _ = ()
