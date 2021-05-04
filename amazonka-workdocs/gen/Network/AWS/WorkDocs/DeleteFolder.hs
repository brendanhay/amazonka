{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkDocs.DeleteFolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified folder and its contents.
module Network.AWS.WorkDocs.DeleteFolder
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
deleteFolder_authenticationToken = Lens.lens (\DeleteFolder' {authenticationToken} -> authenticationToken) (\s@DeleteFolder' {} a -> s {authenticationToken = a} :: DeleteFolder) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the folder.
deleteFolder_folderId :: Lens.Lens' DeleteFolder Prelude.Text
deleteFolder_folderId = Lens.lens (\DeleteFolder' {folderId} -> folderId) (\s@DeleteFolder' {} a -> s {folderId = a} :: DeleteFolder)

instance Prelude.AWSRequest DeleteFolder where
  type Rs DeleteFolder = DeleteFolderResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteFolderResponse'

instance Prelude.Hashable DeleteFolder

instance Prelude.NFData DeleteFolder

instance Prelude.ToHeaders DeleteFolder where
  toHeaders DeleteFolder' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DeleteFolder where
  toPath DeleteFolder' {..} =
    Prelude.mconcat
      ["/api/v1/folders/", Prelude.toBS folderId]

instance Prelude.ToQuery DeleteFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFolderResponse' smart constructor.
data DeleteFolderResponse = DeleteFolderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFolderResponse ::
  DeleteFolderResponse
newDeleteFolderResponse = DeleteFolderResponse'

instance Prelude.NFData DeleteFolderResponse
