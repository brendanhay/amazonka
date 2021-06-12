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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the folder.
    folderId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteFolder
newDeleteFolder pFolderId_ =
  DeleteFolder'
    { authenticationToken = Core.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteFolder_authenticationToken :: Lens.Lens' DeleteFolder (Core.Maybe Core.Text)
deleteFolder_authenticationToken = Lens.lens (\DeleteFolder' {authenticationToken} -> authenticationToken) (\s@DeleteFolder' {} a -> s {authenticationToken = a} :: DeleteFolder) Core.. Lens.mapping Core._Sensitive

-- | The ID of the folder.
deleteFolder_folderId :: Lens.Lens' DeleteFolder Core.Text
deleteFolder_folderId = Lens.lens (\DeleteFolder' {folderId} -> folderId) (\s@DeleteFolder' {} a -> s {folderId = a} :: DeleteFolder)

instance Core.AWSRequest DeleteFolder where
  type AWSResponse DeleteFolder = DeleteFolderResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteFolderResponse'

instance Core.Hashable DeleteFolder

instance Core.NFData DeleteFolder

instance Core.ToHeaders DeleteFolder where
  toHeaders DeleteFolder' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath DeleteFolder where
  toPath DeleteFolder' {..} =
    Core.mconcat
      ["/api/v1/folders/", Core.toBS folderId]

instance Core.ToQuery DeleteFolder where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteFolderResponse' smart constructor.
data DeleteFolderResponse = DeleteFolderResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFolderResponse ::
  DeleteFolderResponse
newDeleteFolderResponse = DeleteFolderResponse'

instance Core.NFData DeleteFolderResponse
