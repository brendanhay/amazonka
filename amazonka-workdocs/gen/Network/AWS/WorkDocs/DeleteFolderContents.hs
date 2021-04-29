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
-- Module      : Network.AWS.WorkDocs.DeleteFolderContents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the contents of the specified folder.
module Network.AWS.WorkDocs.DeleteFolderContents
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteFolderContents' smart constructor.
data DeleteFolderContents = DeleteFolderContents'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderContents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deleteFolderContents_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
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

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteFolderContents_authenticationToken :: Lens.Lens' DeleteFolderContents (Prelude.Maybe Prelude.Text)
deleteFolderContents_authenticationToken = Lens.lens (\DeleteFolderContents' {authenticationToken} -> authenticationToken) (\s@DeleteFolderContents' {} a -> s {authenticationToken = a} :: DeleteFolderContents) Prelude.. Lens.mapping Prelude._Sensitive

-- | The ID of the folder.
deleteFolderContents_folderId :: Lens.Lens' DeleteFolderContents Prelude.Text
deleteFolderContents_folderId = Lens.lens (\DeleteFolderContents' {folderId} -> folderId) (\s@DeleteFolderContents' {} a -> s {folderId = a} :: DeleteFolderContents)

instance Prelude.AWSRequest DeleteFolderContents where
  type
    Rs DeleteFolderContents =
      DeleteFolderContentsResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteFolderContentsResponse'

instance Prelude.Hashable DeleteFolderContents

instance Prelude.NFData DeleteFolderContents

instance Prelude.ToHeaders DeleteFolderContents where
  toHeaders DeleteFolderContents' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DeleteFolderContents where
  toPath DeleteFolderContents' {..} =
    Prelude.mconcat
      [ "/api/v1/folders/",
        Prelude.toBS folderId,
        "/contents"
      ]

instance Prelude.ToQuery DeleteFolderContents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFolderContentsResponse' smart constructor.
data DeleteFolderContentsResponse = DeleteFolderContentsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderContentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFolderContentsResponse ::
  DeleteFolderContentsResponse
newDeleteFolderContentsResponse =
  DeleteFolderContentsResponse'

instance Prelude.NFData DeleteFolderContentsResponse
