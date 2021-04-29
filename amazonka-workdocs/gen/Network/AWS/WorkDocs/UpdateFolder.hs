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
-- Module      : Network.AWS.WorkDocs.UpdateFolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified folder. The user must
-- have access to both the folder and its parent folder, if applicable.
module Network.AWS.WorkDocs.UpdateFolder
  ( -- * Creating a Request
    UpdateFolder (..),
    newUpdateFolder,

    -- * Request Lenses
    updateFolder_parentFolderId,
    updateFolder_name,
    updateFolder_authenticationToken,
    updateFolder_resourceState,
    updateFolder_folderId,

    -- * Destructuring the Response
    UpdateFolderResponse (..),
    newUpdateFolderResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newUpdateFolder' smart constructor.
data UpdateFolder = UpdateFolder'
  { -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The name of the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The resource state of the folder. Only ACTIVE and RECYCLED are accepted
    -- values from the API.
    resourceState :: Prelude.Maybe ResourceStateType,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentFolderId', 'updateFolder_parentFolderId' - The ID of the parent folder.
--
-- 'name', 'updateFolder_name' - The name of the folder.
--
-- 'authenticationToken', 'updateFolder_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'resourceState', 'updateFolder_resourceState' - The resource state of the folder. Only ACTIVE and RECYCLED are accepted
-- values from the API.
--
-- 'folderId', 'updateFolder_folderId' - The ID of the folder.
newUpdateFolder ::
  -- | 'folderId'
  Prelude.Text ->
  UpdateFolder
newUpdateFolder pFolderId_ =
  UpdateFolder'
    { parentFolderId = Prelude.Nothing,
      name = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      resourceState = Prelude.Nothing,
      folderId = pFolderId_
    }

-- | The ID of the parent folder.
updateFolder_parentFolderId :: Lens.Lens' UpdateFolder (Prelude.Maybe Prelude.Text)
updateFolder_parentFolderId = Lens.lens (\UpdateFolder' {parentFolderId} -> parentFolderId) (\s@UpdateFolder' {} a -> s {parentFolderId = a} :: UpdateFolder)

-- | The name of the folder.
updateFolder_name :: Lens.Lens' UpdateFolder (Prelude.Maybe Prelude.Text)
updateFolder_name = Lens.lens (\UpdateFolder' {name} -> name) (\s@UpdateFolder' {} a -> s {name = a} :: UpdateFolder)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
updateFolder_authenticationToken :: Lens.Lens' UpdateFolder (Prelude.Maybe Prelude.Text)
updateFolder_authenticationToken = Lens.lens (\UpdateFolder' {authenticationToken} -> authenticationToken) (\s@UpdateFolder' {} a -> s {authenticationToken = a} :: UpdateFolder) Prelude.. Lens.mapping Prelude._Sensitive

-- | The resource state of the folder. Only ACTIVE and RECYCLED are accepted
-- values from the API.
updateFolder_resourceState :: Lens.Lens' UpdateFolder (Prelude.Maybe ResourceStateType)
updateFolder_resourceState = Lens.lens (\UpdateFolder' {resourceState} -> resourceState) (\s@UpdateFolder' {} a -> s {resourceState = a} :: UpdateFolder)

-- | The ID of the folder.
updateFolder_folderId :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_folderId = Lens.lens (\UpdateFolder' {folderId} -> folderId) (\s@UpdateFolder' {} a -> s {folderId = a} :: UpdateFolder)

instance Prelude.AWSRequest UpdateFolder where
  type Rs UpdateFolder = UpdateFolderResponse
  request = Request.patchJSON defaultService
  response = Response.receiveNull UpdateFolderResponse'

instance Prelude.Hashable UpdateFolder

instance Prelude.NFData UpdateFolder

instance Prelude.ToHeaders UpdateFolder where
  toHeaders UpdateFolder' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON UpdateFolder where
  toJSON UpdateFolder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ParentFolderId" Prelude..=)
              Prelude.<$> parentFolderId,
            ("Name" Prelude..=) Prelude.<$> name,
            ("ResourceState" Prelude..=)
              Prelude.<$> resourceState
          ]
      )

instance Prelude.ToPath UpdateFolder where
  toPath UpdateFolder' {..} =
    Prelude.mconcat
      ["/api/v1/folders/", Prelude.toBS folderId]

instance Prelude.ToQuery UpdateFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFolderResponse' smart constructor.
data UpdateFolderResponse = UpdateFolderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateFolderResponse ::
  UpdateFolderResponse
newUpdateFolderResponse = UpdateFolderResponse'

instance Prelude.NFData UpdateFolderResponse
