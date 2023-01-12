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
-- Module      : Amazonka.WorkDocs.UpdateFolder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified folder. The user must
-- have access to both the folder and its parent folder, if applicable.
module Amazonka.WorkDocs.UpdateFolder
  ( -- * Creating a Request
    UpdateFolder (..),
    newUpdateFolder,

    -- * Request Lenses
    updateFolder_authenticationToken,
    updateFolder_name,
    updateFolder_parentFolderId,
    updateFolder_resourceState,
    updateFolder_folderId,

    -- * Destructuring the Response
    UpdateFolderResponse (..),
    newUpdateFolderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newUpdateFolder' smart constructor.
data UpdateFolder = UpdateFolder'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the folder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the parent folder.
    parentFolderId :: Prelude.Maybe Prelude.Text,
    -- | The resource state of the folder. Only ACTIVE and RECYCLED are accepted
    -- values from the API.
    resourceState :: Prelude.Maybe ResourceStateType,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'updateFolder_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'name', 'updateFolder_name' - The name of the folder.
--
-- 'parentFolderId', 'updateFolder_parentFolderId' - The ID of the parent folder.
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
    { authenticationToken =
        Prelude.Nothing,
      name = Prelude.Nothing,
      parentFolderId = Prelude.Nothing,
      resourceState = Prelude.Nothing,
      folderId = pFolderId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
updateFolder_authenticationToken :: Lens.Lens' UpdateFolder (Prelude.Maybe Prelude.Text)
updateFolder_authenticationToken = Lens.lens (\UpdateFolder' {authenticationToken} -> authenticationToken) (\s@UpdateFolder' {} a -> s {authenticationToken = a} :: UpdateFolder) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the folder.
updateFolder_name :: Lens.Lens' UpdateFolder (Prelude.Maybe Prelude.Text)
updateFolder_name = Lens.lens (\UpdateFolder' {name} -> name) (\s@UpdateFolder' {} a -> s {name = a} :: UpdateFolder)

-- | The ID of the parent folder.
updateFolder_parentFolderId :: Lens.Lens' UpdateFolder (Prelude.Maybe Prelude.Text)
updateFolder_parentFolderId = Lens.lens (\UpdateFolder' {parentFolderId} -> parentFolderId) (\s@UpdateFolder' {} a -> s {parentFolderId = a} :: UpdateFolder)

-- | The resource state of the folder. Only ACTIVE and RECYCLED are accepted
-- values from the API.
updateFolder_resourceState :: Lens.Lens' UpdateFolder (Prelude.Maybe ResourceStateType)
updateFolder_resourceState = Lens.lens (\UpdateFolder' {resourceState} -> resourceState) (\s@UpdateFolder' {} a -> s {resourceState = a} :: UpdateFolder)

-- | The ID of the folder.
updateFolder_folderId :: Lens.Lens' UpdateFolder Prelude.Text
updateFolder_folderId = Lens.lens (\UpdateFolder' {folderId} -> folderId) (\s@UpdateFolder' {} a -> s {folderId = a} :: UpdateFolder)

instance Core.AWSRequest UpdateFolder where
  type AWSResponse UpdateFolder = UpdateFolderResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response = Response.receiveNull UpdateFolderResponse'

instance Prelude.Hashable UpdateFolder where
  hashWithSalt _salt UpdateFolder' {..} =
    _salt `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parentFolderId
      `Prelude.hashWithSalt` resourceState
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData UpdateFolder where
  rnf UpdateFolder' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parentFolderId
      `Prelude.seq` Prelude.rnf resourceState
      `Prelude.seq` Prelude.rnf folderId

instance Data.ToHeaders UpdateFolder where
  toHeaders UpdateFolder' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON UpdateFolder where
  toJSON UpdateFolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("ParentFolderId" Data..=)
              Prelude.<$> parentFolderId,
            ("ResourceState" Data..=) Prelude.<$> resourceState
          ]
      )

instance Data.ToPath UpdateFolder where
  toPath UpdateFolder' {..} =
    Prelude.mconcat
      ["/api/v1/folders/", Data.toBS folderId]

instance Data.ToQuery UpdateFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFolderResponse' smart constructor.
data UpdateFolderResponse = UpdateFolderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateFolderResponse ::
  UpdateFolderResponse
newUpdateFolderResponse = UpdateFolderResponse'

instance Prelude.NFData UpdateFolderResponse where
  rnf _ = ()
