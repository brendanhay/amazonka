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
-- Module      : Amazonka.Grafana.DeleteWorkspaceApiKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Grafana API key for the workspace.
module Amazonka.Grafana.DeleteWorkspaceApiKey
  ( -- * Creating a Request
    DeleteWorkspaceApiKey (..),
    newDeleteWorkspaceApiKey,

    -- * Request Lenses
    deleteWorkspaceApiKey_keyName,
    deleteWorkspaceApiKey_workspaceId,

    -- * Destructuring the Response
    DeleteWorkspaceApiKeyResponse (..),
    newDeleteWorkspaceApiKeyResponse,

    -- * Response Lenses
    deleteWorkspaceApiKeyResponse_httpStatus,
    deleteWorkspaceApiKeyResponse_keyName,
    deleteWorkspaceApiKeyResponse_workspaceId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkspaceApiKey' smart constructor.
data DeleteWorkspaceApiKey = DeleteWorkspaceApiKey'
  { -- | The name of the API key to delete.
    keyName :: Prelude.Text,
    -- | The ID of the workspace to delete.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'deleteWorkspaceApiKey_keyName' - The name of the API key to delete.
--
-- 'workspaceId', 'deleteWorkspaceApiKey_workspaceId' - The ID of the workspace to delete.
newDeleteWorkspaceApiKey ::
  -- | 'keyName'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  DeleteWorkspaceApiKey
newDeleteWorkspaceApiKey pKeyName_ pWorkspaceId_ =
  DeleteWorkspaceApiKey'
    { keyName = pKeyName_,
      workspaceId = pWorkspaceId_
    }

-- | The name of the API key to delete.
deleteWorkspaceApiKey_keyName :: Lens.Lens' DeleteWorkspaceApiKey Prelude.Text
deleteWorkspaceApiKey_keyName = Lens.lens (\DeleteWorkspaceApiKey' {keyName} -> keyName) (\s@DeleteWorkspaceApiKey' {} a -> s {keyName = a} :: DeleteWorkspaceApiKey)

-- | The ID of the workspace to delete.
deleteWorkspaceApiKey_workspaceId :: Lens.Lens' DeleteWorkspaceApiKey Prelude.Text
deleteWorkspaceApiKey_workspaceId = Lens.lens (\DeleteWorkspaceApiKey' {workspaceId} -> workspaceId) (\s@DeleteWorkspaceApiKey' {} a -> s {workspaceId = a} :: DeleteWorkspaceApiKey)

instance Core.AWSRequest DeleteWorkspaceApiKey where
  type
    AWSResponse DeleteWorkspaceApiKey =
      DeleteWorkspaceApiKeyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkspaceApiKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "keyName")
            Prelude.<*> (x Data..:> "workspaceId")
      )

instance Prelude.Hashable DeleteWorkspaceApiKey where
  hashWithSalt _salt DeleteWorkspaceApiKey' {..} =
    _salt `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DeleteWorkspaceApiKey where
  rnf DeleteWorkspaceApiKey' {..} =
    Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders DeleteWorkspaceApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteWorkspaceApiKey where
  toPath DeleteWorkspaceApiKey' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/apikeys/",
        Data.toBS keyName
      ]

instance Data.ToQuery DeleteWorkspaceApiKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkspaceApiKeyResponse' smart constructor.
data DeleteWorkspaceApiKeyResponse = DeleteWorkspaceApiKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the key that was deleted.
    keyName :: Prelude.Text,
    -- | The ID of the workspace where the key was deleted.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceApiKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkspaceApiKeyResponse_httpStatus' - The response's http status code.
--
-- 'keyName', 'deleteWorkspaceApiKeyResponse_keyName' - The name of the key that was deleted.
--
-- 'workspaceId', 'deleteWorkspaceApiKeyResponse_workspaceId' - The ID of the workspace where the key was deleted.
newDeleteWorkspaceApiKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  DeleteWorkspaceApiKeyResponse
newDeleteWorkspaceApiKeyResponse
  pHttpStatus_
  pKeyName_
  pWorkspaceId_ =
    DeleteWorkspaceApiKeyResponse'
      { httpStatus =
          pHttpStatus_,
        keyName = pKeyName_,
        workspaceId = pWorkspaceId_
      }

-- | The response's http status code.
deleteWorkspaceApiKeyResponse_httpStatus :: Lens.Lens' DeleteWorkspaceApiKeyResponse Prelude.Int
deleteWorkspaceApiKeyResponse_httpStatus = Lens.lens (\DeleteWorkspaceApiKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkspaceApiKeyResponse' {} a -> s {httpStatus = a} :: DeleteWorkspaceApiKeyResponse)

-- | The name of the key that was deleted.
deleteWorkspaceApiKeyResponse_keyName :: Lens.Lens' DeleteWorkspaceApiKeyResponse Prelude.Text
deleteWorkspaceApiKeyResponse_keyName = Lens.lens (\DeleteWorkspaceApiKeyResponse' {keyName} -> keyName) (\s@DeleteWorkspaceApiKeyResponse' {} a -> s {keyName = a} :: DeleteWorkspaceApiKeyResponse)

-- | The ID of the workspace where the key was deleted.
deleteWorkspaceApiKeyResponse_workspaceId :: Lens.Lens' DeleteWorkspaceApiKeyResponse Prelude.Text
deleteWorkspaceApiKeyResponse_workspaceId = Lens.lens (\DeleteWorkspaceApiKeyResponse' {workspaceId} -> workspaceId) (\s@DeleteWorkspaceApiKeyResponse' {} a -> s {workspaceId = a} :: DeleteWorkspaceApiKeyResponse)

instance Prelude.NFData DeleteWorkspaceApiKeyResponse where
  rnf DeleteWorkspaceApiKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf workspaceId
