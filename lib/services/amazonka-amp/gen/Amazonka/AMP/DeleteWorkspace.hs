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
-- Module      : Amazonka.AMP.DeleteWorkspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AMP workspace.
module Amazonka.AMP.DeleteWorkspace
  ( -- * Creating a Request
    DeleteWorkspace (..),
    newDeleteWorkspace,

    -- * Request Lenses
    deleteWorkspace_clientToken,
    deleteWorkspace_workspaceId,

    -- * Destructuring the Response
    DeleteWorkspaceResponse (..),
    newDeleteWorkspaceResponse,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a DeleteWorkspace operation.
--
-- /See:/ 'newDeleteWorkspace' smart constructor.
data DeleteWorkspace = DeleteWorkspace'
  { -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace to delete.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteWorkspace_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'workspaceId', 'deleteWorkspace_workspaceId' - The ID of the workspace to delete.
newDeleteWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  DeleteWorkspace
newDeleteWorkspace pWorkspaceId_ =
  DeleteWorkspace'
    { clientToken = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
deleteWorkspace_clientToken :: Lens.Lens' DeleteWorkspace (Prelude.Maybe Prelude.Text)
deleteWorkspace_clientToken = Lens.lens (\DeleteWorkspace' {clientToken} -> clientToken) (\s@DeleteWorkspace' {} a -> s {clientToken = a} :: DeleteWorkspace)

-- | The ID of the workspace to delete.
deleteWorkspace_workspaceId :: Lens.Lens' DeleteWorkspace Prelude.Text
deleteWorkspace_workspaceId = Lens.lens (\DeleteWorkspace' {workspaceId} -> workspaceId) (\s@DeleteWorkspace' {} a -> s {workspaceId = a} :: DeleteWorkspace)

instance Core.AWSRequest DeleteWorkspace where
  type
    AWSResponse DeleteWorkspace =
      DeleteWorkspaceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteWorkspaceResponse'

instance Prelude.Hashable DeleteWorkspace where
  hashWithSalt _salt DeleteWorkspace' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DeleteWorkspace where
  rnf DeleteWorkspace' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf workspaceId

instance Data.ToHeaders DeleteWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteWorkspace where
  toPath DeleteWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId]

instance Data.ToQuery DeleteWorkspace where
  toQuery DeleteWorkspace' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteWorkspaceResponse' smart constructor.
data DeleteWorkspaceResponse = DeleteWorkspaceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteWorkspaceResponse ::
  DeleteWorkspaceResponse
newDeleteWorkspaceResponse = DeleteWorkspaceResponse'

instance Prelude.NFData DeleteWorkspaceResponse where
  rnf _ = ()
