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
-- Module      : Network.AWS.Grafana.DeleteWorkspace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Managed Grafana workspace.
module Network.AWS.Grafana.DeleteWorkspace
  ( -- * Creating a Request
    DeleteWorkspace (..),
    newDeleteWorkspace,

    -- * Request Lenses
    deleteWorkspace_workspaceId,

    -- * Destructuring the Response
    DeleteWorkspaceResponse (..),
    newDeleteWorkspaceResponse,

    -- * Response Lenses
    deleteWorkspaceResponse_httpStatus,
    deleteWorkspaceResponse_workspace,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Grafana.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWorkspace' smart constructor.
data DeleteWorkspace = DeleteWorkspace'
  { -- | The ID of the workspace to delete.
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
-- 'workspaceId', 'deleteWorkspace_workspaceId' - The ID of the workspace to delete.
newDeleteWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  DeleteWorkspace
newDeleteWorkspace pWorkspaceId_ =
  DeleteWorkspace' {workspaceId = pWorkspaceId_}

-- | The ID of the workspace to delete.
deleteWorkspace_workspaceId :: Lens.Lens' DeleteWorkspace Prelude.Text
deleteWorkspace_workspaceId = Lens.lens (\DeleteWorkspace' {workspaceId} -> workspaceId) (\s@DeleteWorkspace' {} a -> s {workspaceId = a} :: DeleteWorkspace)

instance Core.AWSRequest DeleteWorkspace where
  type
    AWSResponse DeleteWorkspace =
      DeleteWorkspaceResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "workspace")
      )

instance Prelude.Hashable DeleteWorkspace

instance Prelude.NFData DeleteWorkspace

instance Core.ToHeaders DeleteWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteWorkspace where
  toPath DeleteWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Core.toBS workspaceId]

instance Core.ToQuery DeleteWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkspaceResponse' smart constructor.
data DeleteWorkspaceResponse = DeleteWorkspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing information about the workspace that was deleted.
    workspace :: WorkspaceDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkspaceResponse_httpStatus' - The response's http status code.
--
-- 'workspace', 'deleteWorkspaceResponse_workspace' - A structure containing information about the workspace that was deleted.
newDeleteWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspace'
  WorkspaceDescription ->
  DeleteWorkspaceResponse
newDeleteWorkspaceResponse pHttpStatus_ pWorkspace_ =
  DeleteWorkspaceResponse'
    { httpStatus = pHttpStatus_,
      workspace = pWorkspace_
    }

-- | The response's http status code.
deleteWorkspaceResponse_httpStatus :: Lens.Lens' DeleteWorkspaceResponse Prelude.Int
deleteWorkspaceResponse_httpStatus = Lens.lens (\DeleteWorkspaceResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkspaceResponse' {} a -> s {httpStatus = a} :: DeleteWorkspaceResponse)

-- | A structure containing information about the workspace that was deleted.
deleteWorkspaceResponse_workspace :: Lens.Lens' DeleteWorkspaceResponse WorkspaceDescription
deleteWorkspaceResponse_workspace = Lens.lens (\DeleteWorkspaceResponse' {workspace} -> workspace) (\s@DeleteWorkspaceResponse' {} a -> s {workspace = a} :: DeleteWorkspaceResponse)

instance Prelude.NFData DeleteWorkspaceResponse
