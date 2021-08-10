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
-- Module      : Network.AWS.WorkSpaces.CreateWorkspaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more WorkSpaces.
--
-- This operation is asynchronous and returns before the WorkSpaces are
-- created.
module Network.AWS.WorkSpaces.CreateWorkspaces
  ( -- * Creating a Request
    CreateWorkspaces (..),
    newCreateWorkspaces,

    -- * Request Lenses
    createWorkspaces_workspaces,

    -- * Destructuring the Response
    CreateWorkspacesResponse (..),
    newCreateWorkspacesResponse,

    -- * Response Lenses
    createWorkspacesResponse_failedRequests,
    createWorkspacesResponse_pendingRequests,
    createWorkspacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newCreateWorkspaces' smart constructor.
data CreateWorkspaces = CreateWorkspaces'
  { -- | The WorkSpaces to create. You can specify up to 25 WorkSpaces.
    workspaces :: Prelude.NonEmpty WorkspaceRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaces', 'createWorkspaces_workspaces' - The WorkSpaces to create. You can specify up to 25 WorkSpaces.
newCreateWorkspaces ::
  -- | 'workspaces'
  Prelude.NonEmpty WorkspaceRequest ->
  CreateWorkspaces
newCreateWorkspaces pWorkspaces_ =
  CreateWorkspaces'
    { workspaces =
        Lens._Coerce Lens.# pWorkspaces_
    }

-- | The WorkSpaces to create. You can specify up to 25 WorkSpaces.
createWorkspaces_workspaces :: Lens.Lens' CreateWorkspaces (Prelude.NonEmpty WorkspaceRequest)
createWorkspaces_workspaces = Lens.lens (\CreateWorkspaces' {workspaces} -> workspaces) (\s@CreateWorkspaces' {} a -> s {workspaces = a} :: CreateWorkspaces) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateWorkspaces where
  type
    AWSResponse CreateWorkspaces =
      CreateWorkspacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkspacesResponse'
            Prelude.<$> (x Core..?> "FailedRequests" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "PendingRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkspaces

instance Prelude.NFData CreateWorkspaces

instance Core.ToHeaders CreateWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.CreateWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorkspaces where
  toJSON CreateWorkspaces' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Workspaces" Core..= workspaces)]
      )

instance Core.ToPath CreateWorkspaces where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkspacesResponse' smart constructor.
data CreateWorkspacesResponse = CreateWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be created.
    failedRequests :: Prelude.Maybe [FailedCreateWorkspaceRequest],
    -- | Information about the WorkSpaces that were created.
    --
    -- Because this operation is asynchronous, the identifier returned is not
    -- immediately available for use with other operations. For example, if you
    -- call DescribeWorkspaces before the WorkSpace is created, the information
    -- returned can be incomplete.
    pendingRequests :: Prelude.Maybe [Workspace],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRequests', 'createWorkspacesResponse_failedRequests' - Information about the WorkSpaces that could not be created.
--
-- 'pendingRequests', 'createWorkspacesResponse_pendingRequests' - Information about the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier returned is not
-- immediately available for use with other operations. For example, if you
-- call DescribeWorkspaces before the WorkSpace is created, the information
-- returned can be incomplete.
--
-- 'httpStatus', 'createWorkspacesResponse_httpStatus' - The response's http status code.
newCreateWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkspacesResponse
newCreateWorkspacesResponse pHttpStatus_ =
  CreateWorkspacesResponse'
    { failedRequests =
        Prelude.Nothing,
      pendingRequests = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WorkSpaces that could not be created.
createWorkspacesResponse_failedRequests :: Lens.Lens' CreateWorkspacesResponse (Prelude.Maybe [FailedCreateWorkspaceRequest])
createWorkspacesResponse_failedRequests = Lens.lens (\CreateWorkspacesResponse' {failedRequests} -> failedRequests) (\s@CreateWorkspacesResponse' {} a -> s {failedRequests = a} :: CreateWorkspacesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier returned is not
-- immediately available for use with other operations. For example, if you
-- call DescribeWorkspaces before the WorkSpace is created, the information
-- returned can be incomplete.
createWorkspacesResponse_pendingRequests :: Lens.Lens' CreateWorkspacesResponse (Prelude.Maybe [Workspace])
createWorkspacesResponse_pendingRequests = Lens.lens (\CreateWorkspacesResponse' {pendingRequests} -> pendingRequests) (\s@CreateWorkspacesResponse' {} a -> s {pendingRequests = a} :: CreateWorkspacesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createWorkspacesResponse_httpStatus :: Lens.Lens' CreateWorkspacesResponse Prelude.Int
createWorkspacesResponse_httpStatus = Lens.lens (\CreateWorkspacesResponse' {httpStatus} -> httpStatus) (\s@CreateWorkspacesResponse' {} a -> s {httpStatus = a} :: CreateWorkspacesResponse)

instance Prelude.NFData CreateWorkspacesResponse
