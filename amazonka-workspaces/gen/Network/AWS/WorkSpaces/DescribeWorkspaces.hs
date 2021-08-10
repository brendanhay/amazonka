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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified WorkSpaces.
--
-- You can filter the results by using the bundle identifier, directory
-- identifier, or owner, but you can specify only one filter at a time.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaces
  ( -- * Creating a Request
    DescribeWorkspaces (..),
    newDescribeWorkspaces,

    -- * Request Lenses
    describeWorkspaces_nextToken,
    describeWorkspaces_bundleId,
    describeWorkspaces_workspaceIds,
    describeWorkspaces_directoryId,
    describeWorkspaces_userName,
    describeWorkspaces_limit,

    -- * Destructuring the Response
    DescribeWorkspacesResponse (..),
    newDescribeWorkspacesResponse,

    -- * Response Lenses
    describeWorkspacesResponse_nextToken,
    describeWorkspacesResponse_workspaces,
    describeWorkspacesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaces' smart constructor.
data DescribeWorkspaces = DescribeWorkspaces'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bundle. All WorkSpaces that are created from this
    -- bundle are retrieved. You cannot combine this parameter with any other
    -- filter.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the WorkSpaces. You cannot combine this parameter
    -- with any other filter.
    --
    -- Because the CreateWorkspaces operation is asynchronous, the identifier
    -- it returns is not immediately available. If you immediately call
    -- DescribeWorkspaces with this identifier, no information is returned.
    workspaceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The identifier of the directory. In addition, you can optionally specify
    -- a specific directory user (see @UserName@). You cannot combine this
    -- parameter with any other filter.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the directory user. You must specify this parameter with
    -- @DirectoryId@.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspaces_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'bundleId', 'describeWorkspaces_bundleId' - The identifier of the bundle. All WorkSpaces that are created from this
-- bundle are retrieved. You cannot combine this parameter with any other
-- filter.
--
-- 'workspaceIds', 'describeWorkspaces_workspaceIds' - The identifiers of the WorkSpaces. You cannot combine this parameter
-- with any other filter.
--
-- Because the CreateWorkspaces operation is asynchronous, the identifier
-- it returns is not immediately available. If you immediately call
-- DescribeWorkspaces with this identifier, no information is returned.
--
-- 'directoryId', 'describeWorkspaces_directoryId' - The identifier of the directory. In addition, you can optionally specify
-- a specific directory user (see @UserName@). You cannot combine this
-- parameter with any other filter.
--
-- 'userName', 'describeWorkspaces_userName' - The name of the directory user. You must specify this parameter with
-- @DirectoryId@.
--
-- 'limit', 'describeWorkspaces_limit' - The maximum number of items to return.
newDescribeWorkspaces ::
  DescribeWorkspaces
newDescribeWorkspaces =
  DescribeWorkspaces'
    { nextToken = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      workspaceIds = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      userName = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaces_nextToken :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_nextToken = Lens.lens (\DescribeWorkspaces' {nextToken} -> nextToken) (\s@DescribeWorkspaces' {} a -> s {nextToken = a} :: DescribeWorkspaces)

-- | The identifier of the bundle. All WorkSpaces that are created from this
-- bundle are retrieved. You cannot combine this parameter with any other
-- filter.
describeWorkspaces_bundleId :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_bundleId = Lens.lens (\DescribeWorkspaces' {bundleId} -> bundleId) (\s@DescribeWorkspaces' {} a -> s {bundleId = a} :: DescribeWorkspaces)

-- | The identifiers of the WorkSpaces. You cannot combine this parameter
-- with any other filter.
--
-- Because the CreateWorkspaces operation is asynchronous, the identifier
-- it returns is not immediately available. If you immediately call
-- DescribeWorkspaces with this identifier, no information is returned.
describeWorkspaces_workspaceIds :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWorkspaces_workspaceIds = Lens.lens (\DescribeWorkspaces' {workspaceIds} -> workspaceIds) (\s@DescribeWorkspaces' {} a -> s {workspaceIds = a} :: DescribeWorkspaces) Prelude.. Lens.mapping Lens._Coerce

-- | The identifier of the directory. In addition, you can optionally specify
-- a specific directory user (see @UserName@). You cannot combine this
-- parameter with any other filter.
describeWorkspaces_directoryId :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_directoryId = Lens.lens (\DescribeWorkspaces' {directoryId} -> directoryId) (\s@DescribeWorkspaces' {} a -> s {directoryId = a} :: DescribeWorkspaces)

-- | The name of the directory user. You must specify this parameter with
-- @DirectoryId@.
describeWorkspaces_userName :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_userName = Lens.lens (\DescribeWorkspaces' {userName} -> userName) (\s@DescribeWorkspaces' {} a -> s {userName = a} :: DescribeWorkspaces)

-- | The maximum number of items to return.
describeWorkspaces_limit :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Natural)
describeWorkspaces_limit = Lens.lens (\DescribeWorkspaces' {limit} -> limit) (\s@DescribeWorkspaces' {} a -> s {limit = a} :: DescribeWorkspaces)

instance Core.AWSPager DescribeWorkspaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspacesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspacesResponse_workspaces
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeWorkspaces_nextToken
          Lens..~ rs
          Lens.^? describeWorkspacesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeWorkspaces where
  type
    AWSResponse DescribeWorkspaces =
      DescribeWorkspacesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspacesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Workspaces" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorkspaces

instance Prelude.NFData DescribeWorkspaces

instance Core.ToHeaders DescribeWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorkspaces where
  toJSON DescribeWorkspaces' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("BundleId" Core..=) Prelude.<$> bundleId,
            ("WorkspaceIds" Core..=) Prelude.<$> workspaceIds,
            ("DirectoryId" Core..=) Prelude.<$> directoryId,
            ("UserName" Core..=) Prelude.<$> userName,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeWorkspaces where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspacesResponse' smart constructor.
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the WorkSpaces.
    --
    -- Because CreateWorkspaces is an asynchronous operation, some of the
    -- returned information could be incomplete.
    workspaces :: Prelude.Maybe [Workspace],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspacesResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'workspaces', 'describeWorkspacesResponse_workspaces' - Information about the WorkSpaces.
--
-- Because CreateWorkspaces is an asynchronous operation, some of the
-- returned information could be incomplete.
--
-- 'httpStatus', 'describeWorkspacesResponse_httpStatus' - The response's http status code.
newDescribeWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkspacesResponse
newDescribeWorkspacesResponse pHttpStatus_ =
  DescribeWorkspacesResponse'
    { nextToken =
        Prelude.Nothing,
      workspaces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeWorkspacesResponse_nextToken :: Lens.Lens' DescribeWorkspacesResponse (Prelude.Maybe Prelude.Text)
describeWorkspacesResponse_nextToken = Lens.lens (\DescribeWorkspacesResponse' {nextToken} -> nextToken) (\s@DescribeWorkspacesResponse' {} a -> s {nextToken = a} :: DescribeWorkspacesResponse)

-- | Information about the WorkSpaces.
--
-- Because CreateWorkspaces is an asynchronous operation, some of the
-- returned information could be incomplete.
describeWorkspacesResponse_workspaces :: Lens.Lens' DescribeWorkspacesResponse (Prelude.Maybe [Workspace])
describeWorkspacesResponse_workspaces = Lens.lens (\DescribeWorkspacesResponse' {workspaces} -> workspaces) (\s@DescribeWorkspacesResponse' {} a -> s {workspaces = a} :: DescribeWorkspacesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeWorkspacesResponse_httpStatus :: Lens.Lens' DescribeWorkspacesResponse Prelude.Int
describeWorkspacesResponse_httpStatus = Lens.lens (\DescribeWorkspacesResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspacesResponse' {} a -> s {httpStatus = a} :: DescribeWorkspacesResponse)

instance Prelude.NFData DescribeWorkspacesResponse
