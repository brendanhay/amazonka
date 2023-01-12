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
-- Module      : Amazonka.WorkSpaces.DescribeWorkspaces
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.WorkSpaces.DescribeWorkspaces
  ( -- * Creating a Request
    DescribeWorkspaces (..),
    newDescribeWorkspaces,

    -- * Request Lenses
    describeWorkspaces_bundleId,
    describeWorkspaces_directoryId,
    describeWorkspaces_limit,
    describeWorkspaces_nextToken,
    describeWorkspaces_userName,
    describeWorkspaces_workspaceIds,

    -- * Destructuring the Response
    DescribeWorkspacesResponse (..),
    newDescribeWorkspacesResponse,

    -- * Response Lenses
    describeWorkspacesResponse_nextToken,
    describeWorkspacesResponse_workspaces,
    describeWorkspacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaces' smart constructor.
data DescribeWorkspaces = DescribeWorkspaces'
  { -- | The identifier of the bundle. All WorkSpaces that are created from this
    -- bundle are retrieved. You cannot combine this parameter with any other
    -- filter.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory. In addition, you can optionally specify
    -- a specific directory user (see @UserName@). You cannot combine this
    -- parameter with any other filter.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the directory user. You must specify this parameter with
    -- @DirectoryId@.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the WorkSpaces. You cannot combine this parameter
    -- with any other filter.
    --
    -- Because the CreateWorkspaces operation is asynchronous, the identifier
    -- it returns is not immediately available. If you immediately call
    -- DescribeWorkspaces with this identifier, no information is returned.
    workspaceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'bundleId', 'describeWorkspaces_bundleId' - The identifier of the bundle. All WorkSpaces that are created from this
-- bundle are retrieved. You cannot combine this parameter with any other
-- filter.
--
-- 'directoryId', 'describeWorkspaces_directoryId' - The identifier of the directory. In addition, you can optionally specify
-- a specific directory user (see @UserName@). You cannot combine this
-- parameter with any other filter.
--
-- 'limit', 'describeWorkspaces_limit' - The maximum number of items to return.
--
-- 'nextToken', 'describeWorkspaces_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'userName', 'describeWorkspaces_userName' - The name of the directory user. You must specify this parameter with
-- @DirectoryId@.
--
-- 'workspaceIds', 'describeWorkspaces_workspaceIds' - The identifiers of the WorkSpaces. You cannot combine this parameter
-- with any other filter.
--
-- Because the CreateWorkspaces operation is asynchronous, the identifier
-- it returns is not immediately available. If you immediately call
-- DescribeWorkspaces with this identifier, no information is returned.
newDescribeWorkspaces ::
  DescribeWorkspaces
newDescribeWorkspaces =
  DescribeWorkspaces'
    { bundleId = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      userName = Prelude.Nothing,
      workspaceIds = Prelude.Nothing
    }

-- | The identifier of the bundle. All WorkSpaces that are created from this
-- bundle are retrieved. You cannot combine this parameter with any other
-- filter.
describeWorkspaces_bundleId :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_bundleId = Lens.lens (\DescribeWorkspaces' {bundleId} -> bundleId) (\s@DescribeWorkspaces' {} a -> s {bundleId = a} :: DescribeWorkspaces)

-- | The identifier of the directory. In addition, you can optionally specify
-- a specific directory user (see @UserName@). You cannot combine this
-- parameter with any other filter.
describeWorkspaces_directoryId :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_directoryId = Lens.lens (\DescribeWorkspaces' {directoryId} -> directoryId) (\s@DescribeWorkspaces' {} a -> s {directoryId = a} :: DescribeWorkspaces)

-- | The maximum number of items to return.
describeWorkspaces_limit :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Natural)
describeWorkspaces_limit = Lens.lens (\DescribeWorkspaces' {limit} -> limit) (\s@DescribeWorkspaces' {} a -> s {limit = a} :: DescribeWorkspaces)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaces_nextToken :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_nextToken = Lens.lens (\DescribeWorkspaces' {nextToken} -> nextToken) (\s@DescribeWorkspaces' {} a -> s {nextToken = a} :: DescribeWorkspaces)

-- | The name of the directory user. You must specify this parameter with
-- @DirectoryId@.
describeWorkspaces_userName :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe Prelude.Text)
describeWorkspaces_userName = Lens.lens (\DescribeWorkspaces' {userName} -> userName) (\s@DescribeWorkspaces' {} a -> s {userName = a} :: DescribeWorkspaces)

-- | The identifiers of the WorkSpaces. You cannot combine this parameter
-- with any other filter.
--
-- Because the CreateWorkspaces operation is asynchronous, the identifier
-- it returns is not immediately available. If you immediately call
-- DescribeWorkspaces with this identifier, no information is returned.
describeWorkspaces_workspaceIds :: Lens.Lens' DescribeWorkspaces (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWorkspaces_workspaceIds = Lens.lens (\DescribeWorkspaces' {workspaceIds} -> workspaceIds) (\s@DescribeWorkspaces' {} a -> s {workspaceIds = a} :: DescribeWorkspaces) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspacesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Workspaces" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorkspaces where
  hashWithSalt _salt DescribeWorkspaces' {..} =
    _salt `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` workspaceIds

instance Prelude.NFData DescribeWorkspaces where
  rnf DescribeWorkspaces' {..} =
    Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf workspaceIds

instance Data.ToHeaders DescribeWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWorkspaces where
  toJSON DescribeWorkspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BundleId" Data..=) Prelude.<$> bundleId,
            ("DirectoryId" Data..=) Prelude.<$> directoryId,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("UserName" Data..=) Prelude.<$> userName,
            ("WorkspaceIds" Data..=) Prelude.<$> workspaceIds
          ]
      )

instance Data.ToPath DescribeWorkspaces where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspacesResponse' smart constructor.
data DescribeWorkspacesResponse = DescribeWorkspacesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
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
-- 'nextToken', 'describeWorkspacesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
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

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeWorkspacesResponse_nextToken :: Lens.Lens' DescribeWorkspacesResponse (Prelude.Maybe Prelude.Text)
describeWorkspacesResponse_nextToken = Lens.lens (\DescribeWorkspacesResponse' {nextToken} -> nextToken) (\s@DescribeWorkspacesResponse' {} a -> s {nextToken = a} :: DescribeWorkspacesResponse)

-- | Information about the WorkSpaces.
--
-- Because CreateWorkspaces is an asynchronous operation, some of the
-- returned information could be incomplete.
describeWorkspacesResponse_workspaces :: Lens.Lens' DescribeWorkspacesResponse (Prelude.Maybe [Workspace])
describeWorkspacesResponse_workspaces = Lens.lens (\DescribeWorkspacesResponse' {workspaces} -> workspaces) (\s@DescribeWorkspacesResponse' {} a -> s {workspaces = a} :: DescribeWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeWorkspacesResponse_httpStatus :: Lens.Lens' DescribeWorkspacesResponse Prelude.Int
describeWorkspacesResponse_httpStatus = Lens.lens (\DescribeWorkspacesResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspacesResponse' {} a -> s {httpStatus = a} :: DescribeWorkspacesResponse)

instance Prelude.NFData DescribeWorkspacesResponse where
  rnf DescribeWorkspacesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workspaces
      `Prelude.seq` Prelude.rnf httpStatus
