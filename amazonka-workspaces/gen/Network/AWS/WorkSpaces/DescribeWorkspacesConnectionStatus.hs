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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection status of the specified WorkSpaces.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
  ( -- * Creating a Request
    DescribeWorkspacesConnectionStatus (..),
    newDescribeWorkspacesConnectionStatus,

    -- * Request Lenses
    describeWorkspacesConnectionStatus_nextToken,
    describeWorkspacesConnectionStatus_workspaceIds,

    -- * Destructuring the Response
    DescribeWorkspacesConnectionStatusResponse (..),
    newDescribeWorkspacesConnectionStatusResponse,

    -- * Response Lenses
    describeWorkspacesConnectionStatusResponse_nextToken,
    describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus,
    describeWorkspacesConnectionStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspacesConnectionStatus' smart constructor.
data DescribeWorkspacesConnectionStatus = DescribeWorkspacesConnectionStatus'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
    workspaceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspacesConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspacesConnectionStatus_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'workspaceIds', 'describeWorkspacesConnectionStatus_workspaceIds' - The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
newDescribeWorkspacesConnectionStatus ::
  DescribeWorkspacesConnectionStatus
newDescribeWorkspacesConnectionStatus =
  DescribeWorkspacesConnectionStatus'
    { nextToken =
        Prelude.Nothing,
      workspaceIds = Prelude.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspacesConnectionStatus_nextToken :: Lens.Lens' DescribeWorkspacesConnectionStatus (Prelude.Maybe Prelude.Text)
describeWorkspacesConnectionStatus_nextToken = Lens.lens (\DescribeWorkspacesConnectionStatus' {nextToken} -> nextToken) (\s@DescribeWorkspacesConnectionStatus' {} a -> s {nextToken = a} :: DescribeWorkspacesConnectionStatus)

-- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
describeWorkspacesConnectionStatus_workspaceIds :: Lens.Lens' DescribeWorkspacesConnectionStatus (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWorkspacesConnectionStatus_workspaceIds = Lens.lens (\DescribeWorkspacesConnectionStatus' {workspaceIds} -> workspaceIds) (\s@DescribeWorkspacesConnectionStatus' {} a -> s {workspaceIds = a} :: DescribeWorkspacesConnectionStatus) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeWorkspacesConnectionStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspacesConnectionStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeWorkspacesConnectionStatus_nextToken
          Lens..~ rs
          Lens.^? describeWorkspacesConnectionStatusResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeWorkspacesConnectionStatus
  where
  type
    AWSResponse DescribeWorkspacesConnectionStatus =
      DescribeWorkspacesConnectionStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspacesConnectionStatusResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "WorkspacesConnectionStatus"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeWorkspacesConnectionStatus

instance
  Prelude.NFData
    DescribeWorkspacesConnectionStatus

instance
  Core.ToHeaders
    DescribeWorkspacesConnectionStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspacesConnectionStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeWorkspacesConnectionStatus
  where
  toJSON DescribeWorkspacesConnectionStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("WorkspaceIds" Core..=) Prelude.<$> workspaceIds
          ]
      )

instance
  Core.ToPath
    DescribeWorkspacesConnectionStatus
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeWorkspacesConnectionStatus
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspacesConnectionStatusResponse' smart constructor.
data DescribeWorkspacesConnectionStatusResponse = DescribeWorkspacesConnectionStatusResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the connection status of the WorkSpace.
    workspacesConnectionStatus :: Prelude.Maybe [WorkspaceConnectionStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspacesConnectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspacesConnectionStatusResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'workspacesConnectionStatus', 'describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus' - Information about the connection status of the WorkSpace.
--
-- 'httpStatus', 'describeWorkspacesConnectionStatusResponse_httpStatus' - The response's http status code.
newDescribeWorkspacesConnectionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkspacesConnectionStatusResponse
newDescribeWorkspacesConnectionStatusResponse
  pHttpStatus_ =
    DescribeWorkspacesConnectionStatusResponse'
      { nextToken =
          Prelude.Nothing,
        workspacesConnectionStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeWorkspacesConnectionStatusResponse_nextToken :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Prelude.Maybe Prelude.Text)
describeWorkspacesConnectionStatusResponse_nextToken = Lens.lens (\DescribeWorkspacesConnectionStatusResponse' {nextToken} -> nextToken) (\s@DescribeWorkspacesConnectionStatusResponse' {} a -> s {nextToken = a} :: DescribeWorkspacesConnectionStatusResponse)

-- | Information about the connection status of the WorkSpace.
describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Prelude.Maybe [WorkspaceConnectionStatus])
describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus = Lens.lens (\DescribeWorkspacesConnectionStatusResponse' {workspacesConnectionStatus} -> workspacesConnectionStatus) (\s@DescribeWorkspacesConnectionStatusResponse' {} a -> s {workspacesConnectionStatus = a} :: DescribeWorkspacesConnectionStatusResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeWorkspacesConnectionStatusResponse_httpStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse Prelude.Int
describeWorkspacesConnectionStatusResponse_httpStatus = Lens.lens (\DescribeWorkspacesConnectionStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspacesConnectionStatusResponse' {} a -> s {httpStatus = a} :: DescribeWorkspacesConnectionStatusResponse)

instance
  Prelude.NFData
    DescribeWorkspacesConnectionStatusResponse
