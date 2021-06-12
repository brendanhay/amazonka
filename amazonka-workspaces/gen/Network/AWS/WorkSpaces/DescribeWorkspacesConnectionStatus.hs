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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspacesConnectionStatus' smart constructor.
data DescribeWorkspacesConnectionStatus = DescribeWorkspacesConnectionStatus'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
    workspaceIds :: Core.Maybe (Core.NonEmpty Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      workspaceIds = Core.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspacesConnectionStatus_nextToken :: Lens.Lens' DescribeWorkspacesConnectionStatus (Core.Maybe Core.Text)
describeWorkspacesConnectionStatus_nextToken = Lens.lens (\DescribeWorkspacesConnectionStatus' {nextToken} -> nextToken) (\s@DescribeWorkspacesConnectionStatus' {} a -> s {nextToken = a} :: DescribeWorkspacesConnectionStatus)

-- | The identifiers of the WorkSpaces. You can specify up to 25 WorkSpaces.
describeWorkspacesConnectionStatus_workspaceIds :: Lens.Lens' DescribeWorkspacesConnectionStatus (Core.Maybe (Core.NonEmpty Core.Text))
describeWorkspacesConnectionStatus_workspaceIds = Lens.lens (\DescribeWorkspacesConnectionStatus' {workspaceIds} -> workspaceIds) (\s@DescribeWorkspacesConnectionStatus' {} a -> s {workspaceIds = a} :: DescribeWorkspacesConnectionStatus) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeWorkspacesConnectionStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspacesConnectionStatusResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeWorkspacesConnectionStatus_nextToken
          Lens..~ rs
          Lens.^? describeWorkspacesConnectionStatusResponse_nextToken
            Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "WorkspacesConnectionStatus"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeWorkspacesConnectionStatus

instance
  Core.NFData
    DescribeWorkspacesConnectionStatus

instance
  Core.ToHeaders
    DescribeWorkspacesConnectionStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspacesConnectionStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeWorkspacesConnectionStatus
  where
  toJSON DescribeWorkspacesConnectionStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("WorkspaceIds" Core..=) Core.<$> workspaceIds
          ]
      )

instance
  Core.ToPath
    DescribeWorkspacesConnectionStatus
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeWorkspacesConnectionStatus
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeWorkspacesConnectionStatusResponse' smart constructor.
data DescribeWorkspacesConnectionStatusResponse = DescribeWorkspacesConnectionStatusResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the connection status of the WorkSpace.
    workspacesConnectionStatus :: Core.Maybe [WorkspaceConnectionStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeWorkspacesConnectionStatusResponse
newDescribeWorkspacesConnectionStatusResponse
  pHttpStatus_ =
    DescribeWorkspacesConnectionStatusResponse'
      { nextToken =
          Core.Nothing,
        workspacesConnectionStatus =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeWorkspacesConnectionStatusResponse_nextToken :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Core.Maybe Core.Text)
describeWorkspacesConnectionStatusResponse_nextToken = Lens.lens (\DescribeWorkspacesConnectionStatusResponse' {nextToken} -> nextToken) (\s@DescribeWorkspacesConnectionStatusResponse' {} a -> s {nextToken = a} :: DescribeWorkspacesConnectionStatusResponse)

-- | Information about the connection status of the WorkSpace.
describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse (Core.Maybe [WorkspaceConnectionStatus])
describeWorkspacesConnectionStatusResponse_workspacesConnectionStatus = Lens.lens (\DescribeWorkspacesConnectionStatusResponse' {workspacesConnectionStatus} -> workspacesConnectionStatus) (\s@DescribeWorkspacesConnectionStatusResponse' {} a -> s {workspacesConnectionStatus = a} :: DescribeWorkspacesConnectionStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeWorkspacesConnectionStatusResponse_httpStatus :: Lens.Lens' DescribeWorkspacesConnectionStatusResponse Core.Int
describeWorkspacesConnectionStatusResponse_httpStatus = Lens.lens (\DescribeWorkspacesConnectionStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspacesConnectionStatusResponse' {} a -> s {httpStatus = a} :: DescribeWorkspacesConnectionStatusResponse)

instance
  Core.NFData
    DescribeWorkspacesConnectionStatusResponse
