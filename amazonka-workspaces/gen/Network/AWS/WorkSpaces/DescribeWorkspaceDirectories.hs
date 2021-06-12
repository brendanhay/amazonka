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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available directories that are registered with Amazon
-- WorkSpaces.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
  ( -- * Creating a Request
    DescribeWorkspaceDirectories (..),
    newDescribeWorkspaceDirectories,

    -- * Request Lenses
    describeWorkspaceDirectories_nextToken,
    describeWorkspaceDirectories_directoryIds,
    describeWorkspaceDirectories_limit,

    -- * Destructuring the Response
    DescribeWorkspaceDirectoriesResponse (..),
    newDescribeWorkspaceDirectoriesResponse,

    -- * Response Lenses
    describeWorkspaceDirectoriesResponse_nextToken,
    describeWorkspaceDirectoriesResponse_directories,
    describeWorkspaceDirectoriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceDirectories' smart constructor.
data DescribeWorkspaceDirectories = DescribeWorkspaceDirectories'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The identifiers of the directories. If the value is null, all
    -- directories are retrieved.
    directoryIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The maximum number of directories to return.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceDirectories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspaceDirectories_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'directoryIds', 'describeWorkspaceDirectories_directoryIds' - The identifiers of the directories. If the value is null, all
-- directories are retrieved.
--
-- 'limit', 'describeWorkspaceDirectories_limit' - The maximum number of directories to return.
newDescribeWorkspaceDirectories ::
  DescribeWorkspaceDirectories
newDescribeWorkspaceDirectories =
  DescribeWorkspaceDirectories'
    { nextToken =
        Core.Nothing,
      directoryIds = Core.Nothing,
      limit = Core.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaceDirectories_nextToken :: Lens.Lens' DescribeWorkspaceDirectories (Core.Maybe Core.Text)
describeWorkspaceDirectories_nextToken = Lens.lens (\DescribeWorkspaceDirectories' {nextToken} -> nextToken) (\s@DescribeWorkspaceDirectories' {} a -> s {nextToken = a} :: DescribeWorkspaceDirectories)

-- | The identifiers of the directories. If the value is null, all
-- directories are retrieved.
describeWorkspaceDirectories_directoryIds :: Lens.Lens' DescribeWorkspaceDirectories (Core.Maybe (Core.NonEmpty Core.Text))
describeWorkspaceDirectories_directoryIds = Lens.lens (\DescribeWorkspaceDirectories' {directoryIds} -> directoryIds) (\s@DescribeWorkspaceDirectories' {} a -> s {directoryIds = a} :: DescribeWorkspaceDirectories) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of directories to return.
describeWorkspaceDirectories_limit :: Lens.Lens' DescribeWorkspaceDirectories (Core.Maybe Core.Natural)
describeWorkspaceDirectories_limit = Lens.lens (\DescribeWorkspaceDirectories' {limit} -> limit) (\s@DescribeWorkspaceDirectories' {} a -> s {limit = a} :: DescribeWorkspaceDirectories)

instance Core.AWSPager DescribeWorkspaceDirectories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceDirectoriesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceDirectoriesResponse_directories
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeWorkspaceDirectories_nextToken
          Lens..~ rs
          Lens.^? describeWorkspaceDirectoriesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeWorkspaceDirectories where
  type
    AWSResponse DescribeWorkspaceDirectories =
      DescribeWorkspaceDirectoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceDirectoriesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Directories" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeWorkspaceDirectories

instance Core.NFData DescribeWorkspaceDirectories

instance Core.ToHeaders DescribeWorkspaceDirectories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspaceDirectories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeWorkspaceDirectories where
  toJSON DescribeWorkspaceDirectories' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("DirectoryIds" Core..=) Core.<$> directoryIds,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeWorkspaceDirectories where
  toPath = Core.const "/"

instance Core.ToQuery DescribeWorkspaceDirectories where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeWorkspaceDirectoriesResponse' smart constructor.
data DescribeWorkspaceDirectoriesResponse = DescribeWorkspaceDirectoriesResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the directories.
    directories :: Core.Maybe [WorkspaceDirectory],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceDirectoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspaceDirectoriesResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'directories', 'describeWorkspaceDirectoriesResponse_directories' - Information about the directories.
--
-- 'httpStatus', 'describeWorkspaceDirectoriesResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceDirectoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeWorkspaceDirectoriesResponse
newDescribeWorkspaceDirectoriesResponse pHttpStatus_ =
  DescribeWorkspaceDirectoriesResponse'
    { nextToken =
        Core.Nothing,
      directories = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeWorkspaceDirectoriesResponse_nextToken :: Lens.Lens' DescribeWorkspaceDirectoriesResponse (Core.Maybe Core.Text)
describeWorkspaceDirectoriesResponse_nextToken = Lens.lens (\DescribeWorkspaceDirectoriesResponse' {nextToken} -> nextToken) (\s@DescribeWorkspaceDirectoriesResponse' {} a -> s {nextToken = a} :: DescribeWorkspaceDirectoriesResponse)

-- | Information about the directories.
describeWorkspaceDirectoriesResponse_directories :: Lens.Lens' DescribeWorkspaceDirectoriesResponse (Core.Maybe [WorkspaceDirectory])
describeWorkspaceDirectoriesResponse_directories = Lens.lens (\DescribeWorkspaceDirectoriesResponse' {directories} -> directories) (\s@DescribeWorkspaceDirectoriesResponse' {} a -> s {directories = a} :: DescribeWorkspaceDirectoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeWorkspaceDirectoriesResponse_httpStatus :: Lens.Lens' DescribeWorkspaceDirectoriesResponse Core.Int
describeWorkspaceDirectoriesResponse_httpStatus = Lens.lens (\DescribeWorkspaceDirectoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceDirectoriesResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceDirectoriesResponse)

instance
  Core.NFData
    DescribeWorkspaceDirectoriesResponse
