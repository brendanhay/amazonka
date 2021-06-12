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
-- Module      : Network.AWS.WorkSpaces.DescribeIpGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your IP access control groups.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeIpGroups
  ( -- * Creating a Request
    DescribeIpGroups (..),
    newDescribeIpGroups,

    -- * Request Lenses
    describeIpGroups_nextToken,
    describeIpGroups_groupIds,
    describeIpGroups_maxResults,

    -- * Destructuring the Response
    DescribeIpGroupsResponse (..),
    newDescribeIpGroupsResponse,

    -- * Response Lenses
    describeIpGroupsResponse_nextToken,
    describeIpGroupsResponse_result,
    describeIpGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeIpGroups' smart constructor.
data DescribeIpGroups = DescribeIpGroups'
  { -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The identifiers of one or more IP access control groups.
    groupIds :: Core.Maybe [Core.Text],
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIpGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeIpGroups_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'groupIds', 'describeIpGroups_groupIds' - The identifiers of one or more IP access control groups.
--
-- 'maxResults', 'describeIpGroups_maxResults' - The maximum number of items to return.
newDescribeIpGroups ::
  DescribeIpGroups
newDescribeIpGroups =
  DescribeIpGroups'
    { nextToken = Core.Nothing,
      groupIds = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeIpGroups_nextToken :: Lens.Lens' DescribeIpGroups (Core.Maybe Core.Text)
describeIpGroups_nextToken = Lens.lens (\DescribeIpGroups' {nextToken} -> nextToken) (\s@DescribeIpGroups' {} a -> s {nextToken = a} :: DescribeIpGroups)

-- | The identifiers of one or more IP access control groups.
describeIpGroups_groupIds :: Lens.Lens' DescribeIpGroups (Core.Maybe [Core.Text])
describeIpGroups_groupIds = Lens.lens (\DescribeIpGroups' {groupIds} -> groupIds) (\s@DescribeIpGroups' {} a -> s {groupIds = a} :: DescribeIpGroups) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return.
describeIpGroups_maxResults :: Lens.Lens' DescribeIpGroups (Core.Maybe Core.Natural)
describeIpGroups_maxResults = Lens.lens (\DescribeIpGroups' {maxResults} -> maxResults) (\s@DescribeIpGroups' {} a -> s {maxResults = a} :: DescribeIpGroups)

instance Core.AWSPager DescribeIpGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpGroupsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpGroupsResponse_result Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeIpGroups_nextToken
          Lens..~ rs
          Lens.^? describeIpGroupsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeIpGroups where
  type
    AWSResponse DescribeIpGroups =
      DescribeIpGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIpGroupsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Result" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeIpGroups

instance Core.NFData DescribeIpGroups

instance Core.ToHeaders DescribeIpGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeIpGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeIpGroups where
  toJSON DescribeIpGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("GroupIds" Core..=) Core.<$> groupIds,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath DescribeIpGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeIpGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeIpGroupsResponse' smart constructor.
data DescribeIpGroupsResponse = DescribeIpGroupsResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the IP access control groups.
    result :: Core.Maybe [WorkspacesIpGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeIpGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeIpGroupsResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'result', 'describeIpGroupsResponse_result' - Information about the IP access control groups.
--
-- 'httpStatus', 'describeIpGroupsResponse_httpStatus' - The response's http status code.
newDescribeIpGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeIpGroupsResponse
newDescribeIpGroupsResponse pHttpStatus_ =
  DescribeIpGroupsResponse'
    { nextToken = Core.Nothing,
      result = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeIpGroupsResponse_nextToken :: Lens.Lens' DescribeIpGroupsResponse (Core.Maybe Core.Text)
describeIpGroupsResponse_nextToken = Lens.lens (\DescribeIpGroupsResponse' {nextToken} -> nextToken) (\s@DescribeIpGroupsResponse' {} a -> s {nextToken = a} :: DescribeIpGroupsResponse)

-- | Information about the IP access control groups.
describeIpGroupsResponse_result :: Lens.Lens' DescribeIpGroupsResponse (Core.Maybe [WorkspacesIpGroup])
describeIpGroupsResponse_result = Lens.lens (\DescribeIpGroupsResponse' {result} -> result) (\s@DescribeIpGroupsResponse' {} a -> s {result = a} :: DescribeIpGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeIpGroupsResponse_httpStatus :: Lens.Lens' DescribeIpGroupsResponse Core.Int
describeIpGroupsResponse_httpStatus = Lens.lens (\DescribeIpGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeIpGroupsResponse' {} a -> s {httpStatus = a} :: DescribeIpGroupsResponse)

instance Core.NFData DescribeIpGroupsResponse
