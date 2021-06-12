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
-- Module      : Network.AWS.Connect.ListUserHierarchyGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the hierarchy groups for the
-- specified Amazon Connect instance.
--
-- For more information about agent hierarchies, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/agent-hierarchy.html Set Up Agent Hierarchies>
-- in the /Amazon Connect Administrator Guide/.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListUserHierarchyGroups
  ( -- * Creating a Request
    ListUserHierarchyGroups (..),
    newListUserHierarchyGroups,

    -- * Request Lenses
    listUserHierarchyGroups_nextToken,
    listUserHierarchyGroups_maxResults,
    listUserHierarchyGroups_instanceId,

    -- * Destructuring the Response
    ListUserHierarchyGroupsResponse (..),
    newListUserHierarchyGroupsResponse,

    -- * Response Lenses
    listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList,
    listUserHierarchyGroupsResponse_nextToken,
    listUserHierarchyGroupsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUserHierarchyGroups' smart constructor.
data ListUserHierarchyGroups = ListUserHierarchyGroups'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserHierarchyGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUserHierarchyGroups_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listUserHierarchyGroups_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listUserHierarchyGroups_instanceId' - The identifier of the Amazon Connect instance.
newListUserHierarchyGroups ::
  -- | 'instanceId'
  Core.Text ->
  ListUserHierarchyGroups
newListUserHierarchyGroups pInstanceId_ =
  ListUserHierarchyGroups'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listUserHierarchyGroups_nextToken :: Lens.Lens' ListUserHierarchyGroups (Core.Maybe Core.Text)
listUserHierarchyGroups_nextToken = Lens.lens (\ListUserHierarchyGroups' {nextToken} -> nextToken) (\s@ListUserHierarchyGroups' {} a -> s {nextToken = a} :: ListUserHierarchyGroups)

-- | The maximum number of results to return per page.
listUserHierarchyGroups_maxResults :: Lens.Lens' ListUserHierarchyGroups (Core.Maybe Core.Natural)
listUserHierarchyGroups_maxResults = Lens.lens (\ListUserHierarchyGroups' {maxResults} -> maxResults) (\s@ListUserHierarchyGroups' {} a -> s {maxResults = a} :: ListUserHierarchyGroups)

-- | The identifier of the Amazon Connect instance.
listUserHierarchyGroups_instanceId :: Lens.Lens' ListUserHierarchyGroups Core.Text
listUserHierarchyGroups_instanceId = Lens.lens (\ListUserHierarchyGroups' {instanceId} -> instanceId) (\s@ListUserHierarchyGroups' {} a -> s {instanceId = a} :: ListUserHierarchyGroups)

instance Core.AWSPager ListUserHierarchyGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserHierarchyGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listUserHierarchyGroups_nextToken
          Lens..~ rs
          Lens.^? listUserHierarchyGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListUserHierarchyGroups where
  type
    AWSResponse ListUserHierarchyGroups =
      ListUserHierarchyGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserHierarchyGroupsResponse'
            Core.<$> ( x Core..?> "UserHierarchyGroupSummaryList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUserHierarchyGroups

instance Core.NFData ListUserHierarchyGroups

instance Core.ToHeaders ListUserHierarchyGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListUserHierarchyGroups where
  toPath ListUserHierarchyGroups' {..} =
    Core.mconcat
      [ "/user-hierarchy-groups-summary/",
        Core.toBS instanceId
      ]

instance Core.ToQuery ListUserHierarchyGroups where
  toQuery ListUserHierarchyGroups' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListUserHierarchyGroupsResponse' smart constructor.
data ListUserHierarchyGroupsResponse = ListUserHierarchyGroupsResponse'
  { -- | Information about the hierarchy groups.
    userHierarchyGroupSummaryList :: Core.Maybe [HierarchyGroupSummary],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserHierarchyGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userHierarchyGroupSummaryList', 'listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList' - Information about the hierarchy groups.
--
-- 'nextToken', 'listUserHierarchyGroupsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listUserHierarchyGroupsResponse_httpStatus' - The response's http status code.
newListUserHierarchyGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUserHierarchyGroupsResponse
newListUserHierarchyGroupsResponse pHttpStatus_ =
  ListUserHierarchyGroupsResponse'
    { userHierarchyGroupSummaryList =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the hierarchy groups.
listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList :: Lens.Lens' ListUserHierarchyGroupsResponse (Core.Maybe [HierarchyGroupSummary])
listUserHierarchyGroupsResponse_userHierarchyGroupSummaryList = Lens.lens (\ListUserHierarchyGroupsResponse' {userHierarchyGroupSummaryList} -> userHierarchyGroupSummaryList) (\s@ListUserHierarchyGroupsResponse' {} a -> s {userHierarchyGroupSummaryList = a} :: ListUserHierarchyGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | If there are additional results, this is the token for the next set of
-- results.
listUserHierarchyGroupsResponse_nextToken :: Lens.Lens' ListUserHierarchyGroupsResponse (Core.Maybe Core.Text)
listUserHierarchyGroupsResponse_nextToken = Lens.lens (\ListUserHierarchyGroupsResponse' {nextToken} -> nextToken) (\s@ListUserHierarchyGroupsResponse' {} a -> s {nextToken = a} :: ListUserHierarchyGroupsResponse)

-- | The response's http status code.
listUserHierarchyGroupsResponse_httpStatus :: Lens.Lens' ListUserHierarchyGroupsResponse Core.Int
listUserHierarchyGroupsResponse_httpStatus = Lens.lens (\ListUserHierarchyGroupsResponse' {httpStatus} -> httpStatus) (\s@ListUserHierarchyGroupsResponse' {} a -> s {httpStatus = a} :: ListUserHierarchyGroupsResponse)

instance Core.NFData ListUserHierarchyGroupsResponse
