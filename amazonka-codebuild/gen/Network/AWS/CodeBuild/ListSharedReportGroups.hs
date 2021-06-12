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
-- Module      : Network.AWS.CodeBuild.ListSharedReportGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of report groups that are shared with other AWS accounts or
-- users.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedReportGroups
  ( -- * Creating a Request
    ListSharedReportGroups (..),
    newListSharedReportGroups,

    -- * Request Lenses
    listSharedReportGroups_sortOrder,
    listSharedReportGroups_nextToken,
    listSharedReportGroups_maxResults,
    listSharedReportGroups_sortBy,

    -- * Destructuring the Response
    ListSharedReportGroupsResponse (..),
    newListSharedReportGroupsResponse,

    -- * Response Lenses
    listSharedReportGroupsResponse_nextToken,
    listSharedReportGroupsResponse_reportGroups,
    listSharedReportGroupsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSharedReportGroups' smart constructor.
data ListSharedReportGroups = ListSharedReportGroups'
  { -- | The order in which to list shared report groups. Valid values include:
    --
    -- -   @ASCENDING@: List in ascending order.
    --
    -- -   @DESCENDING@: List in descending order.
    sortOrder :: Core.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of paginated shared report groups per response. Use
    -- @nextToken@ to iterate pages in the list of returned @ReportGroup@
    -- objects. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The criterion to be used to list report groups shared with the current
    -- AWS account or user. Valid values include:
    --
    -- -   @ARN@: List based on the ARN.
    --
    -- -   @MODIFIED_TIME@: List based on when information about the shared
    --     report group was last changed.
    sortBy :: Core.Maybe SharedResourceSortByType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSharedReportGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listSharedReportGroups_sortOrder' - The order in which to list shared report groups. Valid values include:
--
-- -   @ASCENDING@: List in ascending order.
--
-- -   @DESCENDING@: List in descending order.
--
-- 'nextToken', 'listSharedReportGroups_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'maxResults', 'listSharedReportGroups_maxResults' - The maximum number of paginated shared report groups per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
--
-- 'sortBy', 'listSharedReportGroups_sortBy' - The criterion to be used to list report groups shared with the current
-- AWS account or user. Valid values include:
--
-- -   @ARN@: List based on the ARN.
--
-- -   @MODIFIED_TIME@: List based on when information about the shared
--     report group was last changed.
newListSharedReportGroups ::
  ListSharedReportGroups
newListSharedReportGroups =
  ListSharedReportGroups'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | The order in which to list shared report groups. Valid values include:
--
-- -   @ASCENDING@: List in ascending order.
--
-- -   @DESCENDING@: List in descending order.
listSharedReportGroups_sortOrder :: Lens.Lens' ListSharedReportGroups (Core.Maybe SortOrderType)
listSharedReportGroups_sortOrder = Lens.lens (\ListSharedReportGroups' {sortOrder} -> sortOrder) (\s@ListSharedReportGroups' {} a -> s {sortOrder = a} :: ListSharedReportGroups)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listSharedReportGroups_nextToken :: Lens.Lens' ListSharedReportGroups (Core.Maybe Core.Text)
listSharedReportGroups_nextToken = Lens.lens (\ListSharedReportGroups' {nextToken} -> nextToken) (\s@ListSharedReportGroups' {} a -> s {nextToken = a} :: ListSharedReportGroups)

-- | The maximum number of paginated shared report groups per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
listSharedReportGroups_maxResults :: Lens.Lens' ListSharedReportGroups (Core.Maybe Core.Natural)
listSharedReportGroups_maxResults = Lens.lens (\ListSharedReportGroups' {maxResults} -> maxResults) (\s@ListSharedReportGroups' {} a -> s {maxResults = a} :: ListSharedReportGroups)

-- | The criterion to be used to list report groups shared with the current
-- AWS account or user. Valid values include:
--
-- -   @ARN@: List based on the ARN.
--
-- -   @MODIFIED_TIME@: List based on when information about the shared
--     report group was last changed.
listSharedReportGroups_sortBy :: Lens.Lens' ListSharedReportGroups (Core.Maybe SharedResourceSortByType)
listSharedReportGroups_sortBy = Lens.lens (\ListSharedReportGroups' {sortBy} -> sortBy) (\s@ListSharedReportGroups' {} a -> s {sortBy = a} :: ListSharedReportGroups)

instance Core.AWSPager ListSharedReportGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSharedReportGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSharedReportGroupsResponse_reportGroups
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSharedReportGroups_nextToken
          Lens..~ rs
          Lens.^? listSharedReportGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListSharedReportGroups where
  type
    AWSResponse ListSharedReportGroups =
      ListSharedReportGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSharedReportGroupsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "reportGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSharedReportGroups

instance Core.NFData ListSharedReportGroups

instance Core.ToHeaders ListSharedReportGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListSharedReportGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSharedReportGroups where
  toJSON ListSharedReportGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("sortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListSharedReportGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListSharedReportGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSharedReportGroupsResponse' smart constructor.
data ListSharedReportGroupsResponse = ListSharedReportGroupsResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of ARNs for the report groups shared with the current AWS
    -- account or user.
    reportGroups :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSharedReportGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSharedReportGroupsResponse_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'reportGroups', 'listSharedReportGroupsResponse_reportGroups' - The list of ARNs for the report groups shared with the current AWS
-- account or user.
--
-- 'httpStatus', 'listSharedReportGroupsResponse_httpStatus' - The response's http status code.
newListSharedReportGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSharedReportGroupsResponse
newListSharedReportGroupsResponse pHttpStatus_ =
  ListSharedReportGroupsResponse'
    { nextToken =
        Core.Nothing,
      reportGroups = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listSharedReportGroupsResponse_nextToken :: Lens.Lens' ListSharedReportGroupsResponse (Core.Maybe Core.Text)
listSharedReportGroupsResponse_nextToken = Lens.lens (\ListSharedReportGroupsResponse' {nextToken} -> nextToken) (\s@ListSharedReportGroupsResponse' {} a -> s {nextToken = a} :: ListSharedReportGroupsResponse)

-- | The list of ARNs for the report groups shared with the current AWS
-- account or user.
listSharedReportGroupsResponse_reportGroups :: Lens.Lens' ListSharedReportGroupsResponse (Core.Maybe (Core.NonEmpty Core.Text))
listSharedReportGroupsResponse_reportGroups = Lens.lens (\ListSharedReportGroupsResponse' {reportGroups} -> reportGroups) (\s@ListSharedReportGroupsResponse' {} a -> s {reportGroups = a} :: ListSharedReportGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSharedReportGroupsResponse_httpStatus :: Lens.Lens' ListSharedReportGroupsResponse Core.Int
listSharedReportGroupsResponse_httpStatus = Lens.lens (\ListSharedReportGroupsResponse' {httpStatus} -> httpStatus) (\s@ListSharedReportGroupsResponse' {} a -> s {httpStatus = a} :: ListSharedReportGroupsResponse)

instance Core.NFData ListSharedReportGroupsResponse
