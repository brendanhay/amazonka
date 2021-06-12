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
-- Module      : Network.AWS.CodeBuild.ListReportGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list ARNs for the report groups in the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReportGroups
  ( -- * Creating a Request
    ListReportGroups (..),
    newListReportGroups,

    -- * Request Lenses
    listReportGroups_sortOrder,
    listReportGroups_nextToken,
    listReportGroups_maxResults,
    listReportGroups_sortBy,

    -- * Destructuring the Response
    ListReportGroupsResponse (..),
    newListReportGroupsResponse,

    -- * Response Lenses
    listReportGroupsResponse_nextToken,
    listReportGroupsResponse_reportGroups,
    listReportGroupsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReportGroups' smart constructor.
data ListReportGroups = ListReportGroups'
  { -- | Used to specify the order to sort the list of returned report groups.
    -- Valid values are @ASCENDING@ and @DESCENDING@.
    sortOrder :: Core.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of paginated report groups returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @ReportGroup@
    -- objects. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The criterion to be used to list build report groups. Valid values
    -- include:
    --
    -- -   @CREATED_TIME@: List based on when each report group was created.
    --
    -- -   @LAST_MODIFIED_TIME@: List based on when each report group was last
    --     changed.
    --
    -- -   @NAME@: List based on each report group\'s name.
    sortBy :: Core.Maybe ReportGroupSortByType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReportGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listReportGroups_sortOrder' - Used to specify the order to sort the list of returned report groups.
-- Valid values are @ASCENDING@ and @DESCENDING@.
--
-- 'nextToken', 'listReportGroups_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'maxResults', 'listReportGroups_maxResults' - The maximum number of paginated report groups returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
--
-- 'sortBy', 'listReportGroups_sortBy' - The criterion to be used to list build report groups. Valid values
-- include:
--
-- -   @CREATED_TIME@: List based on when each report group was created.
--
-- -   @LAST_MODIFIED_TIME@: List based on when each report group was last
--     changed.
--
-- -   @NAME@: List based on each report group\'s name.
newListReportGroups ::
  ListReportGroups
newListReportGroups =
  ListReportGroups'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | Used to specify the order to sort the list of returned report groups.
-- Valid values are @ASCENDING@ and @DESCENDING@.
listReportGroups_sortOrder :: Lens.Lens' ListReportGroups (Core.Maybe SortOrderType)
listReportGroups_sortOrder = Lens.lens (\ListReportGroups' {sortOrder} -> sortOrder) (\s@ListReportGroups' {} a -> s {sortOrder = a} :: ListReportGroups)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportGroups_nextToken :: Lens.Lens' ListReportGroups (Core.Maybe Core.Text)
listReportGroups_nextToken = Lens.lens (\ListReportGroups' {nextToken} -> nextToken) (\s@ListReportGroups' {} a -> s {nextToken = a} :: ListReportGroups)

-- | The maximum number of paginated report groups returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
listReportGroups_maxResults :: Lens.Lens' ListReportGroups (Core.Maybe Core.Natural)
listReportGroups_maxResults = Lens.lens (\ListReportGroups' {maxResults} -> maxResults) (\s@ListReportGroups' {} a -> s {maxResults = a} :: ListReportGroups)

-- | The criterion to be used to list build report groups. Valid values
-- include:
--
-- -   @CREATED_TIME@: List based on when each report group was created.
--
-- -   @LAST_MODIFIED_TIME@: List based on when each report group was last
--     changed.
--
-- -   @NAME@: List based on each report group\'s name.
listReportGroups_sortBy :: Lens.Lens' ListReportGroups (Core.Maybe ReportGroupSortByType)
listReportGroups_sortBy = Lens.lens (\ListReportGroups' {sortBy} -> sortBy) (\s@ListReportGroups' {} a -> s {sortBy = a} :: ListReportGroups)

instance Core.AWSPager ListReportGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReportGroupsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listReportGroupsResponse_reportGroups
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listReportGroups_nextToken
          Lens..~ rs
          Lens.^? listReportGroupsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListReportGroups where
  type
    AWSResponse ListReportGroups =
      ListReportGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportGroupsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "reportGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReportGroups

instance Core.NFData ListReportGroups

instance Core.ToHeaders ListReportGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListReportGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListReportGroups where
  toJSON ListReportGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("sortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListReportGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListReportGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListReportGroupsResponse' smart constructor.
data ListReportGroupsResponse = ListReportGroupsResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of ARNs for the report groups in the current AWS account.
    reportGroups :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReportGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReportGroupsResponse_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'reportGroups', 'listReportGroupsResponse_reportGroups' - The list of ARNs for the report groups in the current AWS account.
--
-- 'httpStatus', 'listReportGroupsResponse_httpStatus' - The response's http status code.
newListReportGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReportGroupsResponse
newListReportGroupsResponse pHttpStatus_ =
  ListReportGroupsResponse'
    { nextToken = Core.Nothing,
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
listReportGroupsResponse_nextToken :: Lens.Lens' ListReportGroupsResponse (Core.Maybe Core.Text)
listReportGroupsResponse_nextToken = Lens.lens (\ListReportGroupsResponse' {nextToken} -> nextToken) (\s@ListReportGroupsResponse' {} a -> s {nextToken = a} :: ListReportGroupsResponse)

-- | The list of ARNs for the report groups in the current AWS account.
listReportGroupsResponse_reportGroups :: Lens.Lens' ListReportGroupsResponse (Core.Maybe (Core.NonEmpty Core.Text))
listReportGroupsResponse_reportGroups = Lens.lens (\ListReportGroupsResponse' {reportGroups} -> reportGroups) (\s@ListReportGroupsResponse' {} a -> s {reportGroups = a} :: ListReportGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listReportGroupsResponse_httpStatus :: Lens.Lens' ListReportGroupsResponse Core.Int
listReportGroupsResponse_httpStatus = Lens.lens (\ListReportGroupsResponse' {httpStatus} -> httpStatus) (\s@ListReportGroupsResponse' {} a -> s {httpStatus = a} :: ListReportGroupsResponse)

instance Core.NFData ListReportGroupsResponse
