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
-- Module      : Network.AWS.CodeBuild.ListReportsForReportGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs for the reports that belong to a @ReportGroup@.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReportsForReportGroup
  ( -- * Creating a Request
    ListReportsForReportGroup (..),
    newListReportsForReportGroup,

    -- * Request Lenses
    listReportsForReportGroup_sortOrder,
    listReportsForReportGroup_nextToken,
    listReportsForReportGroup_maxResults,
    listReportsForReportGroup_filter,
    listReportsForReportGroup_reportGroupArn,

    -- * Destructuring the Response
    ListReportsForReportGroupResponse (..),
    newListReportsForReportGroupResponse,

    -- * Response Lenses
    listReportsForReportGroupResponse_nextToken,
    listReportsForReportGroupResponse_reports,
    listReportsForReportGroupResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReportsForReportGroup' smart constructor.
data ListReportsForReportGroup = ListReportsForReportGroup'
  { -- | Use to specify whether the results are returned in ascending or
    -- descending order.
    sortOrder :: Core.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of paginated reports in this report group returned
    -- per response. Use @nextToken@ to iterate pages in the list of returned
    -- @Report@ objects. The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | A @ReportFilter@ object used to filter the returned reports.
    filter' :: Core.Maybe ReportFilter,
    -- | The ARN of the report group for which you want to return report ARNs.
    reportGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReportsForReportGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listReportsForReportGroup_sortOrder' - Use to specify whether the results are returned in ascending or
-- descending order.
--
-- 'nextToken', 'listReportsForReportGroup_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'maxResults', 'listReportsForReportGroup_maxResults' - The maximum number of paginated reports in this report group returned
-- per response. Use @nextToken@ to iterate pages in the list of returned
-- @Report@ objects. The default value is 100.
--
-- 'filter'', 'listReportsForReportGroup_filter' - A @ReportFilter@ object used to filter the returned reports.
--
-- 'reportGroupArn', 'listReportsForReportGroup_reportGroupArn' - The ARN of the report group for which you want to return report ARNs.
newListReportsForReportGroup ::
  -- | 'reportGroupArn'
  Core.Text ->
  ListReportsForReportGroup
newListReportsForReportGroup pReportGroupArn_ =
  ListReportsForReportGroup'
    { sortOrder =
        Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing,
      reportGroupArn = pReportGroupArn_
    }

-- | Use to specify whether the results are returned in ascending or
-- descending order.
listReportsForReportGroup_sortOrder :: Lens.Lens' ListReportsForReportGroup (Core.Maybe SortOrderType)
listReportsForReportGroup_sortOrder = Lens.lens (\ListReportsForReportGroup' {sortOrder} -> sortOrder) (\s@ListReportsForReportGroup' {} a -> s {sortOrder = a} :: ListReportsForReportGroup)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportsForReportGroup_nextToken :: Lens.Lens' ListReportsForReportGroup (Core.Maybe Core.Text)
listReportsForReportGroup_nextToken = Lens.lens (\ListReportsForReportGroup' {nextToken} -> nextToken) (\s@ListReportsForReportGroup' {} a -> s {nextToken = a} :: ListReportsForReportGroup)

-- | The maximum number of paginated reports in this report group returned
-- per response. Use @nextToken@ to iterate pages in the list of returned
-- @Report@ objects. The default value is 100.
listReportsForReportGroup_maxResults :: Lens.Lens' ListReportsForReportGroup (Core.Maybe Core.Natural)
listReportsForReportGroup_maxResults = Lens.lens (\ListReportsForReportGroup' {maxResults} -> maxResults) (\s@ListReportsForReportGroup' {} a -> s {maxResults = a} :: ListReportsForReportGroup)

-- | A @ReportFilter@ object used to filter the returned reports.
listReportsForReportGroup_filter :: Lens.Lens' ListReportsForReportGroup (Core.Maybe ReportFilter)
listReportsForReportGroup_filter = Lens.lens (\ListReportsForReportGroup' {filter'} -> filter') (\s@ListReportsForReportGroup' {} a -> s {filter' = a} :: ListReportsForReportGroup)

-- | The ARN of the report group for which you want to return report ARNs.
listReportsForReportGroup_reportGroupArn :: Lens.Lens' ListReportsForReportGroup Core.Text
listReportsForReportGroup_reportGroupArn = Lens.lens (\ListReportsForReportGroup' {reportGroupArn} -> reportGroupArn) (\s@ListReportsForReportGroup' {} a -> s {reportGroupArn = a} :: ListReportsForReportGroup)

instance Core.AWSPager ListReportsForReportGroup where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReportsForReportGroupResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listReportsForReportGroupResponse_reports
              Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listReportsForReportGroup_nextToken
          Lens..~ rs
          Lens.^? listReportsForReportGroupResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListReportsForReportGroup where
  type
    AWSResponse ListReportsForReportGroup =
      ListReportsForReportGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportsForReportGroupResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "reports")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReportsForReportGroup

instance Core.NFData ListReportsForReportGroup

instance Core.ToHeaders ListReportsForReportGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListReportsForReportGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListReportsForReportGroup where
  toJSON ListReportsForReportGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter',
            Core.Just ("reportGroupArn" Core..= reportGroupArn)
          ]
      )

instance Core.ToPath ListReportsForReportGroup where
  toPath = Core.const "/"

instance Core.ToQuery ListReportsForReportGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListReportsForReportGroupResponse' smart constructor.
data ListReportsForReportGroupResponse = ListReportsForReportGroupResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of report ARNs.
    reports :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReportsForReportGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReportsForReportGroupResponse_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'reports', 'listReportsForReportGroupResponse_reports' - The list of report ARNs.
--
-- 'httpStatus', 'listReportsForReportGroupResponse_httpStatus' - The response's http status code.
newListReportsForReportGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReportsForReportGroupResponse
newListReportsForReportGroupResponse pHttpStatus_ =
  ListReportsForReportGroupResponse'
    { nextToken =
        Core.Nothing,
      reports = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportsForReportGroupResponse_nextToken :: Lens.Lens' ListReportsForReportGroupResponse (Core.Maybe Core.Text)
listReportsForReportGroupResponse_nextToken = Lens.lens (\ListReportsForReportGroupResponse' {nextToken} -> nextToken) (\s@ListReportsForReportGroupResponse' {} a -> s {nextToken = a} :: ListReportsForReportGroupResponse)

-- | The list of report ARNs.
listReportsForReportGroupResponse_reports :: Lens.Lens' ListReportsForReportGroupResponse (Core.Maybe (Core.NonEmpty Core.Text))
listReportsForReportGroupResponse_reports = Lens.lens (\ListReportsForReportGroupResponse' {reports} -> reports) (\s@ListReportsForReportGroupResponse' {} a -> s {reports = a} :: ListReportsForReportGroupResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listReportsForReportGroupResponse_httpStatus :: Lens.Lens' ListReportsForReportGroupResponse Core.Int
listReportsForReportGroupResponse_httpStatus = Lens.lens (\ListReportsForReportGroupResponse' {httpStatus} -> httpStatus) (\s@ListReportsForReportGroupResponse' {} a -> s {httpStatus = a} :: ListReportsForReportGroupResponse)

instance
  Core.NFData
    ListReportsForReportGroupResponse
