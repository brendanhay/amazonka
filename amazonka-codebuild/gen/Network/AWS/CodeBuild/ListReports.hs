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
-- Module      : Network.AWS.CodeBuild.ListReports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs for the reports in the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReports
  ( -- * Creating a Request
    ListReports (..),
    newListReports,

    -- * Request Lenses
    listReports_sortOrder,
    listReports_nextToken,
    listReports_maxResults,
    listReports_filter,

    -- * Destructuring the Response
    ListReportsResponse (..),
    newListReportsResponse,

    -- * Response Lenses
    listReportsResponse_nextToken,
    listReportsResponse_reports,
    listReportsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReports' smart constructor.
data ListReports = ListReports'
  { -- | Specifies the sort order for the list of returned reports. Valid values
    -- are:
    --
    -- -   @ASCENDING@: return reports in chronological order based on their
    --     creation date.
    --
    -- -   @DESCENDING@: return reports in the reverse chronological order
    --     based on their creation date.
    sortOrder :: Core.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of paginated reports returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @Report@ objects.
    -- The default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | A @ReportFilter@ object used to filter the returned reports.
    filter' :: Core.Maybe ReportFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listReports_sortOrder' - Specifies the sort order for the list of returned reports. Valid values
-- are:
--
-- -   @ASCENDING@: return reports in chronological order based on their
--     creation date.
--
-- -   @DESCENDING@: return reports in the reverse chronological order
--     based on their creation date.
--
-- 'nextToken', 'listReports_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'maxResults', 'listReports_maxResults' - The maximum number of paginated reports returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @Report@ objects.
-- The default value is 100.
--
-- 'filter'', 'listReports_filter' - A @ReportFilter@ object used to filter the returned reports.
newListReports ::
  ListReports
newListReports =
  ListReports'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing
    }

-- | Specifies the sort order for the list of returned reports. Valid values
-- are:
--
-- -   @ASCENDING@: return reports in chronological order based on their
--     creation date.
--
-- -   @DESCENDING@: return reports in the reverse chronological order
--     based on their creation date.
listReports_sortOrder :: Lens.Lens' ListReports (Core.Maybe SortOrderType)
listReports_sortOrder = Lens.lens (\ListReports' {sortOrder} -> sortOrder) (\s@ListReports' {} a -> s {sortOrder = a} :: ListReports)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReports_nextToken :: Lens.Lens' ListReports (Core.Maybe Core.Text)
listReports_nextToken = Lens.lens (\ListReports' {nextToken} -> nextToken) (\s@ListReports' {} a -> s {nextToken = a} :: ListReports)

-- | The maximum number of paginated reports returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @Report@ objects.
-- The default value is 100.
listReports_maxResults :: Lens.Lens' ListReports (Core.Maybe Core.Natural)
listReports_maxResults = Lens.lens (\ListReports' {maxResults} -> maxResults) (\s@ListReports' {} a -> s {maxResults = a} :: ListReports)

-- | A @ReportFilter@ object used to filter the returned reports.
listReports_filter :: Lens.Lens' ListReports (Core.Maybe ReportFilter)
listReports_filter = Lens.lens (\ListReports' {filter'} -> filter') (\s@ListReports' {} a -> s {filter' = a} :: ListReports)

instance Core.AWSPager ListReports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReportsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listReportsResponse_reports Core.. Lens._Just
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listReports_nextToken
          Lens..~ rs
          Lens.^? listReportsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListReports where
  type AWSResponse ListReports = ListReportsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "reports")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReports

instance Core.NFData ListReports

instance Core.ToHeaders ListReports where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.ListReports" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListReports where
  toJSON ListReports' {..} =
    Core.object
      ( Core.catMaybes
          [ ("sortOrder" Core..=) Core.<$> sortOrder,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath ListReports where
  toPath = Core.const "/"

instance Core.ToQuery ListReports where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListReportsResponse' smart constructor.
data ListReportsResponse = ListReportsResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of returned ARNs for the reports in the current AWS account.
    reports :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReportsResponse_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'reports', 'listReportsResponse_reports' - The list of returned ARNs for the reports in the current AWS account.
--
-- 'httpStatus', 'listReportsResponse_httpStatus' - The response's http status code.
newListReportsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReportsResponse
newListReportsResponse pHttpStatus_ =
  ListReportsResponse'
    { nextToken = Core.Nothing,
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
listReportsResponse_nextToken :: Lens.Lens' ListReportsResponse (Core.Maybe Core.Text)
listReportsResponse_nextToken = Lens.lens (\ListReportsResponse' {nextToken} -> nextToken) (\s@ListReportsResponse' {} a -> s {nextToken = a} :: ListReportsResponse)

-- | The list of returned ARNs for the reports in the current AWS account.
listReportsResponse_reports :: Lens.Lens' ListReportsResponse (Core.Maybe (Core.NonEmpty Core.Text))
listReportsResponse_reports = Lens.lens (\ListReportsResponse' {reports} -> reports) (\s@ListReportsResponse' {} a -> s {reports = a} :: ListReportsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listReportsResponse_httpStatus :: Lens.Lens' ListReportsResponse Core.Int
listReportsResponse_httpStatus = Lens.lens (\ListReportsResponse' {httpStatus} -> httpStatus) (\s@ListReportsResponse' {} a -> s {httpStatus = a} :: ListReportsResponse)

instance Core.NFData ListReportsResponse
