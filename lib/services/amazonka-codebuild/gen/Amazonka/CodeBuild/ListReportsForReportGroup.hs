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
-- Module      : Amazonka.CodeBuild.ListReportsForReportGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs for the reports that belong to a @ReportGroup@.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.ListReportsForReportGroup
  ( -- * Creating a Request
    ListReportsForReportGroup (..),
    newListReportsForReportGroup,

    -- * Request Lenses
    listReportsForReportGroup_filter,
    listReportsForReportGroup_maxResults,
    listReportsForReportGroup_nextToken,
    listReportsForReportGroup_sortOrder,
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

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReportsForReportGroup' smart constructor.
data ListReportsForReportGroup = ListReportsForReportGroup'
  { -- | A @ReportFilter@ object used to filter the returned reports.
    filter' :: Prelude.Maybe ReportFilter,
    -- | The maximum number of paginated reports in this report group returned
    -- per response. Use @nextToken@ to iterate pages in the list of returned
    -- @Report@ objects. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use to specify whether the results are returned in ascending or
    -- descending order.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | The ARN of the report group for which you want to return report ARNs.
    reportGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportsForReportGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReportsForReportGroup_filter' - A @ReportFilter@ object used to filter the returned reports.
--
-- 'maxResults', 'listReportsForReportGroup_maxResults' - The maximum number of paginated reports in this report group returned
-- per response. Use @nextToken@ to iterate pages in the list of returned
-- @Report@ objects. The default value is 100.
--
-- 'nextToken', 'listReportsForReportGroup_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'sortOrder', 'listReportsForReportGroup_sortOrder' - Use to specify whether the results are returned in ascending or
-- descending order.
--
-- 'reportGroupArn', 'listReportsForReportGroup_reportGroupArn' - The ARN of the report group for which you want to return report ARNs.
newListReportsForReportGroup ::
  -- | 'reportGroupArn'
  Prelude.Text ->
  ListReportsForReportGroup
newListReportsForReportGroup pReportGroupArn_ =
  ListReportsForReportGroup'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      reportGroupArn = pReportGroupArn_
    }

-- | A @ReportFilter@ object used to filter the returned reports.
listReportsForReportGroup_filter :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe ReportFilter)
listReportsForReportGroup_filter = Lens.lens (\ListReportsForReportGroup' {filter'} -> filter') (\s@ListReportsForReportGroup' {} a -> s {filter' = a} :: ListReportsForReportGroup)

-- | The maximum number of paginated reports in this report group returned
-- per response. Use @nextToken@ to iterate pages in the list of returned
-- @Report@ objects. The default value is 100.
listReportsForReportGroup_maxResults :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe Prelude.Natural)
listReportsForReportGroup_maxResults = Lens.lens (\ListReportsForReportGroup' {maxResults} -> maxResults) (\s@ListReportsForReportGroup' {} a -> s {maxResults = a} :: ListReportsForReportGroup)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportsForReportGroup_nextToken :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe Prelude.Text)
listReportsForReportGroup_nextToken = Lens.lens (\ListReportsForReportGroup' {nextToken} -> nextToken) (\s@ListReportsForReportGroup' {} a -> s {nextToken = a} :: ListReportsForReportGroup)

-- | Use to specify whether the results are returned in ascending or
-- descending order.
listReportsForReportGroup_sortOrder :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe SortOrderType)
listReportsForReportGroup_sortOrder = Lens.lens (\ListReportsForReportGroup' {sortOrder} -> sortOrder) (\s@ListReportsForReportGroup' {} a -> s {sortOrder = a} :: ListReportsForReportGroup)

-- | The ARN of the report group for which you want to return report ARNs.
listReportsForReportGroup_reportGroupArn :: Lens.Lens' ListReportsForReportGroup Prelude.Text
listReportsForReportGroup_reportGroupArn = Lens.lens (\ListReportsForReportGroup' {reportGroupArn} -> reportGroupArn) (\s@ListReportsForReportGroup' {} a -> s {reportGroupArn = a} :: ListReportsForReportGroup)

instance Core.AWSPager ListReportsForReportGroup where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReportsForReportGroupResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReportsForReportGroupResponse_reports
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listReportsForReportGroup_nextToken
          Lens..~ rs
          Lens.^? listReportsForReportGroupResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListReportsForReportGroup where
  type
    AWSResponse ListReportsForReportGroup =
      ListReportsForReportGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportsForReportGroupResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "reports")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportsForReportGroup where
  hashWithSalt _salt ListReportsForReportGroup' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` reportGroupArn

instance Prelude.NFData ListReportsForReportGroup where
  rnf ListReportsForReportGroup' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf reportGroupArn

instance Data.ToHeaders ListReportsForReportGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.ListReportsForReportGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReportsForReportGroup where
  toJSON ListReportsForReportGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortOrder" Data..=) Prelude.<$> sortOrder,
            Prelude.Just
              ("reportGroupArn" Data..= reportGroupArn)
          ]
      )

instance Data.ToPath ListReportsForReportGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ListReportsForReportGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReportsForReportGroupResponse' smart constructor.
data ListReportsForReportGroupResponse = ListReportsForReportGroupResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of report ARNs.
    reports :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListReportsForReportGroupResponse
newListReportsForReportGroupResponse pHttpStatus_ =
  ListReportsForReportGroupResponse'
    { nextToken =
        Prelude.Nothing,
      reports = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportsForReportGroupResponse_nextToken :: Lens.Lens' ListReportsForReportGroupResponse (Prelude.Maybe Prelude.Text)
listReportsForReportGroupResponse_nextToken = Lens.lens (\ListReportsForReportGroupResponse' {nextToken} -> nextToken) (\s@ListReportsForReportGroupResponse' {} a -> s {nextToken = a} :: ListReportsForReportGroupResponse)

-- | The list of report ARNs.
listReportsForReportGroupResponse_reports :: Lens.Lens' ListReportsForReportGroupResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listReportsForReportGroupResponse_reports = Lens.lens (\ListReportsForReportGroupResponse' {reports} -> reports) (\s@ListReportsForReportGroupResponse' {} a -> s {reports = a} :: ListReportsForReportGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReportsForReportGroupResponse_httpStatus :: Lens.Lens' ListReportsForReportGroupResponse Prelude.Int
listReportsForReportGroupResponse_httpStatus = Lens.lens (\ListReportsForReportGroupResponse' {httpStatus} -> httpStatus) (\s@ListReportsForReportGroupResponse' {} a -> s {httpStatus = a} :: ListReportsForReportGroupResponse)

instance
  Prelude.NFData
    ListReportsForReportGroupResponse
  where
  rnf ListReportsForReportGroupResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reports
      `Prelude.seq` Prelude.rnf httpStatus
