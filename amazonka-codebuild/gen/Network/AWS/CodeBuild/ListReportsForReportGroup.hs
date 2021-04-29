{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReportsForReportGroup' smart constructor.
data ListReportsForReportGroup = ListReportsForReportGroup'
  { -- | Use to specify whether the results are returned in ascending or
    -- descending order.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of paginated reports in this report group returned
    -- per response. Use @nextToken@ to iterate pages in the list of returned
    -- @Report@ objects. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A @ReportFilter@ object used to filter the returned reports.
    filter' :: Prelude.Maybe ReportFilter,
    -- | The ARN of the report group for which you want to return report ARNs.
    reportGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListReportsForReportGroup
newListReportsForReportGroup pReportGroupArn_ =
  ListReportsForReportGroup'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing,
      reportGroupArn = pReportGroupArn_
    }

-- | Use to specify whether the results are returned in ascending or
-- descending order.
listReportsForReportGroup_sortOrder :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe SortOrderType)
listReportsForReportGroup_sortOrder = Lens.lens (\ListReportsForReportGroup' {sortOrder} -> sortOrder) (\s@ListReportsForReportGroup' {} a -> s {sortOrder = a} :: ListReportsForReportGroup)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportsForReportGroup_nextToken :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe Prelude.Text)
listReportsForReportGroup_nextToken = Lens.lens (\ListReportsForReportGroup' {nextToken} -> nextToken) (\s@ListReportsForReportGroup' {} a -> s {nextToken = a} :: ListReportsForReportGroup)

-- | The maximum number of paginated reports in this report group returned
-- per response. Use @nextToken@ to iterate pages in the list of returned
-- @Report@ objects. The default value is 100.
listReportsForReportGroup_maxResults :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe Prelude.Natural)
listReportsForReportGroup_maxResults = Lens.lens (\ListReportsForReportGroup' {maxResults} -> maxResults) (\s@ListReportsForReportGroup' {} a -> s {maxResults = a} :: ListReportsForReportGroup)

-- | A @ReportFilter@ object used to filter the returned reports.
listReportsForReportGroup_filter :: Lens.Lens' ListReportsForReportGroup (Prelude.Maybe ReportFilter)
listReportsForReportGroup_filter = Lens.lens (\ListReportsForReportGroup' {filter'} -> filter') (\s@ListReportsForReportGroup' {} a -> s {filter' = a} :: ListReportsForReportGroup)

-- | The ARN of the report group for which you want to return report ARNs.
listReportsForReportGroup_reportGroupArn :: Lens.Lens' ListReportsForReportGroup Prelude.Text
listReportsForReportGroup_reportGroupArn = Lens.lens (\ListReportsForReportGroup' {reportGroupArn} -> reportGroupArn) (\s@ListReportsForReportGroup' {} a -> s {reportGroupArn = a} :: ListReportsForReportGroup)

instance Pager.AWSPager ListReportsForReportGroup where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listReportsForReportGroupResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listReportsForReportGroupResponse_reports
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listReportsForReportGroup_nextToken
          Lens..~ rs
          Lens.^? listReportsForReportGroupResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListReportsForReportGroup where
  type
    Rs ListReportsForReportGroup =
      ListReportsForReportGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportsForReportGroupResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "reports")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportsForReportGroup

instance Prelude.NFData ListReportsForReportGroup

instance Prelude.ToHeaders ListReportsForReportGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.ListReportsForReportGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListReportsForReportGroup where
  toJSON ListReportsForReportGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("sortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("filter" Prelude..=) Prelude.<$> filter',
            Prelude.Just
              ("reportGroupArn" Prelude..= reportGroupArn)
          ]
      )

instance Prelude.ToPath ListReportsForReportGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListReportsForReportGroup where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listReportsForReportGroupResponse_reports = Lens.lens (\ListReportsForReportGroupResponse' {reports} -> reports) (\s@ListReportsForReportGroupResponse' {} a -> s {reports = a} :: ListReportsForReportGroupResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listReportsForReportGroupResponse_httpStatus :: Lens.Lens' ListReportsForReportGroupResponse Prelude.Int
listReportsForReportGroupResponse_httpStatus = Lens.lens (\ListReportsForReportGroupResponse' {httpStatus} -> httpStatus) (\s@ListReportsForReportGroupResponse' {} a -> s {httpStatus = a} :: ListReportsForReportGroupResponse)

instance
  Prelude.NFData
    ListReportsForReportGroupResponse
