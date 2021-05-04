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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of paginated reports returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @Report@ objects.
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A @ReportFilter@ object used to filter the returned reports.
    filter' :: Prelude.Maybe ReportFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Specifies the sort order for the list of returned reports. Valid values
-- are:
--
-- -   @ASCENDING@: return reports in chronological order based on their
--     creation date.
--
-- -   @DESCENDING@: return reports in the reverse chronological order
--     based on their creation date.
listReports_sortOrder :: Lens.Lens' ListReports (Prelude.Maybe SortOrderType)
listReports_sortOrder = Lens.lens (\ListReports' {sortOrder} -> sortOrder) (\s@ListReports' {} a -> s {sortOrder = a} :: ListReports)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReports_nextToken :: Lens.Lens' ListReports (Prelude.Maybe Prelude.Text)
listReports_nextToken = Lens.lens (\ListReports' {nextToken} -> nextToken) (\s@ListReports' {} a -> s {nextToken = a} :: ListReports)

-- | The maximum number of paginated reports returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @Report@ objects.
-- The default value is 100.
listReports_maxResults :: Lens.Lens' ListReports (Prelude.Maybe Prelude.Natural)
listReports_maxResults = Lens.lens (\ListReports' {maxResults} -> maxResults) (\s@ListReports' {} a -> s {maxResults = a} :: ListReports)

-- | A @ReportFilter@ object used to filter the returned reports.
listReports_filter :: Lens.Lens' ListReports (Prelude.Maybe ReportFilter)
listReports_filter = Lens.lens (\ListReports' {filter'} -> filter') (\s@ListReports' {} a -> s {filter' = a} :: ListReports)

instance Pager.AWSPager ListReports where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listReportsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listReportsResponse_reports Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listReports_nextToken
          Lens..~ rs
          Lens.^? listReportsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListReports where
  type Rs ListReports = ListReportsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "reports")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReports

instance Prelude.NFData ListReports

instance Prelude.ToHeaders ListReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.ListReports" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListReports where
  toJSON ListReports' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("sortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance Prelude.ToPath ListReports where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListReports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReportsResponse' smart constructor.
data ListReportsResponse = ListReportsResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of returned ARNs for the reports in the current AWS account.
    reports :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListReportsResponse
newListReportsResponse pHttpStatus_ =
  ListReportsResponse'
    { nextToken = Prelude.Nothing,
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
listReportsResponse_nextToken :: Lens.Lens' ListReportsResponse (Prelude.Maybe Prelude.Text)
listReportsResponse_nextToken = Lens.lens (\ListReportsResponse' {nextToken} -> nextToken) (\s@ListReportsResponse' {} a -> s {nextToken = a} :: ListReportsResponse)

-- | The list of returned ARNs for the reports in the current AWS account.
listReportsResponse_reports :: Lens.Lens' ListReportsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listReportsResponse_reports = Lens.lens (\ListReportsResponse' {reports} -> reports) (\s@ListReportsResponse' {} a -> s {reports = a} :: ListReportsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listReportsResponse_httpStatus :: Lens.Lens' ListReportsResponse Prelude.Int
listReportsResponse_httpStatus = Lens.lens (\ListReportsResponse' {httpStatus} -> httpStatus) (\s@ListReportsResponse' {} a -> s {httpStatus = a} :: ListReportsResponse)

instance Prelude.NFData ListReportsResponse
