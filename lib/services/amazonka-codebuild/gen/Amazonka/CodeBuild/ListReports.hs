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
-- Module      : Amazonka.CodeBuild.ListReports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs for the reports in the current Amazon Web
-- Services account.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.ListReports
  ( -- * Creating a Request
    ListReports (..),
    newListReports,

    -- * Request Lenses
    listReports_filter,
    listReports_maxResults,
    listReports_nextToken,
    listReports_sortOrder,

    -- * Destructuring the Response
    ListReportsResponse (..),
    newListReportsResponse,

    -- * Response Lenses
    listReportsResponse_nextToken,
    listReportsResponse_reports,
    listReportsResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReports' smart constructor.
data ListReports = ListReports'
  { -- | A @ReportFilter@ object used to filter the returned reports.
    filter' :: Prelude.Maybe ReportFilter,
    -- | The maximum number of paginated reports returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @Report@ objects.
    -- The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the sort order for the list of returned reports. Valid values
    -- are:
    --
    -- -   @ASCENDING@: return reports in chronological order based on their
    --     creation date.
    --
    -- -   @DESCENDING@: return reports in the reverse chronological order
    --     based on their creation date.
    sortOrder :: Prelude.Maybe SortOrderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReports_filter' - A @ReportFilter@ object used to filter the returned reports.
--
-- 'maxResults', 'listReports_maxResults' - The maximum number of paginated reports returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @Report@ objects.
-- The default value is 100.
--
-- 'nextToken', 'listReports_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'sortOrder', 'listReports_sortOrder' - Specifies the sort order for the list of returned reports. Valid values
-- are:
--
-- -   @ASCENDING@: return reports in chronological order based on their
--     creation date.
--
-- -   @DESCENDING@: return reports in the reverse chronological order
--     based on their creation date.
newListReports ::
  ListReports
newListReports =
  ListReports'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A @ReportFilter@ object used to filter the returned reports.
listReports_filter :: Lens.Lens' ListReports (Prelude.Maybe ReportFilter)
listReports_filter = Lens.lens (\ListReports' {filter'} -> filter') (\s@ListReports' {} a -> s {filter' = a} :: ListReports)

-- | The maximum number of paginated reports returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @Report@ objects.
-- The default value is 100.
listReports_maxResults :: Lens.Lens' ListReports (Prelude.Maybe Prelude.Natural)
listReports_maxResults = Lens.lens (\ListReports' {maxResults} -> maxResults) (\s@ListReports' {} a -> s {maxResults = a} :: ListReports)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReports_nextToken :: Lens.Lens' ListReports (Prelude.Maybe Prelude.Text)
listReports_nextToken = Lens.lens (\ListReports' {nextToken} -> nextToken) (\s@ListReports' {} a -> s {nextToken = a} :: ListReports)

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

instance Core.AWSPager ListReports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReportsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReportsResponse_reports Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listReports_nextToken
          Lens..~ rs
          Lens.^? listReportsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListReports where
  type AWSResponse ListReports = ListReportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "reports")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReports where
  hashWithSalt _salt ListReports' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListReports where
  rnf ListReports' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.ListReports" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReports where
  toJSON ListReports' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListReports where
  toPath = Prelude.const "/"

instance Data.ToQuery ListReports where
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
    -- | The list of returned ARNs for the reports in the current Amazon Web
    -- Services account.
    reports :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'reports', 'listReportsResponse_reports' - The list of returned ARNs for the reports in the current Amazon Web
-- Services account.
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

-- | The list of returned ARNs for the reports in the current Amazon Web
-- Services account.
listReportsResponse_reports :: Lens.Lens' ListReportsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listReportsResponse_reports = Lens.lens (\ListReportsResponse' {reports} -> reports) (\s@ListReportsResponse' {} a -> s {reports = a} :: ListReportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReportsResponse_httpStatus :: Lens.Lens' ListReportsResponse Prelude.Int
listReportsResponse_httpStatus = Lens.lens (\ListReportsResponse' {httpStatus} -> httpStatus) (\s@ListReportsResponse' {} a -> s {httpStatus = a} :: ListReportsResponse)

instance Prelude.NFData ListReportsResponse where
  rnf ListReportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reports
      `Prelude.seq` Prelude.rnf httpStatus
