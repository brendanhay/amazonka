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
-- Module      : Amazonka.CodeBuild.ListReportGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list ARNs for the report groups in the current Amazon Web
-- Services account.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.ListReportGroups
  ( -- * Creating a Request
    ListReportGroups (..),
    newListReportGroups,

    -- * Request Lenses
    listReportGroups_maxResults,
    listReportGroups_nextToken,
    listReportGroups_sortBy,
    listReportGroups_sortOrder,

    -- * Destructuring the Response
    ListReportGroupsResponse (..),
    newListReportGroupsResponse,

    -- * Response Lenses
    listReportGroupsResponse_nextToken,
    listReportGroupsResponse_reportGroups,
    listReportGroupsResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReportGroups' smart constructor.
data ListReportGroups = ListReportGroups'
  { -- | The maximum number of paginated report groups returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @ReportGroup@
    -- objects. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The criterion to be used to list build report groups. Valid values
    -- include:
    --
    -- -   @CREATED_TIME@: List based on when each report group was created.
    --
    -- -   @LAST_MODIFIED_TIME@: List based on when each report group was last
    --     changed.
    --
    -- -   @NAME@: List based on each report group\'s name.
    sortBy :: Prelude.Maybe ReportGroupSortByType,
    -- | Used to specify the order to sort the list of returned report groups.
    -- Valid values are @ASCENDING@ and @DESCENDING@.
    sortOrder :: Prelude.Maybe SortOrderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listReportGroups_maxResults' - The maximum number of paginated report groups returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
--
-- 'nextToken', 'listReportGroups_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
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
--
-- 'sortOrder', 'listReportGroups_sortOrder' - Used to specify the order to sort the list of returned report groups.
-- Valid values are @ASCENDING@ and @DESCENDING@.
newListReportGroups ::
  ListReportGroups
newListReportGroups =
  ListReportGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The maximum number of paginated report groups returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
listReportGroups_maxResults :: Lens.Lens' ListReportGroups (Prelude.Maybe Prelude.Natural)
listReportGroups_maxResults = Lens.lens (\ListReportGroups' {maxResults} -> maxResults) (\s@ListReportGroups' {} a -> s {maxResults = a} :: ListReportGroups)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportGroups_nextToken :: Lens.Lens' ListReportGroups (Prelude.Maybe Prelude.Text)
listReportGroups_nextToken = Lens.lens (\ListReportGroups' {nextToken} -> nextToken) (\s@ListReportGroups' {} a -> s {nextToken = a} :: ListReportGroups)

-- | The criterion to be used to list build report groups. Valid values
-- include:
--
-- -   @CREATED_TIME@: List based on when each report group was created.
--
-- -   @LAST_MODIFIED_TIME@: List based on when each report group was last
--     changed.
--
-- -   @NAME@: List based on each report group\'s name.
listReportGroups_sortBy :: Lens.Lens' ListReportGroups (Prelude.Maybe ReportGroupSortByType)
listReportGroups_sortBy = Lens.lens (\ListReportGroups' {sortBy} -> sortBy) (\s@ListReportGroups' {} a -> s {sortBy = a} :: ListReportGroups)

-- | Used to specify the order to sort the list of returned report groups.
-- Valid values are @ASCENDING@ and @DESCENDING@.
listReportGroups_sortOrder :: Lens.Lens' ListReportGroups (Prelude.Maybe SortOrderType)
listReportGroups_sortOrder = Lens.lens (\ListReportGroups' {sortOrder} -> sortOrder) (\s@ListReportGroups' {} a -> s {sortOrder = a} :: ListReportGroups)

instance Core.AWSPager ListReportGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReportGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReportGroupsResponse_reportGroups
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listReportGroups_nextToken
              Lens..~ rs
              Lens.^? listReportGroupsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListReportGroups where
  type
    AWSResponse ListReportGroups =
      ListReportGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "reportGroups")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportGroups where
  hashWithSalt _salt ListReportGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListReportGroups where
  rnf ListReportGroups' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf sortBy `Prelude.seq`
          Prelude.rnf sortOrder

instance Data.ToHeaders ListReportGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.ListReportGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReportGroups where
  toJSON ListReportGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListReportGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListReportGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReportGroupsResponse' smart constructor.
data ListReportGroupsResponse = ListReportGroupsResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of ARNs for the report groups in the current Amazon Web
    -- Services account.
    reportGroups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'reportGroups', 'listReportGroupsResponse_reportGroups' - The list of ARNs for the report groups in the current Amazon Web
-- Services account.
--
-- 'httpStatus', 'listReportGroupsResponse_httpStatus' - The response's http status code.
newListReportGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReportGroupsResponse
newListReportGroupsResponse pHttpStatus_ =
  ListReportGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      reportGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportGroupsResponse_nextToken :: Lens.Lens' ListReportGroupsResponse (Prelude.Maybe Prelude.Text)
listReportGroupsResponse_nextToken = Lens.lens (\ListReportGroupsResponse' {nextToken} -> nextToken) (\s@ListReportGroupsResponse' {} a -> s {nextToken = a} :: ListReportGroupsResponse)

-- | The list of ARNs for the report groups in the current Amazon Web
-- Services account.
listReportGroupsResponse_reportGroups :: Lens.Lens' ListReportGroupsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listReportGroupsResponse_reportGroups = Lens.lens (\ListReportGroupsResponse' {reportGroups} -> reportGroups) (\s@ListReportGroupsResponse' {} a -> s {reportGroups = a} :: ListReportGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReportGroupsResponse_httpStatus :: Lens.Lens' ListReportGroupsResponse Prelude.Int
listReportGroupsResponse_httpStatus = Lens.lens (\ListReportGroupsResponse' {httpStatus} -> httpStatus) (\s@ListReportGroupsResponse' {} a -> s {httpStatus = a} :: ListReportGroupsResponse)

instance Prelude.NFData ListReportGroupsResponse where
  rnf ListReportGroupsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf reportGroups `Prelude.seq`
        Prelude.rnf httpStatus
