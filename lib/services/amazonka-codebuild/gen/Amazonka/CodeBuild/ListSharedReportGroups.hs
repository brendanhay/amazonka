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
-- Module      : Amazonka.CodeBuild.ListSharedReportGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of report groups that are shared with other Amazon Web
-- Services accounts or users.
--
-- This operation returns paginated results.
module Amazonka.CodeBuild.ListSharedReportGroups
  ( -- * Creating a Request
    ListSharedReportGroups (..),
    newListSharedReportGroups,

    -- * Request Lenses
    listSharedReportGroups_maxResults,
    listSharedReportGroups_nextToken,
    listSharedReportGroups_sortBy,
    listSharedReportGroups_sortOrder,

    -- * Destructuring the Response
    ListSharedReportGroupsResponse (..),
    newListSharedReportGroupsResponse,

    -- * Response Lenses
    listSharedReportGroupsResponse_nextToken,
    listSharedReportGroupsResponse_reportGroups,
    listSharedReportGroupsResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSharedReportGroups' smart constructor.
data ListSharedReportGroups = ListSharedReportGroups'
  { -- | The maximum number of paginated shared report groups per response. Use
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
    -- | The criterion to be used to list report groups shared with the current
    -- Amazon Web Services account or user. Valid values include:
    --
    -- -   @ARN@: List based on the ARN.
    --
    -- -   @MODIFIED_TIME@: List based on when information about the shared
    --     report group was last changed.
    sortBy :: Prelude.Maybe SharedResourceSortByType,
    -- | The order in which to list shared report groups. Valid values include:
    --
    -- -   @ASCENDING@: List in ascending order.
    --
    -- -   @DESCENDING@: List in descending order.
    sortOrder :: Prelude.Maybe SortOrderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSharedReportGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSharedReportGroups_maxResults' - The maximum number of paginated shared report groups per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
--
-- 'nextToken', 'listSharedReportGroups_nextToken' - During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
--
-- 'sortBy', 'listSharedReportGroups_sortBy' - The criterion to be used to list report groups shared with the current
-- Amazon Web Services account or user. Valid values include:
--
-- -   @ARN@: List based on the ARN.
--
-- -   @MODIFIED_TIME@: List based on when information about the shared
--     report group was last changed.
--
-- 'sortOrder', 'listSharedReportGroups_sortOrder' - The order in which to list shared report groups. Valid values include:
--
-- -   @ASCENDING@: List in ascending order.
--
-- -   @DESCENDING@: List in descending order.
newListSharedReportGroups ::
  ListSharedReportGroups
newListSharedReportGroups =
  ListSharedReportGroups'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The maximum number of paginated shared report groups per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
listSharedReportGroups_maxResults :: Lens.Lens' ListSharedReportGroups (Prelude.Maybe Prelude.Natural)
listSharedReportGroups_maxResults = Lens.lens (\ListSharedReportGroups' {maxResults} -> maxResults) (\s@ListSharedReportGroups' {} a -> s {maxResults = a} :: ListSharedReportGroups)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listSharedReportGroups_nextToken :: Lens.Lens' ListSharedReportGroups (Prelude.Maybe Prelude.Text)
listSharedReportGroups_nextToken = Lens.lens (\ListSharedReportGroups' {nextToken} -> nextToken) (\s@ListSharedReportGroups' {} a -> s {nextToken = a} :: ListSharedReportGroups)

-- | The criterion to be used to list report groups shared with the current
-- Amazon Web Services account or user. Valid values include:
--
-- -   @ARN@: List based on the ARN.
--
-- -   @MODIFIED_TIME@: List based on when information about the shared
--     report group was last changed.
listSharedReportGroups_sortBy :: Lens.Lens' ListSharedReportGroups (Prelude.Maybe SharedResourceSortByType)
listSharedReportGroups_sortBy = Lens.lens (\ListSharedReportGroups' {sortBy} -> sortBy) (\s@ListSharedReportGroups' {} a -> s {sortBy = a} :: ListSharedReportGroups)

-- | The order in which to list shared report groups. Valid values include:
--
-- -   @ASCENDING@: List in ascending order.
--
-- -   @DESCENDING@: List in descending order.
listSharedReportGroups_sortOrder :: Lens.Lens' ListSharedReportGroups (Prelude.Maybe SortOrderType)
listSharedReportGroups_sortOrder = Lens.lens (\ListSharedReportGroups' {sortOrder} -> sortOrder) (\s@ListSharedReportGroups' {} a -> s {sortOrder = a} :: ListSharedReportGroups)

instance Core.AWSPager ListSharedReportGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSharedReportGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSharedReportGroupsResponse_reportGroups
            Prelude.. Lens._Just
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSharedReportGroups_nextToken
          Lens..~ rs
          Lens.^? listSharedReportGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSharedReportGroups where
  type
    AWSResponse ListSharedReportGroups =
      ListSharedReportGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSharedReportGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "reportGroups")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSharedReportGroups where
  hashWithSalt _salt ListSharedReportGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListSharedReportGroups where
  rnf ListSharedReportGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListSharedReportGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.ListSharedReportGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSharedReportGroups where
  toJSON ListSharedReportGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListSharedReportGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSharedReportGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSharedReportGroupsResponse' smart constructor.
data ListSharedReportGroupsResponse = ListSharedReportGroupsResponse'
  { -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of ARNs for the report groups shared with the current Amazon
    -- Web Services account or user.
    reportGroups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'reportGroups', 'listSharedReportGroupsResponse_reportGroups' - The list of ARNs for the report groups shared with the current Amazon
-- Web Services account or user.
--
-- 'httpStatus', 'listSharedReportGroupsResponse_httpStatus' - The response's http status code.
newListSharedReportGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSharedReportGroupsResponse
newListSharedReportGroupsResponse pHttpStatus_ =
  ListSharedReportGroupsResponse'
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
listSharedReportGroupsResponse_nextToken :: Lens.Lens' ListSharedReportGroupsResponse (Prelude.Maybe Prelude.Text)
listSharedReportGroupsResponse_nextToken = Lens.lens (\ListSharedReportGroupsResponse' {nextToken} -> nextToken) (\s@ListSharedReportGroupsResponse' {} a -> s {nextToken = a} :: ListSharedReportGroupsResponse)

-- | The list of ARNs for the report groups shared with the current Amazon
-- Web Services account or user.
listSharedReportGroupsResponse_reportGroups :: Lens.Lens' ListSharedReportGroupsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listSharedReportGroupsResponse_reportGroups = Lens.lens (\ListSharedReportGroupsResponse' {reportGroups} -> reportGroups) (\s@ListSharedReportGroupsResponse' {} a -> s {reportGroups = a} :: ListSharedReportGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSharedReportGroupsResponse_httpStatus :: Lens.Lens' ListSharedReportGroupsResponse Prelude.Int
listSharedReportGroupsResponse_httpStatus = Lens.lens (\ListSharedReportGroupsResponse' {httpStatus} -> httpStatus) (\s@ListSharedReportGroupsResponse' {} a -> s {httpStatus = a} :: ListSharedReportGroupsResponse)

instance
  Prelude.NFData
    ListSharedReportGroupsResponse
  where
  rnf ListSharedReportGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reportGroups
      `Prelude.seq` Prelude.rnf httpStatus
