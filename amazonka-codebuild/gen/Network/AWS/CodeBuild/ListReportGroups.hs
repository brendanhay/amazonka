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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReportGroups' smart constructor.
data ListReportGroups = ListReportGroups'
  { -- | Used to specify the order to sort the list of returned report groups.
    -- Valid values are @ASCENDING@ and @DESCENDING@.
    sortOrder :: Prelude.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned
    -- is the value specified in @maxResults@. If there more items in the list,
    -- then a unique string called a /nextToken/ is returned. To get the next
    -- batch of items in the list, call this operation again, adding the next
    -- token to the call. To get all of the items in the list, keep calling
    -- this operation with each subsequent next token that is returned, until
    -- no more next tokens are returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of paginated report groups returned per response. Use
    -- @nextToken@ to iterate pages in the list of returned @ReportGroup@
    -- objects. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The criterion to be used to list build report groups. Valid values
    -- include:
    --
    -- -   @CREATED_TIME@: List based on when each report group was created.
    --
    -- -   @LAST_MODIFIED_TIME@: List based on when each report group was last
    --     changed.
    --
    -- -   @NAME@: List based on each report group\'s name.
    sortBy :: Prelude.Maybe ReportGroupSortByType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | Used to specify the order to sort the list of returned report groups.
-- Valid values are @ASCENDING@ and @DESCENDING@.
listReportGroups_sortOrder :: Lens.Lens' ListReportGroups (Prelude.Maybe SortOrderType)
listReportGroups_sortOrder = Lens.lens (\ListReportGroups' {sortOrder} -> sortOrder) (\s@ListReportGroups' {} a -> s {sortOrder = a} :: ListReportGroups)

-- | During a previous call, the maximum number of items that can be returned
-- is the value specified in @maxResults@. If there more items in the list,
-- then a unique string called a /nextToken/ is returned. To get the next
-- batch of items in the list, call this operation again, adding the next
-- token to the call. To get all of the items in the list, keep calling
-- this operation with each subsequent next token that is returned, until
-- no more next tokens are returned.
listReportGroups_nextToken :: Lens.Lens' ListReportGroups (Prelude.Maybe Prelude.Text)
listReportGroups_nextToken = Lens.lens (\ListReportGroups' {nextToken} -> nextToken) (\s@ListReportGroups' {} a -> s {nextToken = a} :: ListReportGroups)

-- | The maximum number of paginated report groups returned per response. Use
-- @nextToken@ to iterate pages in the list of returned @ReportGroup@
-- objects. The default value is 100.
listReportGroups_maxResults :: Lens.Lens' ListReportGroups (Prelude.Maybe Prelude.Natural)
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
listReportGroups_sortBy :: Lens.Lens' ListReportGroups (Prelude.Maybe ReportGroupSortByType)
listReportGroups_sortBy = Lens.lens (\ListReportGroups' {sortBy} -> sortBy) (\s@ListReportGroups' {} a -> s {sortBy = a} :: ListReportGroups)

instance Pager.AWSPager ListReportGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listReportGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listReportGroupsResponse_reportGroups
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listReportGroups_nextToken
          Lens..~ rs
          Lens.^? listReportGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListReportGroups where
  type Rs ListReportGroups = ListReportGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportGroupsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "reportGroups")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportGroups

instance Prelude.NFData ListReportGroups

instance Prelude.ToHeaders ListReportGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.ListReportGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListReportGroups where
  toJSON ListReportGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("sortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("sortBy" Prelude..=) Prelude.<$> sortBy
          ]
      )

instance Prelude.ToPath ListReportGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListReportGroups where
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
    -- | The list of ARNs for the report groups in the current AWS account.
    reportGroups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | The list of ARNs for the report groups in the current AWS account.
listReportGroupsResponse_reportGroups :: Lens.Lens' ListReportGroupsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listReportGroupsResponse_reportGroups = Lens.lens (\ListReportGroupsResponse' {reportGroups} -> reportGroups) (\s@ListReportGroupsResponse' {} a -> s {reportGroups = a} :: ListReportGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listReportGroupsResponse_httpStatus :: Lens.Lens' ListReportGroupsResponse Prelude.Int
listReportGroupsResponse_httpStatus = Lens.lens (\ListReportGroupsResponse' {httpStatus} -> httpStatus) (\s@ListReportGroupsResponse' {} a -> s {httpStatus = a} :: ListReportGroupsResponse)

instance Prelude.NFData ListReportGroupsResponse
