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
-- Module      : Amazonka.Connect.SearchQuickConnects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches quick connects in an Amazon Connect instance, with optional
-- filtering.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchQuickConnects
  ( -- * Creating a Request
    SearchQuickConnects (..),
    newSearchQuickConnects,

    -- * Request Lenses
    searchQuickConnects_maxResults,
    searchQuickConnects_nextToken,
    searchQuickConnects_searchCriteria,
    searchQuickConnects_searchFilter,
    searchQuickConnects_instanceId,

    -- * Destructuring the Response
    SearchQuickConnectsResponse (..),
    newSearchQuickConnectsResponse,

    -- * Response Lenses
    searchQuickConnectsResponse_approximateTotalCount,
    searchQuickConnectsResponse_nextToken,
    searchQuickConnectsResponse_quickConnects,
    searchQuickConnectsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchQuickConnects' smart constructor.
data SearchQuickConnects = SearchQuickConnects'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The search criteria to be used to return quick connects.
    searchCriteria :: Prelude.Maybe QuickConnectSearchCriteria,
    -- | Filters to be applied to search results.
    searchFilter :: Prelude.Maybe QuickConnectSearchFilter,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchQuickConnects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchQuickConnects_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchQuickConnects_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'searchCriteria', 'searchQuickConnects_searchCriteria' - The search criteria to be used to return quick connects.
--
-- 'searchFilter', 'searchQuickConnects_searchFilter' - Filters to be applied to search results.
--
-- 'instanceId', 'searchQuickConnects_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newSearchQuickConnects ::
  -- | 'instanceId'
  Prelude.Text ->
  SearchQuickConnects
newSearchQuickConnects pInstanceId_ =
  SearchQuickConnects'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchCriteria = Prelude.Nothing,
      searchFilter = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
searchQuickConnects_maxResults :: Lens.Lens' SearchQuickConnects (Prelude.Maybe Prelude.Natural)
searchQuickConnects_maxResults = Lens.lens (\SearchQuickConnects' {maxResults} -> maxResults) (\s@SearchQuickConnects' {} a -> s {maxResults = a} :: SearchQuickConnects)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchQuickConnects_nextToken :: Lens.Lens' SearchQuickConnects (Prelude.Maybe Prelude.Text)
searchQuickConnects_nextToken = Lens.lens (\SearchQuickConnects' {nextToken} -> nextToken) (\s@SearchQuickConnects' {} a -> s {nextToken = a} :: SearchQuickConnects)

-- | The search criteria to be used to return quick connects.
searchQuickConnects_searchCriteria :: Lens.Lens' SearchQuickConnects (Prelude.Maybe QuickConnectSearchCriteria)
searchQuickConnects_searchCriteria = Lens.lens (\SearchQuickConnects' {searchCriteria} -> searchCriteria) (\s@SearchQuickConnects' {} a -> s {searchCriteria = a} :: SearchQuickConnects)

-- | Filters to be applied to search results.
searchQuickConnects_searchFilter :: Lens.Lens' SearchQuickConnects (Prelude.Maybe QuickConnectSearchFilter)
searchQuickConnects_searchFilter = Lens.lens (\SearchQuickConnects' {searchFilter} -> searchFilter) (\s@SearchQuickConnects' {} a -> s {searchFilter = a} :: SearchQuickConnects)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
searchQuickConnects_instanceId :: Lens.Lens' SearchQuickConnects Prelude.Text
searchQuickConnects_instanceId = Lens.lens (\SearchQuickConnects' {instanceId} -> instanceId) (\s@SearchQuickConnects' {} a -> s {instanceId = a} :: SearchQuickConnects)

instance Core.AWSPager SearchQuickConnects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchQuickConnectsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchQuickConnectsResponse_quickConnects
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchQuickConnects_nextToken
          Lens..~ rs
          Lens.^? searchQuickConnectsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchQuickConnects where
  type
    AWSResponse SearchQuickConnects =
      SearchQuickConnectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchQuickConnectsResponse'
            Prelude.<$> (x Data..?> "ApproximateTotalCount")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "QuickConnects" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchQuickConnects where
  hashWithSalt _salt SearchQuickConnects' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchCriteria
      `Prelude.hashWithSalt` searchFilter
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SearchQuickConnects where
  rnf SearchQuickConnects' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchCriteria
      `Prelude.seq` Prelude.rnf searchFilter
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders SearchQuickConnects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchQuickConnects where
  toJSON SearchQuickConnects' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SearchCriteria" Data..=)
              Prelude.<$> searchCriteria,
            ("SearchFilter" Data..=) Prelude.<$> searchFilter,
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath SearchQuickConnects where
  toPath = Prelude.const "/search-quick-connects"

instance Data.ToQuery SearchQuickConnects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchQuickConnectsResponse' smart constructor.
data SearchQuickConnectsResponse = SearchQuickConnectsResponse'
  { -- | The total number of quick connects which matched your search query.
    approximateTotalCount :: Prelude.Maybe Prelude.Integer,
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the quick connects.
    quickConnects :: Prelude.Maybe [QuickConnect],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchQuickConnectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateTotalCount', 'searchQuickConnectsResponse_approximateTotalCount' - The total number of quick connects which matched your search query.
--
-- 'nextToken', 'searchQuickConnectsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'quickConnects', 'searchQuickConnectsResponse_quickConnects' - Information about the quick connects.
--
-- 'httpStatus', 'searchQuickConnectsResponse_httpStatus' - The response's http status code.
newSearchQuickConnectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchQuickConnectsResponse
newSearchQuickConnectsResponse pHttpStatus_ =
  SearchQuickConnectsResponse'
    { approximateTotalCount =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      quickConnects = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of quick connects which matched your search query.
searchQuickConnectsResponse_approximateTotalCount :: Lens.Lens' SearchQuickConnectsResponse (Prelude.Maybe Prelude.Integer)
searchQuickConnectsResponse_approximateTotalCount = Lens.lens (\SearchQuickConnectsResponse' {approximateTotalCount} -> approximateTotalCount) (\s@SearchQuickConnectsResponse' {} a -> s {approximateTotalCount = a} :: SearchQuickConnectsResponse)

-- | If there are additional results, this is the token for the next set of
-- results.
searchQuickConnectsResponse_nextToken :: Lens.Lens' SearchQuickConnectsResponse (Prelude.Maybe Prelude.Text)
searchQuickConnectsResponse_nextToken = Lens.lens (\SearchQuickConnectsResponse' {nextToken} -> nextToken) (\s@SearchQuickConnectsResponse' {} a -> s {nextToken = a} :: SearchQuickConnectsResponse)

-- | Information about the quick connects.
searchQuickConnectsResponse_quickConnects :: Lens.Lens' SearchQuickConnectsResponse (Prelude.Maybe [QuickConnect])
searchQuickConnectsResponse_quickConnects = Lens.lens (\SearchQuickConnectsResponse' {quickConnects} -> quickConnects) (\s@SearchQuickConnectsResponse' {} a -> s {quickConnects = a} :: SearchQuickConnectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchQuickConnectsResponse_httpStatus :: Lens.Lens' SearchQuickConnectsResponse Prelude.Int
searchQuickConnectsResponse_httpStatus = Lens.lens (\SearchQuickConnectsResponse' {httpStatus} -> httpStatus) (\s@SearchQuickConnectsResponse' {} a -> s {httpStatus = a} :: SearchQuickConnectsResponse)

instance Prelude.NFData SearchQuickConnectsResponse where
  rnf SearchQuickConnectsResponse' {..} =
    Prelude.rnf approximateTotalCount
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf quickConnects
      `Prelude.seq` Prelude.rnf httpStatus
