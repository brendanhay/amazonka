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
-- Module      : Amazonka.Connect.SearchHoursOfOperations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches the hours of operation in an Amazon Connect instance, with
-- optional filtering.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchHoursOfOperations
  ( -- * Creating a Request
    SearchHoursOfOperations (..),
    newSearchHoursOfOperations,

    -- * Request Lenses
    searchHoursOfOperations_maxResults,
    searchHoursOfOperations_nextToken,
    searchHoursOfOperations_searchCriteria,
    searchHoursOfOperations_searchFilter,
    searchHoursOfOperations_instanceId,

    -- * Destructuring the Response
    SearchHoursOfOperationsResponse (..),
    newSearchHoursOfOperationsResponse,

    -- * Response Lenses
    searchHoursOfOperationsResponse_approximateTotalCount,
    searchHoursOfOperationsResponse_hoursOfOperations,
    searchHoursOfOperationsResponse_nextToken,
    searchHoursOfOperationsResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchHoursOfOperations' smart constructor.
data SearchHoursOfOperations = SearchHoursOfOperations'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The search criteria to be used to return hours of operations.
    searchCriteria :: Prelude.Maybe HoursOfOperationSearchCriteria,
    -- | Filters to be applied to search results.
    searchFilter :: Prelude.Maybe HoursOfOperationSearchFilter,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchHoursOfOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'searchHoursOfOperations_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'searchHoursOfOperations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'searchCriteria', 'searchHoursOfOperations_searchCriteria' - The search criteria to be used to return hours of operations.
--
-- 'searchFilter', 'searchHoursOfOperations_searchFilter' - Filters to be applied to search results.
--
-- 'instanceId', 'searchHoursOfOperations_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newSearchHoursOfOperations ::
  -- | 'instanceId'
  Prelude.Text ->
  SearchHoursOfOperations
newSearchHoursOfOperations pInstanceId_ =
  SearchHoursOfOperations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      searchCriteria = Prelude.Nothing,
      searchFilter = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
searchHoursOfOperations_maxResults :: Lens.Lens' SearchHoursOfOperations (Prelude.Maybe Prelude.Natural)
searchHoursOfOperations_maxResults = Lens.lens (\SearchHoursOfOperations' {maxResults} -> maxResults) (\s@SearchHoursOfOperations' {} a -> s {maxResults = a} :: SearchHoursOfOperations)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchHoursOfOperations_nextToken :: Lens.Lens' SearchHoursOfOperations (Prelude.Maybe Prelude.Text)
searchHoursOfOperations_nextToken = Lens.lens (\SearchHoursOfOperations' {nextToken} -> nextToken) (\s@SearchHoursOfOperations' {} a -> s {nextToken = a} :: SearchHoursOfOperations)

-- | The search criteria to be used to return hours of operations.
searchHoursOfOperations_searchCriteria :: Lens.Lens' SearchHoursOfOperations (Prelude.Maybe HoursOfOperationSearchCriteria)
searchHoursOfOperations_searchCriteria = Lens.lens (\SearchHoursOfOperations' {searchCriteria} -> searchCriteria) (\s@SearchHoursOfOperations' {} a -> s {searchCriteria = a} :: SearchHoursOfOperations)

-- | Filters to be applied to search results.
searchHoursOfOperations_searchFilter :: Lens.Lens' SearchHoursOfOperations (Prelude.Maybe HoursOfOperationSearchFilter)
searchHoursOfOperations_searchFilter = Lens.lens (\SearchHoursOfOperations' {searchFilter} -> searchFilter) (\s@SearchHoursOfOperations' {} a -> s {searchFilter = a} :: SearchHoursOfOperations)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
searchHoursOfOperations_instanceId :: Lens.Lens' SearchHoursOfOperations Prelude.Text
searchHoursOfOperations_instanceId = Lens.lens (\SearchHoursOfOperations' {instanceId} -> instanceId) (\s@SearchHoursOfOperations' {} a -> s {instanceId = a} :: SearchHoursOfOperations)

instance Core.AWSPager SearchHoursOfOperations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchHoursOfOperationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchHoursOfOperationsResponse_hoursOfOperations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchHoursOfOperations_nextToken
          Lens..~ rs
          Lens.^? searchHoursOfOperationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchHoursOfOperations where
  type
    AWSResponse SearchHoursOfOperations =
      SearchHoursOfOperationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchHoursOfOperationsResponse'
            Prelude.<$> (x Data..?> "ApproximateTotalCount")
            Prelude.<*> ( x
                            Data..?> "HoursOfOperations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchHoursOfOperations where
  hashWithSalt _salt SearchHoursOfOperations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` searchCriteria
      `Prelude.hashWithSalt` searchFilter
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SearchHoursOfOperations where
  rnf SearchHoursOfOperations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchCriteria
      `Prelude.seq` Prelude.rnf searchFilter
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders SearchHoursOfOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchHoursOfOperations where
  toJSON SearchHoursOfOperations' {..} =
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

instance Data.ToPath SearchHoursOfOperations where
  toPath = Prelude.const "/search-hours-of-operations"

instance Data.ToQuery SearchHoursOfOperations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchHoursOfOperationsResponse' smart constructor.
data SearchHoursOfOperationsResponse = SearchHoursOfOperationsResponse'
  { -- | The total number of hours of operations which matched your search query.
    approximateTotalCount :: Prelude.Maybe Prelude.Integer,
    -- | Information about the hours of operations.
    hoursOfOperations :: Prelude.Maybe [HoursOfOperation],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchHoursOfOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateTotalCount', 'searchHoursOfOperationsResponse_approximateTotalCount' - The total number of hours of operations which matched your search query.
--
-- 'hoursOfOperations', 'searchHoursOfOperationsResponse_hoursOfOperations' - Information about the hours of operations.
--
-- 'nextToken', 'searchHoursOfOperationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'searchHoursOfOperationsResponse_httpStatus' - The response's http status code.
newSearchHoursOfOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchHoursOfOperationsResponse
newSearchHoursOfOperationsResponse pHttpStatus_ =
  SearchHoursOfOperationsResponse'
    { approximateTotalCount =
        Prelude.Nothing,
      hoursOfOperations = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of hours of operations which matched your search query.
searchHoursOfOperationsResponse_approximateTotalCount :: Lens.Lens' SearchHoursOfOperationsResponse (Prelude.Maybe Prelude.Integer)
searchHoursOfOperationsResponse_approximateTotalCount = Lens.lens (\SearchHoursOfOperationsResponse' {approximateTotalCount} -> approximateTotalCount) (\s@SearchHoursOfOperationsResponse' {} a -> s {approximateTotalCount = a} :: SearchHoursOfOperationsResponse)

-- | Information about the hours of operations.
searchHoursOfOperationsResponse_hoursOfOperations :: Lens.Lens' SearchHoursOfOperationsResponse (Prelude.Maybe [HoursOfOperation])
searchHoursOfOperationsResponse_hoursOfOperations = Lens.lens (\SearchHoursOfOperationsResponse' {hoursOfOperations} -> hoursOfOperations) (\s@SearchHoursOfOperationsResponse' {} a -> s {hoursOfOperations = a} :: SearchHoursOfOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
searchHoursOfOperationsResponse_nextToken :: Lens.Lens' SearchHoursOfOperationsResponse (Prelude.Maybe Prelude.Text)
searchHoursOfOperationsResponse_nextToken = Lens.lens (\SearchHoursOfOperationsResponse' {nextToken} -> nextToken) (\s@SearchHoursOfOperationsResponse' {} a -> s {nextToken = a} :: SearchHoursOfOperationsResponse)

-- | The response's http status code.
searchHoursOfOperationsResponse_httpStatus :: Lens.Lens' SearchHoursOfOperationsResponse Prelude.Int
searchHoursOfOperationsResponse_httpStatus = Lens.lens (\SearchHoursOfOperationsResponse' {httpStatus} -> httpStatus) (\s@SearchHoursOfOperationsResponse' {} a -> s {httpStatus = a} :: SearchHoursOfOperationsResponse)

instance
  Prelude.NFData
    SearchHoursOfOperationsResponse
  where
  rnf SearchHoursOfOperationsResponse' {..} =
    Prelude.rnf approximateTotalCount
      `Prelude.seq` Prelude.rnf hoursOfOperations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
