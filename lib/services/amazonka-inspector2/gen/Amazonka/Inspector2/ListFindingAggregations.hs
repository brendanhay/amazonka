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
-- Module      : Amazonka.Inspector2.ListFindingAggregations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists aggregated finding data for your environment based on specific
-- criteria.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.ListFindingAggregations
  ( -- * Creating a Request
    ListFindingAggregations (..),
    newListFindingAggregations,

    -- * Request Lenses
    listFindingAggregations_accountIds,
    listFindingAggregations_aggregationRequest,
    listFindingAggregations_maxResults,
    listFindingAggregations_nextToken,
    listFindingAggregations_aggregationType,

    -- * Destructuring the Response
    ListFindingAggregationsResponse (..),
    newListFindingAggregationsResponse,

    -- * Response Lenses
    listFindingAggregationsResponse_nextToken,
    listFindingAggregationsResponse_responses,
    listFindingAggregationsResponse_httpStatus,
    listFindingAggregationsResponse_aggregationType,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFindingAggregations' smart constructor.
data ListFindingAggregations = ListFindingAggregations'
  { -- | The Amazon Web Services account IDs to retrieve finding aggregation data
    -- for.
    accountIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Details of the aggregation request that is used to filter your
    -- aggregation results.
    aggregationRequest :: Prelude.Maybe AggregationRequest,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of the aggregation request.
    aggregationType :: AggregationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingAggregations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'listFindingAggregations_accountIds' - The Amazon Web Services account IDs to retrieve finding aggregation data
-- for.
--
-- 'aggregationRequest', 'listFindingAggregations_aggregationRequest' - Details of the aggregation request that is used to filter your
-- aggregation results.
--
-- 'maxResults', 'listFindingAggregations_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listFindingAggregations_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'aggregationType', 'listFindingAggregations_aggregationType' - The type of the aggregation request.
newListFindingAggregations ::
  -- | 'aggregationType'
  AggregationType ->
  ListFindingAggregations
newListFindingAggregations pAggregationType_ =
  ListFindingAggregations'
    { accountIds =
        Prelude.Nothing,
      aggregationRequest = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      aggregationType = pAggregationType_
    }

-- | The Amazon Web Services account IDs to retrieve finding aggregation data
-- for.
listFindingAggregations_accountIds :: Lens.Lens' ListFindingAggregations (Prelude.Maybe (Prelude.NonEmpty StringFilter))
listFindingAggregations_accountIds = Lens.lens (\ListFindingAggregations' {accountIds} -> accountIds) (\s@ListFindingAggregations' {} a -> s {accountIds = a} :: ListFindingAggregations) Prelude.. Lens.mapping Lens.coerced

-- | Details of the aggregation request that is used to filter your
-- aggregation results.
listFindingAggregations_aggregationRequest :: Lens.Lens' ListFindingAggregations (Prelude.Maybe AggregationRequest)
listFindingAggregations_aggregationRequest = Lens.lens (\ListFindingAggregations' {aggregationRequest} -> aggregationRequest) (\s@ListFindingAggregations' {} a -> s {aggregationRequest = a} :: ListFindingAggregations)

-- | The maximum number of results to return in the response.
listFindingAggregations_maxResults :: Lens.Lens' ListFindingAggregations (Prelude.Maybe Prelude.Natural)
listFindingAggregations_maxResults = Lens.lens (\ListFindingAggregations' {maxResults} -> maxResults) (\s@ListFindingAggregations' {} a -> s {maxResults = a} :: ListFindingAggregations)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listFindingAggregations_nextToken :: Lens.Lens' ListFindingAggregations (Prelude.Maybe Prelude.Text)
listFindingAggregations_nextToken = Lens.lens (\ListFindingAggregations' {nextToken} -> nextToken) (\s@ListFindingAggregations' {} a -> s {nextToken = a} :: ListFindingAggregations)

-- | The type of the aggregation request.
listFindingAggregations_aggregationType :: Lens.Lens' ListFindingAggregations AggregationType
listFindingAggregations_aggregationType = Lens.lens (\ListFindingAggregations' {aggregationType} -> aggregationType) (\s@ListFindingAggregations' {} a -> s {aggregationType = a} :: ListFindingAggregations)

instance Core.AWSPager ListFindingAggregations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFindingAggregationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFindingAggregationsResponse_responses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFindingAggregations_nextToken
          Lens..~ rs
          Lens.^? listFindingAggregationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFindingAggregations where
  type
    AWSResponse ListFindingAggregations =
      ListFindingAggregationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFindingAggregationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "responses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "aggregationType")
      )

instance Prelude.Hashable ListFindingAggregations where
  hashWithSalt _salt ListFindingAggregations' {..} =
    _salt
      `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` aggregationRequest
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` aggregationType

instance Prelude.NFData ListFindingAggregations where
  rnf ListFindingAggregations' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf aggregationRequest
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf aggregationType

instance Data.ToHeaders ListFindingAggregations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFindingAggregations where
  toJSON ListFindingAggregations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountIds" Data..=) Prelude.<$> accountIds,
            ("aggregationRequest" Data..=)
              Prelude.<$> aggregationRequest,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("aggregationType" Data..= aggregationType)
          ]
      )

instance Data.ToPath ListFindingAggregations where
  toPath = Prelude.const "/findings/aggregation/list"

instance Data.ToQuery ListFindingAggregations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFindingAggregationsResponse' smart constructor.
data ListFindingAggregationsResponse = ListFindingAggregationsResponse'
  { -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Objects that contain the results of an aggregation operation.
    responses :: Prelude.Maybe [AggregationResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The type of aggregation to perform.
    aggregationType :: AggregationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingAggregationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFindingAggregationsResponse_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'responses', 'listFindingAggregationsResponse_responses' - Objects that contain the results of an aggregation operation.
--
-- 'httpStatus', 'listFindingAggregationsResponse_httpStatus' - The response's http status code.
--
-- 'aggregationType', 'listFindingAggregationsResponse_aggregationType' - The type of aggregation to perform.
newListFindingAggregationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'aggregationType'
  AggregationType ->
  ListFindingAggregationsResponse
newListFindingAggregationsResponse
  pHttpStatus_
  pAggregationType_ =
    ListFindingAggregationsResponse'
      { nextToken =
          Prelude.Nothing,
        responses = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        aggregationType = pAggregationType_
      }

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listFindingAggregationsResponse_nextToken :: Lens.Lens' ListFindingAggregationsResponse (Prelude.Maybe Prelude.Text)
listFindingAggregationsResponse_nextToken = Lens.lens (\ListFindingAggregationsResponse' {nextToken} -> nextToken) (\s@ListFindingAggregationsResponse' {} a -> s {nextToken = a} :: ListFindingAggregationsResponse)

-- | Objects that contain the results of an aggregation operation.
listFindingAggregationsResponse_responses :: Lens.Lens' ListFindingAggregationsResponse (Prelude.Maybe [AggregationResponse])
listFindingAggregationsResponse_responses = Lens.lens (\ListFindingAggregationsResponse' {responses} -> responses) (\s@ListFindingAggregationsResponse' {} a -> s {responses = a} :: ListFindingAggregationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFindingAggregationsResponse_httpStatus :: Lens.Lens' ListFindingAggregationsResponse Prelude.Int
listFindingAggregationsResponse_httpStatus = Lens.lens (\ListFindingAggregationsResponse' {httpStatus} -> httpStatus) (\s@ListFindingAggregationsResponse' {} a -> s {httpStatus = a} :: ListFindingAggregationsResponse)

-- | The type of aggregation to perform.
listFindingAggregationsResponse_aggregationType :: Lens.Lens' ListFindingAggregationsResponse AggregationType
listFindingAggregationsResponse_aggregationType = Lens.lens (\ListFindingAggregationsResponse' {aggregationType} -> aggregationType) (\s@ListFindingAggregationsResponse' {} a -> s {aggregationType = a} :: ListFindingAggregationsResponse)

instance
  Prelude.NFData
    ListFindingAggregationsResponse
  where
  rnf ListFindingAggregationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf responses
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf aggregationType
