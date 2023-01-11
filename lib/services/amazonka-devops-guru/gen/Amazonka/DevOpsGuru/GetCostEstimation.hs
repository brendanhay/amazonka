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
-- Module      : Amazonka.DevOpsGuru.GetCostEstimation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an estimate of the monthly cost for DevOps Guru to analyze your
-- Amazon Web Services resources. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/cost-estimate.html Estimate your Amazon DevOps Guru costs>
-- and
-- <http://aws.amazon.com/devops-guru/pricing/ Amazon DevOps Guru pricing>.
--
-- This operation returns paginated results.
module Amazonka.DevOpsGuru.GetCostEstimation
  ( -- * Creating a Request
    GetCostEstimation (..),
    newGetCostEstimation,

    -- * Request Lenses
    getCostEstimation_nextToken,

    -- * Destructuring the Response
    GetCostEstimationResponse (..),
    newGetCostEstimationResponse,

    -- * Response Lenses
    getCostEstimationResponse_costs,
    getCostEstimationResponse_nextToken,
    getCostEstimationResponse_resourceCollection,
    getCostEstimationResponse_status,
    getCostEstimationResponse_timeRange,
    getCostEstimationResponse_totalCost,
    getCostEstimationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCostEstimation' smart constructor.
data GetCostEstimation = GetCostEstimation'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostEstimation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCostEstimation_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
newGetCostEstimation ::
  GetCostEstimation
newGetCostEstimation =
  GetCostEstimation' {nextToken = Prelude.Nothing}

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
getCostEstimation_nextToken :: Lens.Lens' GetCostEstimation (Prelude.Maybe Prelude.Text)
getCostEstimation_nextToken = Lens.lens (\GetCostEstimation' {nextToken} -> nextToken) (\s@GetCostEstimation' {} a -> s {nextToken = a} :: GetCostEstimation)

instance Core.AWSPager GetCostEstimation where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCostEstimationResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCostEstimationResponse_costs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getCostEstimation_nextToken
          Lens..~ rs
          Lens.^? getCostEstimationResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetCostEstimation where
  type
    AWSResponse GetCostEstimation =
      GetCostEstimationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCostEstimationResponse'
            Prelude.<$> (x Data..?> "Costs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResourceCollection")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TimeRange")
            Prelude.<*> (x Data..?> "TotalCost")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCostEstimation where
  hashWithSalt _salt GetCostEstimation' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetCostEstimation where
  rnf GetCostEstimation' {..} = Prelude.rnf nextToken

instance Data.ToHeaders GetCostEstimation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCostEstimation where
  toPath = Prelude.const "/cost-estimation"

instance Data.ToQuery GetCostEstimation where
  toQuery GetCostEstimation' {..} =
    Prelude.mconcat ["NextToken" Data.=: nextToken]

-- | /See:/ 'newGetCostEstimationResponse' smart constructor.
data GetCostEstimationResponse = GetCostEstimationResponse'
  { -- | An array of @ResourceCost@ objects that each contains details about the
    -- monthly cost estimate to analyze one of your Amazon Web Services
    -- resources.
    costs :: Prelude.Maybe [ServiceResourceCost],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of the Amazon Web Services resources used to create your
    -- monthly DevOps Guru cost estimate.
    resourceCollection :: Prelude.Maybe CostEstimationResourceCollectionFilter,
    -- | The status of creating this cost estimate. If it\'s still in progress,
    -- the status @ONGOING@ is returned. If it is finished, the status
    -- @COMPLETED@ is returned.
    status :: Prelude.Maybe CostEstimationStatus,
    -- | The start and end time of the cost estimation.
    timeRange :: Prelude.Maybe CostEstimationTimeRange,
    -- | The estimated monthly cost to analyze the Amazon Web Services resources.
    -- This value is the sum of the estimated costs to analyze each resource in
    -- the @Costs@ object in this response.
    totalCost :: Prelude.Maybe Prelude.Double,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCostEstimationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costs', 'getCostEstimationResponse_costs' - An array of @ResourceCost@ objects that each contains details about the
-- monthly cost estimate to analyze one of your Amazon Web Services
-- resources.
--
-- 'nextToken', 'getCostEstimationResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'resourceCollection', 'getCostEstimationResponse_resourceCollection' - The collection of the Amazon Web Services resources used to create your
-- monthly DevOps Guru cost estimate.
--
-- 'status', 'getCostEstimationResponse_status' - The status of creating this cost estimate. If it\'s still in progress,
-- the status @ONGOING@ is returned. If it is finished, the status
-- @COMPLETED@ is returned.
--
-- 'timeRange', 'getCostEstimationResponse_timeRange' - The start and end time of the cost estimation.
--
-- 'totalCost', 'getCostEstimationResponse_totalCost' - The estimated monthly cost to analyze the Amazon Web Services resources.
-- This value is the sum of the estimated costs to analyze each resource in
-- the @Costs@ object in this response.
--
-- 'httpStatus', 'getCostEstimationResponse_httpStatus' - The response's http status code.
newGetCostEstimationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCostEstimationResponse
newGetCostEstimationResponse pHttpStatus_ =
  GetCostEstimationResponse'
    { costs = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      status = Prelude.Nothing,
      timeRange = Prelude.Nothing,
      totalCost = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @ResourceCost@ objects that each contains details about the
-- monthly cost estimate to analyze one of your Amazon Web Services
-- resources.
getCostEstimationResponse_costs :: Lens.Lens' GetCostEstimationResponse (Prelude.Maybe [ServiceResourceCost])
getCostEstimationResponse_costs = Lens.lens (\GetCostEstimationResponse' {costs} -> costs) (\s@GetCostEstimationResponse' {} a -> s {costs = a} :: GetCostEstimationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
getCostEstimationResponse_nextToken :: Lens.Lens' GetCostEstimationResponse (Prelude.Maybe Prelude.Text)
getCostEstimationResponse_nextToken = Lens.lens (\GetCostEstimationResponse' {nextToken} -> nextToken) (\s@GetCostEstimationResponse' {} a -> s {nextToken = a} :: GetCostEstimationResponse)

-- | The collection of the Amazon Web Services resources used to create your
-- monthly DevOps Guru cost estimate.
getCostEstimationResponse_resourceCollection :: Lens.Lens' GetCostEstimationResponse (Prelude.Maybe CostEstimationResourceCollectionFilter)
getCostEstimationResponse_resourceCollection = Lens.lens (\GetCostEstimationResponse' {resourceCollection} -> resourceCollection) (\s@GetCostEstimationResponse' {} a -> s {resourceCollection = a} :: GetCostEstimationResponse)

-- | The status of creating this cost estimate. If it\'s still in progress,
-- the status @ONGOING@ is returned. If it is finished, the status
-- @COMPLETED@ is returned.
getCostEstimationResponse_status :: Lens.Lens' GetCostEstimationResponse (Prelude.Maybe CostEstimationStatus)
getCostEstimationResponse_status = Lens.lens (\GetCostEstimationResponse' {status} -> status) (\s@GetCostEstimationResponse' {} a -> s {status = a} :: GetCostEstimationResponse)

-- | The start and end time of the cost estimation.
getCostEstimationResponse_timeRange :: Lens.Lens' GetCostEstimationResponse (Prelude.Maybe CostEstimationTimeRange)
getCostEstimationResponse_timeRange = Lens.lens (\GetCostEstimationResponse' {timeRange} -> timeRange) (\s@GetCostEstimationResponse' {} a -> s {timeRange = a} :: GetCostEstimationResponse)

-- | The estimated monthly cost to analyze the Amazon Web Services resources.
-- This value is the sum of the estimated costs to analyze each resource in
-- the @Costs@ object in this response.
getCostEstimationResponse_totalCost :: Lens.Lens' GetCostEstimationResponse (Prelude.Maybe Prelude.Double)
getCostEstimationResponse_totalCost = Lens.lens (\GetCostEstimationResponse' {totalCost} -> totalCost) (\s@GetCostEstimationResponse' {} a -> s {totalCost = a} :: GetCostEstimationResponse)

-- | The response's http status code.
getCostEstimationResponse_httpStatus :: Lens.Lens' GetCostEstimationResponse Prelude.Int
getCostEstimationResponse_httpStatus = Lens.lens (\GetCostEstimationResponse' {httpStatus} -> httpStatus) (\s@GetCostEstimationResponse' {} a -> s {httpStatus = a} :: GetCostEstimationResponse)

instance Prelude.NFData GetCostEstimationResponse where
  rnf GetCostEstimationResponse' {..} =
    Prelude.rnf costs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf timeRange
      `Prelude.seq` Prelude.rnf totalCost
      `Prelude.seq` Prelude.rnf httpStatus
