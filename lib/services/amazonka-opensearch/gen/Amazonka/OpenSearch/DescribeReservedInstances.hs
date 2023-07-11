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
-- Module      : Amazonka.OpenSearch.DescribeReservedInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Amazon OpenSearch Service instances that you have reserved
-- in a given Region. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/ri.html Reserved Instances in Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DescribeReservedInstances
  ( -- * Creating a Request
    DescribeReservedInstances (..),
    newDescribeReservedInstances,

    -- * Request Lenses
    describeReservedInstances_maxResults,
    describeReservedInstances_nextToken,
    describeReservedInstances_reservedInstanceId,

    -- * Destructuring the Response
    DescribeReservedInstancesResponse (..),
    newDescribeReservedInstancesResponse,

    -- * Response Lenses
    describeReservedInstancesResponse_nextToken,
    describeReservedInstancesResponse_reservedInstances,
    describeReservedInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @DescribeReservedInstances@
-- operation.
--
-- /See:/ 'newDescribeReservedInstances' smart constructor.
data DescribeReservedInstances = DescribeReservedInstances'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @DescribeReservedInstances@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @DescribeReservedInstances@ operations, which returns results in the
    -- next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The reserved instance identifier filter value. Use this parameter to
    -- show only the reservation that matches the specified reserved OpenSearch
    -- instance ID.
    reservedInstanceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeReservedInstances_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'describeReservedInstances_nextToken' - If your initial @DescribeReservedInstances@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeReservedInstances@ operations, which returns results in the
-- next page.
--
-- 'reservedInstanceId', 'describeReservedInstances_reservedInstanceId' - The reserved instance identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reserved OpenSearch
-- instance ID.
newDescribeReservedInstances ::
  DescribeReservedInstances
newDescribeReservedInstances =
  DescribeReservedInstances'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      reservedInstanceId = Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
describeReservedInstances_maxResults :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe Prelude.Int)
describeReservedInstances_maxResults = Lens.lens (\DescribeReservedInstances' {maxResults} -> maxResults) (\s@DescribeReservedInstances' {} a -> s {maxResults = a} :: DescribeReservedInstances)

-- | If your initial @DescribeReservedInstances@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeReservedInstances@ operations, which returns results in the
-- next page.
describeReservedInstances_nextToken :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe Prelude.Text)
describeReservedInstances_nextToken = Lens.lens (\DescribeReservedInstances' {nextToken} -> nextToken) (\s@DescribeReservedInstances' {} a -> s {nextToken = a} :: DescribeReservedInstances)

-- | The reserved instance identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reserved OpenSearch
-- instance ID.
describeReservedInstances_reservedInstanceId :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe Prelude.Text)
describeReservedInstances_reservedInstanceId = Lens.lens (\DescribeReservedInstances' {reservedInstanceId} -> reservedInstanceId) (\s@DescribeReservedInstances' {} a -> s {reservedInstanceId = a} :: DescribeReservedInstances)

instance Core.AWSRequest DescribeReservedInstances where
  type
    AWSResponse DescribeReservedInstances =
      DescribeReservedInstancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedInstancesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ReservedInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservedInstances where
  hashWithSalt _salt DescribeReservedInstances' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` reservedInstanceId

instance Prelude.NFData DescribeReservedInstances where
  rnf DescribeReservedInstances' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reservedInstanceId

instance Data.ToHeaders DescribeReservedInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeReservedInstances where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/reservedInstances"

instance Data.ToQuery DescribeReservedInstances where
  toQuery DescribeReservedInstances' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "reservationId" Data.=: reservedInstanceId
      ]

-- | Container for results from @DescribeReservedInstances@
--
-- /See:/ 'newDescribeReservedInstancesResponse' smart constructor.
data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of Reserved Instances in the current Region.
    reservedInstances :: Prelude.Maybe [ReservedInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReservedInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeReservedInstancesResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'reservedInstances', 'describeReservedInstancesResponse_reservedInstances' - List of Reserved Instances in the current Region.
--
-- 'httpStatus', 'describeReservedInstancesResponse_httpStatus' - The response's http status code.
newDescribeReservedInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReservedInstancesResponse
newDescribeReservedInstancesResponse pHttpStatus_ =
  DescribeReservedInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      reservedInstances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
describeReservedInstancesResponse_nextToken :: Lens.Lens' DescribeReservedInstancesResponse (Prelude.Maybe Prelude.Text)
describeReservedInstancesResponse_nextToken = Lens.lens (\DescribeReservedInstancesResponse' {nextToken} -> nextToken) (\s@DescribeReservedInstancesResponse' {} a -> s {nextToken = a} :: DescribeReservedInstancesResponse)

-- | List of Reserved Instances in the current Region.
describeReservedInstancesResponse_reservedInstances :: Lens.Lens' DescribeReservedInstancesResponse (Prelude.Maybe [ReservedInstance])
describeReservedInstancesResponse_reservedInstances = Lens.lens (\DescribeReservedInstancesResponse' {reservedInstances} -> reservedInstances) (\s@DescribeReservedInstancesResponse' {} a -> s {reservedInstances = a} :: DescribeReservedInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReservedInstancesResponse_httpStatus :: Lens.Lens' DescribeReservedInstancesResponse Prelude.Int
describeReservedInstancesResponse_httpStatus = Lens.lens (\DescribeReservedInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeReservedInstancesResponse' {} a -> s {httpStatus = a} :: DescribeReservedInstancesResponse)

instance
  Prelude.NFData
    DescribeReservedInstancesResponse
  where
  rnf DescribeReservedInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf reservedInstances
      `Prelude.seq` Prelude.rnf httpStatus
