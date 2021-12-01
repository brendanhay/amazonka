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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved OpenSearch instances for this
-- account.
module Amazonka.OpenSearch.DescribeReservedInstances
  ( -- * Creating a Request
    DescribeReservedInstances (..),
    newDescribeReservedInstances,

    -- * Request Lenses
    describeReservedInstances_nextToken,
    describeReservedInstances_reservedInstanceId,
    describeReservedInstances_maxResults,

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
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for parameters to @DescribeReservedInstances@
--
-- /See:/ 'newDescribeReservedInstances' smart constructor.
data DescribeReservedInstances = DescribeReservedInstances'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The reserved instance identifier filter value. Use this parameter to
    -- show only the reservation that matches the specified reserved OpenSearch
    -- instance ID.
    reservedInstanceId :: Prelude.Maybe Prelude.Text,
    -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'nextToken', 'describeReservedInstances_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'reservedInstanceId', 'describeReservedInstances_reservedInstanceId' - The reserved instance identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reserved OpenSearch
-- instance ID.
--
-- 'maxResults', 'describeReservedInstances_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
newDescribeReservedInstances ::
  DescribeReservedInstances
newDescribeReservedInstances =
  DescribeReservedInstances'
    { nextToken =
        Prelude.Nothing,
      reservedInstanceId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedInstances_nextToken :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe Prelude.Text)
describeReservedInstances_nextToken = Lens.lens (\DescribeReservedInstances' {nextToken} -> nextToken) (\s@DescribeReservedInstances' {} a -> s {nextToken = a} :: DescribeReservedInstances)

-- | The reserved instance identifier filter value. Use this parameter to
-- show only the reservation that matches the specified reserved OpenSearch
-- instance ID.
describeReservedInstances_reservedInstanceId :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe Prelude.Text)
describeReservedInstances_reservedInstanceId = Lens.lens (\DescribeReservedInstances' {reservedInstanceId} -> reservedInstanceId) (\s@DescribeReservedInstances' {} a -> s {reservedInstanceId = a} :: DescribeReservedInstances)

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeReservedInstances_maxResults :: Lens.Lens' DescribeReservedInstances (Prelude.Maybe Prelude.Int)
describeReservedInstances_maxResults = Lens.lens (\DescribeReservedInstances' {maxResults} -> maxResults) (\s@DescribeReservedInstances' {} a -> s {maxResults = a} :: DescribeReservedInstances)

instance Core.AWSRequest DescribeReservedInstances where
  type
    AWSResponse DescribeReservedInstances =
      DescribeReservedInstancesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReservedInstancesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ReservedInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReservedInstances where
  hashWithSalt salt' DescribeReservedInstances' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` reservedInstanceId
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeReservedInstances where
  rnf DescribeReservedInstances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf reservedInstanceId

instance Core.ToHeaders DescribeReservedInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeReservedInstances where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/reservedInstances"

instance Core.ToQuery DescribeReservedInstances where
  toQuery DescribeReservedInstances' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "reservationId" Core.=: reservedInstanceId,
        "maxResults" Core.=: maxResults
      ]

-- | Container for results from @DescribeReservedInstances@
--
-- /See:/ 'newDescribeReservedInstancesResponse' smart constructor.
data DescribeReservedInstancesResponse = DescribeReservedInstancesResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of reserved OpenSearch instances.
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
-- 'nextToken', 'describeReservedInstancesResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'reservedInstances', 'describeReservedInstancesResponse_reservedInstances' - List of reserved OpenSearch instances.
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

-- | Provides an identifier to allow retrieval of paginated results.
describeReservedInstancesResponse_nextToken :: Lens.Lens' DescribeReservedInstancesResponse (Prelude.Maybe Prelude.Text)
describeReservedInstancesResponse_nextToken = Lens.lens (\DescribeReservedInstancesResponse' {nextToken} -> nextToken) (\s@DescribeReservedInstancesResponse' {} a -> s {nextToken = a} :: DescribeReservedInstancesResponse)

-- | List of reserved OpenSearch instances.
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
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf reservedInstances
