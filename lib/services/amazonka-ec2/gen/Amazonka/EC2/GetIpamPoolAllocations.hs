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
-- Module      : Amazonka.EC2.GetIpamPoolAllocations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of all the CIDR allocations in an IPAM pool.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetIpamPoolAllocations
  ( -- * Creating a Request
    GetIpamPoolAllocations (..),
    newGetIpamPoolAllocations,

    -- * Request Lenses
    getIpamPoolAllocations_nextToken,
    getIpamPoolAllocations_filters,
    getIpamPoolAllocations_dryRun,
    getIpamPoolAllocations_maxResults,
    getIpamPoolAllocations_ipamPoolAllocationId,
    getIpamPoolAllocations_ipamPoolId,

    -- * Destructuring the Response
    GetIpamPoolAllocationsResponse (..),
    newGetIpamPoolAllocationsResponse,

    -- * Response Lenses
    getIpamPoolAllocationsResponse_nextToken,
    getIpamPoolAllocationsResponse_ipamPoolAllocations,
    getIpamPoolAllocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIpamPoolAllocations' smart constructor.
data GetIpamPoolAllocations = GetIpamPoolAllocations'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters for the request. For more information about
    -- filtering, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
    filters :: Prelude.Maybe [Filter],
    -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results you would like returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the allocation.
    ipamPoolAllocationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the IPAM pool you want to see the allocations for.
    ipamPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamPoolAllocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIpamPoolAllocations_nextToken' - The token for the next page of results.
--
-- 'filters', 'getIpamPoolAllocations_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'dryRun', 'getIpamPoolAllocations_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getIpamPoolAllocations_maxResults' - The maximum number of results you would like returned per page.
--
-- 'ipamPoolAllocationId', 'getIpamPoolAllocations_ipamPoolAllocationId' - The ID of the allocation.
--
-- 'ipamPoolId', 'getIpamPoolAllocations_ipamPoolId' - The ID of the IPAM pool you want to see the allocations for.
newGetIpamPoolAllocations ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  GetIpamPoolAllocations
newGetIpamPoolAllocations pIpamPoolId_ =
  GetIpamPoolAllocations'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      ipamPoolAllocationId = Prelude.Nothing,
      ipamPoolId = pIpamPoolId_
    }

-- | The token for the next page of results.
getIpamPoolAllocations_nextToken :: Lens.Lens' GetIpamPoolAllocations (Prelude.Maybe Prelude.Text)
getIpamPoolAllocations_nextToken = Lens.lens (\GetIpamPoolAllocations' {nextToken} -> nextToken) (\s@GetIpamPoolAllocations' {} a -> s {nextToken = a} :: GetIpamPoolAllocations)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
getIpamPoolAllocations_filters :: Lens.Lens' GetIpamPoolAllocations (Prelude.Maybe [Filter])
getIpamPoolAllocations_filters = Lens.lens (\GetIpamPoolAllocations' {filters} -> filters) (\s@GetIpamPoolAllocations' {} a -> s {filters = a} :: GetIpamPoolAllocations) Prelude.. Lens.mapping Lens.coerced

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
getIpamPoolAllocations_dryRun :: Lens.Lens' GetIpamPoolAllocations (Prelude.Maybe Prelude.Bool)
getIpamPoolAllocations_dryRun = Lens.lens (\GetIpamPoolAllocations' {dryRun} -> dryRun) (\s@GetIpamPoolAllocations' {} a -> s {dryRun = a} :: GetIpamPoolAllocations)

-- | The maximum number of results you would like returned per page.
getIpamPoolAllocations_maxResults :: Lens.Lens' GetIpamPoolAllocations (Prelude.Maybe Prelude.Natural)
getIpamPoolAllocations_maxResults = Lens.lens (\GetIpamPoolAllocations' {maxResults} -> maxResults) (\s@GetIpamPoolAllocations' {} a -> s {maxResults = a} :: GetIpamPoolAllocations)

-- | The ID of the allocation.
getIpamPoolAllocations_ipamPoolAllocationId :: Lens.Lens' GetIpamPoolAllocations (Prelude.Maybe Prelude.Text)
getIpamPoolAllocations_ipamPoolAllocationId = Lens.lens (\GetIpamPoolAllocations' {ipamPoolAllocationId} -> ipamPoolAllocationId) (\s@GetIpamPoolAllocations' {} a -> s {ipamPoolAllocationId = a} :: GetIpamPoolAllocations)

-- | The ID of the IPAM pool you want to see the allocations for.
getIpamPoolAllocations_ipamPoolId :: Lens.Lens' GetIpamPoolAllocations Prelude.Text
getIpamPoolAllocations_ipamPoolId = Lens.lens (\GetIpamPoolAllocations' {ipamPoolId} -> ipamPoolId) (\s@GetIpamPoolAllocations' {} a -> s {ipamPoolId = a} :: GetIpamPoolAllocations)

instance Core.AWSPager GetIpamPoolAllocations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIpamPoolAllocationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIpamPoolAllocationsResponse_ipamPoolAllocations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getIpamPoolAllocations_nextToken
          Lens..~ rs
          Lens.^? getIpamPoolAllocationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetIpamPoolAllocations where
  type
    AWSResponse GetIpamPoolAllocations =
      GetIpamPoolAllocationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetIpamPoolAllocationsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "ipamPoolAllocationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIpamPoolAllocations where
  hashWithSalt _salt GetIpamPoolAllocations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` ipamPoolAllocationId
      `Prelude.hashWithSalt` ipamPoolId

instance Prelude.NFData GetIpamPoolAllocations where
  rnf GetIpamPoolAllocations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf ipamPoolAllocationId
      `Prelude.seq` Prelude.rnf ipamPoolId

instance Data.ToHeaders GetIpamPoolAllocations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetIpamPoolAllocations where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIpamPoolAllocations where
  toQuery GetIpamPoolAllocations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetIpamPoolAllocations" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "IpamPoolAllocationId" Data.=: ipamPoolAllocationId,
        "IpamPoolId" Data.=: ipamPoolId
      ]

-- | /See:/ 'newGetIpamPoolAllocationsResponse' smart constructor.
data GetIpamPoolAllocationsResponse = GetIpamPoolAllocationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IPAM pool allocations you want information on.
    ipamPoolAllocations :: Prelude.Maybe [IpamPoolAllocation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamPoolAllocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIpamPoolAllocationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'ipamPoolAllocations', 'getIpamPoolAllocationsResponse_ipamPoolAllocations' - The IPAM pool allocations you want information on.
--
-- 'httpStatus', 'getIpamPoolAllocationsResponse_httpStatus' - The response's http status code.
newGetIpamPoolAllocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpamPoolAllocationsResponse
newGetIpamPoolAllocationsResponse pHttpStatus_ =
  GetIpamPoolAllocationsResponse'
    { nextToken =
        Prelude.Nothing,
      ipamPoolAllocations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getIpamPoolAllocationsResponse_nextToken :: Lens.Lens' GetIpamPoolAllocationsResponse (Prelude.Maybe Prelude.Text)
getIpamPoolAllocationsResponse_nextToken = Lens.lens (\GetIpamPoolAllocationsResponse' {nextToken} -> nextToken) (\s@GetIpamPoolAllocationsResponse' {} a -> s {nextToken = a} :: GetIpamPoolAllocationsResponse)

-- | The IPAM pool allocations you want information on.
getIpamPoolAllocationsResponse_ipamPoolAllocations :: Lens.Lens' GetIpamPoolAllocationsResponse (Prelude.Maybe [IpamPoolAllocation])
getIpamPoolAllocationsResponse_ipamPoolAllocations = Lens.lens (\GetIpamPoolAllocationsResponse' {ipamPoolAllocations} -> ipamPoolAllocations) (\s@GetIpamPoolAllocationsResponse' {} a -> s {ipamPoolAllocations = a} :: GetIpamPoolAllocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getIpamPoolAllocationsResponse_httpStatus :: Lens.Lens' GetIpamPoolAllocationsResponse Prelude.Int
getIpamPoolAllocationsResponse_httpStatus = Lens.lens (\GetIpamPoolAllocationsResponse' {httpStatus} -> httpStatus) (\s@GetIpamPoolAllocationsResponse' {} a -> s {httpStatus = a} :: GetIpamPoolAllocationsResponse)

instance
  Prelude.NFData
    GetIpamPoolAllocationsResponse
  where
  rnf GetIpamPoolAllocationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ipamPoolAllocations
      `Prelude.seq` Prelude.rnf httpStatus
