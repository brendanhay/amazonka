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
-- Module      : Amazonka.EC2.GetIpamPoolCidrs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the CIDRs provisioned to an IPAM pool.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetIpamPoolCidrs
  ( -- * Creating a Request
    GetIpamPoolCidrs (..),
    newGetIpamPoolCidrs,

    -- * Request Lenses
    getIpamPoolCidrs_nextToken,
    getIpamPoolCidrs_filters,
    getIpamPoolCidrs_dryRun,
    getIpamPoolCidrs_maxResults,
    getIpamPoolCidrs_ipamPoolId,

    -- * Destructuring the Response
    GetIpamPoolCidrsResponse (..),
    newGetIpamPoolCidrsResponse,

    -- * Response Lenses
    getIpamPoolCidrsResponse_nextToken,
    getIpamPoolCidrsResponse_ipamPoolCidrs,
    getIpamPoolCidrsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIpamPoolCidrs' smart constructor.
data GetIpamPoolCidrs = GetIpamPoolCidrs'
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
    -- | The maximum number of results to return in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the IPAM pool you want the CIDR for.
    ipamPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamPoolCidrs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIpamPoolCidrs_nextToken' - The token for the next page of results.
--
-- 'filters', 'getIpamPoolCidrs_filters' - One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
--
-- 'dryRun', 'getIpamPoolCidrs_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getIpamPoolCidrs_maxResults' - The maximum number of results to return in the request.
--
-- 'ipamPoolId', 'getIpamPoolCidrs_ipamPoolId' - The ID of the IPAM pool you want the CIDR for.
newGetIpamPoolCidrs ::
  -- | 'ipamPoolId'
  Prelude.Text ->
  GetIpamPoolCidrs
newGetIpamPoolCidrs pIpamPoolId_ =
  GetIpamPoolCidrs'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      ipamPoolId = pIpamPoolId_
    }

-- | The token for the next page of results.
getIpamPoolCidrs_nextToken :: Lens.Lens' GetIpamPoolCidrs (Prelude.Maybe Prelude.Text)
getIpamPoolCidrs_nextToken = Lens.lens (\GetIpamPoolCidrs' {nextToken} -> nextToken) (\s@GetIpamPoolCidrs' {} a -> s {nextToken = a} :: GetIpamPoolCidrs)

-- | One or more filters for the request. For more information about
-- filtering, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-filter.html Filtering CLI output>.
getIpamPoolCidrs_filters :: Lens.Lens' GetIpamPoolCidrs (Prelude.Maybe [Filter])
getIpamPoolCidrs_filters = Lens.lens (\GetIpamPoolCidrs' {filters} -> filters) (\s@GetIpamPoolCidrs' {} a -> s {filters = a} :: GetIpamPoolCidrs) Prelude.. Lens.mapping Lens.coerced

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
getIpamPoolCidrs_dryRun :: Lens.Lens' GetIpamPoolCidrs (Prelude.Maybe Prelude.Bool)
getIpamPoolCidrs_dryRun = Lens.lens (\GetIpamPoolCidrs' {dryRun} -> dryRun) (\s@GetIpamPoolCidrs' {} a -> s {dryRun = a} :: GetIpamPoolCidrs)

-- | The maximum number of results to return in the request.
getIpamPoolCidrs_maxResults :: Lens.Lens' GetIpamPoolCidrs (Prelude.Maybe Prelude.Natural)
getIpamPoolCidrs_maxResults = Lens.lens (\GetIpamPoolCidrs' {maxResults} -> maxResults) (\s@GetIpamPoolCidrs' {} a -> s {maxResults = a} :: GetIpamPoolCidrs)

-- | The ID of the IPAM pool you want the CIDR for.
getIpamPoolCidrs_ipamPoolId :: Lens.Lens' GetIpamPoolCidrs Prelude.Text
getIpamPoolCidrs_ipamPoolId = Lens.lens (\GetIpamPoolCidrs' {ipamPoolId} -> ipamPoolId) (\s@GetIpamPoolCidrs' {} a -> s {ipamPoolId = a} :: GetIpamPoolCidrs)

instance Core.AWSPager GetIpamPoolCidrs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIpamPoolCidrsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIpamPoolCidrsResponse_ipamPoolCidrs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getIpamPoolCidrs_nextToken
          Lens..~ rs
          Lens.^? getIpamPoolCidrsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetIpamPoolCidrs where
  type
    AWSResponse GetIpamPoolCidrs =
      GetIpamPoolCidrsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetIpamPoolCidrsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "ipamPoolCidrSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIpamPoolCidrs where
  hashWithSalt _salt GetIpamPoolCidrs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` ipamPoolId

instance Prelude.NFData GetIpamPoolCidrs where
  rnf GetIpamPoolCidrs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf ipamPoolId

instance Data.ToHeaders GetIpamPoolCidrs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetIpamPoolCidrs where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIpamPoolCidrs where
  toQuery GetIpamPoolCidrs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetIpamPoolCidrs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "IpamPoolId" Data.=: ipamPoolId
      ]

-- | /See:/ 'newGetIpamPoolCidrsResponse' smart constructor.
data GetIpamPoolCidrsResponse = GetIpamPoolCidrsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the CIDRs provisioned to an IPAM pool.
    ipamPoolCidrs :: Prelude.Maybe [IpamPoolCidr],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamPoolCidrsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getIpamPoolCidrsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'ipamPoolCidrs', 'getIpamPoolCidrsResponse_ipamPoolCidrs' - Information about the CIDRs provisioned to an IPAM pool.
--
-- 'httpStatus', 'getIpamPoolCidrsResponse_httpStatus' - The response's http status code.
newGetIpamPoolCidrsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpamPoolCidrsResponse
newGetIpamPoolCidrsResponse pHttpStatus_ =
  GetIpamPoolCidrsResponse'
    { nextToken =
        Prelude.Nothing,
      ipamPoolCidrs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getIpamPoolCidrsResponse_nextToken :: Lens.Lens' GetIpamPoolCidrsResponse (Prelude.Maybe Prelude.Text)
getIpamPoolCidrsResponse_nextToken = Lens.lens (\GetIpamPoolCidrsResponse' {nextToken} -> nextToken) (\s@GetIpamPoolCidrsResponse' {} a -> s {nextToken = a} :: GetIpamPoolCidrsResponse)

-- | Information about the CIDRs provisioned to an IPAM pool.
getIpamPoolCidrsResponse_ipamPoolCidrs :: Lens.Lens' GetIpamPoolCidrsResponse (Prelude.Maybe [IpamPoolCidr])
getIpamPoolCidrsResponse_ipamPoolCidrs = Lens.lens (\GetIpamPoolCidrsResponse' {ipamPoolCidrs} -> ipamPoolCidrs) (\s@GetIpamPoolCidrsResponse' {} a -> s {ipamPoolCidrs = a} :: GetIpamPoolCidrsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getIpamPoolCidrsResponse_httpStatus :: Lens.Lens' GetIpamPoolCidrsResponse Prelude.Int
getIpamPoolCidrsResponse_httpStatus = Lens.lens (\GetIpamPoolCidrsResponse' {httpStatus} -> httpStatus) (\s@GetIpamPoolCidrsResponse' {} a -> s {httpStatus = a} :: GetIpamPoolCidrsResponse)

instance Prelude.NFData GetIpamPoolCidrsResponse where
  rnf GetIpamPoolCidrsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ipamPoolCidrs
      `Prelude.seq` Prelude.rnf httpStatus
