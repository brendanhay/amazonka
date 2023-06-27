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
-- Module      : Amazonka.EC2.GetIpamDiscoveredResourceCidrs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource CIDRs that are monitored as part of a resource
-- discovery. A discovered resource is a resource CIDR monitored under a
-- resource discovery. The following resources can be discovered: VPCs,
-- Public IPv4 pools, VPC subnets, and Elastic IP addresses.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetIpamDiscoveredResourceCidrs
  ( -- * Creating a Request
    GetIpamDiscoveredResourceCidrs (..),
    newGetIpamDiscoveredResourceCidrs,

    -- * Request Lenses
    getIpamDiscoveredResourceCidrs_dryRun,
    getIpamDiscoveredResourceCidrs_filters,
    getIpamDiscoveredResourceCidrs_maxResults,
    getIpamDiscoveredResourceCidrs_nextToken,
    getIpamDiscoveredResourceCidrs_ipamResourceDiscoveryId,
    getIpamDiscoveredResourceCidrs_resourceRegion,

    -- * Destructuring the Response
    GetIpamDiscoveredResourceCidrsResponse (..),
    newGetIpamDiscoveredResourceCidrsResponse,

    -- * Response Lenses
    getIpamDiscoveredResourceCidrsResponse_ipamDiscoveredResourceCidrs,
    getIpamDiscoveredResourceCidrsResponse_nextToken,
    getIpamDiscoveredResourceCidrsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIpamDiscoveredResourceCidrs' smart constructor.
data GetIpamDiscoveredResourceCidrs = GetIpamDiscoveredResourceCidrs'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Filters.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of discovered resource CIDRs to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Text,
    -- | A resource Region.
    resourceRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamDiscoveredResourceCidrs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getIpamDiscoveredResourceCidrs_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'getIpamDiscoveredResourceCidrs_filters' - Filters.
--
-- 'maxResults', 'getIpamDiscoveredResourceCidrs_maxResults' - The maximum number of discovered resource CIDRs to return in one page of
-- results.
--
-- 'nextToken', 'getIpamDiscoveredResourceCidrs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'ipamResourceDiscoveryId', 'getIpamDiscoveredResourceCidrs_ipamResourceDiscoveryId' - A resource discovery ID.
--
-- 'resourceRegion', 'getIpamDiscoveredResourceCidrs_resourceRegion' - A resource Region.
newGetIpamDiscoveredResourceCidrs ::
  -- | 'ipamResourceDiscoveryId'
  Prelude.Text ->
  -- | 'resourceRegion'
  Prelude.Text ->
  GetIpamDiscoveredResourceCidrs
newGetIpamDiscoveredResourceCidrs
  pIpamResourceDiscoveryId_
  pResourceRegion_ =
    GetIpamDiscoveredResourceCidrs'
      { dryRun =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        ipamResourceDiscoveryId =
          pIpamResourceDiscoveryId_,
        resourceRegion = pResourceRegion_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
getIpamDiscoveredResourceCidrs_dryRun :: Lens.Lens' GetIpamDiscoveredResourceCidrs (Prelude.Maybe Prelude.Bool)
getIpamDiscoveredResourceCidrs_dryRun = Lens.lens (\GetIpamDiscoveredResourceCidrs' {dryRun} -> dryRun) (\s@GetIpamDiscoveredResourceCidrs' {} a -> s {dryRun = a} :: GetIpamDiscoveredResourceCidrs)

-- | Filters.
getIpamDiscoveredResourceCidrs_filters :: Lens.Lens' GetIpamDiscoveredResourceCidrs (Prelude.Maybe [Filter])
getIpamDiscoveredResourceCidrs_filters = Lens.lens (\GetIpamDiscoveredResourceCidrs' {filters} -> filters) (\s@GetIpamDiscoveredResourceCidrs' {} a -> s {filters = a} :: GetIpamDiscoveredResourceCidrs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of discovered resource CIDRs to return in one page of
-- results.
getIpamDiscoveredResourceCidrs_maxResults :: Lens.Lens' GetIpamDiscoveredResourceCidrs (Prelude.Maybe Prelude.Natural)
getIpamDiscoveredResourceCidrs_maxResults = Lens.lens (\GetIpamDiscoveredResourceCidrs' {maxResults} -> maxResults) (\s@GetIpamDiscoveredResourceCidrs' {} a -> s {maxResults = a} :: GetIpamDiscoveredResourceCidrs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
getIpamDiscoveredResourceCidrs_nextToken :: Lens.Lens' GetIpamDiscoveredResourceCidrs (Prelude.Maybe Prelude.Text)
getIpamDiscoveredResourceCidrs_nextToken = Lens.lens (\GetIpamDiscoveredResourceCidrs' {nextToken} -> nextToken) (\s@GetIpamDiscoveredResourceCidrs' {} a -> s {nextToken = a} :: GetIpamDiscoveredResourceCidrs)

-- | A resource discovery ID.
getIpamDiscoveredResourceCidrs_ipamResourceDiscoveryId :: Lens.Lens' GetIpamDiscoveredResourceCidrs Prelude.Text
getIpamDiscoveredResourceCidrs_ipamResourceDiscoveryId = Lens.lens (\GetIpamDiscoveredResourceCidrs' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@GetIpamDiscoveredResourceCidrs' {} a -> s {ipamResourceDiscoveryId = a} :: GetIpamDiscoveredResourceCidrs)

-- | A resource Region.
getIpamDiscoveredResourceCidrs_resourceRegion :: Lens.Lens' GetIpamDiscoveredResourceCidrs Prelude.Text
getIpamDiscoveredResourceCidrs_resourceRegion = Lens.lens (\GetIpamDiscoveredResourceCidrs' {resourceRegion} -> resourceRegion) (\s@GetIpamDiscoveredResourceCidrs' {} a -> s {resourceRegion = a} :: GetIpamDiscoveredResourceCidrs)

instance Core.AWSPager GetIpamDiscoveredResourceCidrs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIpamDiscoveredResourceCidrsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIpamDiscoveredResourceCidrsResponse_ipamDiscoveredResourceCidrs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getIpamDiscoveredResourceCidrs_nextToken
          Lens..~ rs
          Lens.^? getIpamDiscoveredResourceCidrsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetIpamDiscoveredResourceCidrs
  where
  type
    AWSResponse GetIpamDiscoveredResourceCidrs =
      GetIpamDiscoveredResourceCidrsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetIpamDiscoveredResourceCidrsResponse'
            Prelude.<$> ( x
                            Data..@? "ipamDiscoveredResourceCidrSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetIpamDiscoveredResourceCidrs
  where
  hashWithSalt
    _salt
    GetIpamDiscoveredResourceCidrs' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` ipamResourceDiscoveryId
        `Prelude.hashWithSalt` resourceRegion

instance
  Prelude.NFData
    GetIpamDiscoveredResourceCidrs
  where
  rnf GetIpamDiscoveredResourceCidrs' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId
      `Prelude.seq` Prelude.rnf resourceRegion

instance
  Data.ToHeaders
    GetIpamDiscoveredResourceCidrs
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetIpamDiscoveredResourceCidrs where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIpamDiscoveredResourceCidrs where
  toQuery GetIpamDiscoveredResourceCidrs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetIpamDiscoveredResourceCidrs" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "IpamResourceDiscoveryId"
          Data.=: ipamResourceDiscoveryId,
        "ResourceRegion" Data.=: resourceRegion
      ]

-- | /See:/ 'newGetIpamDiscoveredResourceCidrsResponse' smart constructor.
data GetIpamDiscoveredResourceCidrsResponse = GetIpamDiscoveredResourceCidrsResponse'
  { -- | Discovered resource CIDRs.
    ipamDiscoveredResourceCidrs :: Prelude.Maybe [IpamDiscoveredResourceCidr],
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamDiscoveredResourceCidrsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamDiscoveredResourceCidrs', 'getIpamDiscoveredResourceCidrsResponse_ipamDiscoveredResourceCidrs' - Discovered resource CIDRs.
--
-- 'nextToken', 'getIpamDiscoveredResourceCidrsResponse_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'httpStatus', 'getIpamDiscoveredResourceCidrsResponse_httpStatus' - The response's http status code.
newGetIpamDiscoveredResourceCidrsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpamDiscoveredResourceCidrsResponse
newGetIpamDiscoveredResourceCidrsResponse
  pHttpStatus_ =
    GetIpamDiscoveredResourceCidrsResponse'
      { ipamDiscoveredResourceCidrs =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Discovered resource CIDRs.
getIpamDiscoveredResourceCidrsResponse_ipamDiscoveredResourceCidrs :: Lens.Lens' GetIpamDiscoveredResourceCidrsResponse (Prelude.Maybe [IpamDiscoveredResourceCidr])
getIpamDiscoveredResourceCidrsResponse_ipamDiscoveredResourceCidrs = Lens.lens (\GetIpamDiscoveredResourceCidrsResponse' {ipamDiscoveredResourceCidrs} -> ipamDiscoveredResourceCidrs) (\s@GetIpamDiscoveredResourceCidrsResponse' {} a -> s {ipamDiscoveredResourceCidrs = a} :: GetIpamDiscoveredResourceCidrsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
getIpamDiscoveredResourceCidrsResponse_nextToken :: Lens.Lens' GetIpamDiscoveredResourceCidrsResponse (Prelude.Maybe Prelude.Text)
getIpamDiscoveredResourceCidrsResponse_nextToken = Lens.lens (\GetIpamDiscoveredResourceCidrsResponse' {nextToken} -> nextToken) (\s@GetIpamDiscoveredResourceCidrsResponse' {} a -> s {nextToken = a} :: GetIpamDiscoveredResourceCidrsResponse)

-- | The response's http status code.
getIpamDiscoveredResourceCidrsResponse_httpStatus :: Lens.Lens' GetIpamDiscoveredResourceCidrsResponse Prelude.Int
getIpamDiscoveredResourceCidrsResponse_httpStatus = Lens.lens (\GetIpamDiscoveredResourceCidrsResponse' {httpStatus} -> httpStatus) (\s@GetIpamDiscoveredResourceCidrsResponse' {} a -> s {httpStatus = a} :: GetIpamDiscoveredResourceCidrsResponse)

instance
  Prelude.NFData
    GetIpamDiscoveredResourceCidrsResponse
  where
  rnf GetIpamDiscoveredResourceCidrsResponse' {..} =
    Prelude.rnf ipamDiscoveredResourceCidrs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
