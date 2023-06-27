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
-- Module      : Amazonka.EC2.DescribeIpamResourceDiscoveries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes IPAM resource discoveries. A resource discovery is an IPAM
-- component that enables IPAM to manage and monitor resources that belong
-- to the owning account.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeIpamResourceDiscoveries
  ( -- * Creating a Request
    DescribeIpamResourceDiscoveries (..),
    newDescribeIpamResourceDiscoveries,

    -- * Request Lenses
    describeIpamResourceDiscoveries_dryRun,
    describeIpamResourceDiscoveries_filters,
    describeIpamResourceDiscoveries_ipamResourceDiscoveryIds,
    describeIpamResourceDiscoveries_maxResults,
    describeIpamResourceDiscoveries_nextToken,

    -- * Destructuring the Response
    DescribeIpamResourceDiscoveriesResponse (..),
    newDescribeIpamResourceDiscoveriesResponse,

    -- * Response Lenses
    describeIpamResourceDiscoveriesResponse_ipamResourceDiscoveries,
    describeIpamResourceDiscoveriesResponse_nextToken,
    describeIpamResourceDiscoveriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIpamResourceDiscoveries' smart constructor.
data DescribeIpamResourceDiscoveries = DescribeIpamResourceDiscoveries'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The resource discovery filters.
    filters :: Prelude.Maybe [Filter],
    -- | The IPAM resource discovery IDs.
    ipamResourceDiscoveryIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of resource discoveries to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamResourceDiscoveries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeIpamResourceDiscoveries_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeIpamResourceDiscoveries_filters' - The resource discovery filters.
--
-- 'ipamResourceDiscoveryIds', 'describeIpamResourceDiscoveries_ipamResourceDiscoveryIds' - The IPAM resource discovery IDs.
--
-- 'maxResults', 'describeIpamResourceDiscoveries_maxResults' - The maximum number of resource discoveries to return in one page of
-- results.
--
-- 'nextToken', 'describeIpamResourceDiscoveries_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newDescribeIpamResourceDiscoveries ::
  DescribeIpamResourceDiscoveries
newDescribeIpamResourceDiscoveries =
  DescribeIpamResourceDiscoveries'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      ipamResourceDiscoveryIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
describeIpamResourceDiscoveries_dryRun :: Lens.Lens' DescribeIpamResourceDiscoveries (Prelude.Maybe Prelude.Bool)
describeIpamResourceDiscoveries_dryRun = Lens.lens (\DescribeIpamResourceDiscoveries' {dryRun} -> dryRun) (\s@DescribeIpamResourceDiscoveries' {} a -> s {dryRun = a} :: DescribeIpamResourceDiscoveries)

-- | The resource discovery filters.
describeIpamResourceDiscoveries_filters :: Lens.Lens' DescribeIpamResourceDiscoveries (Prelude.Maybe [Filter])
describeIpamResourceDiscoveries_filters = Lens.lens (\DescribeIpamResourceDiscoveries' {filters} -> filters) (\s@DescribeIpamResourceDiscoveries' {} a -> s {filters = a} :: DescribeIpamResourceDiscoveries) Prelude.. Lens.mapping Lens.coerced

-- | The IPAM resource discovery IDs.
describeIpamResourceDiscoveries_ipamResourceDiscoveryIds :: Lens.Lens' DescribeIpamResourceDiscoveries (Prelude.Maybe [Prelude.Text])
describeIpamResourceDiscoveries_ipamResourceDiscoveryIds = Lens.lens (\DescribeIpamResourceDiscoveries' {ipamResourceDiscoveryIds} -> ipamResourceDiscoveryIds) (\s@DescribeIpamResourceDiscoveries' {} a -> s {ipamResourceDiscoveryIds = a} :: DescribeIpamResourceDiscoveries) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of resource discoveries to return in one page of
-- results.
describeIpamResourceDiscoveries_maxResults :: Lens.Lens' DescribeIpamResourceDiscoveries (Prelude.Maybe Prelude.Natural)
describeIpamResourceDiscoveries_maxResults = Lens.lens (\DescribeIpamResourceDiscoveries' {maxResults} -> maxResults) (\s@DescribeIpamResourceDiscoveries' {} a -> s {maxResults = a} :: DescribeIpamResourceDiscoveries)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
describeIpamResourceDiscoveries_nextToken :: Lens.Lens' DescribeIpamResourceDiscoveries (Prelude.Maybe Prelude.Text)
describeIpamResourceDiscoveries_nextToken = Lens.lens (\DescribeIpamResourceDiscoveries' {nextToken} -> nextToken) (\s@DescribeIpamResourceDiscoveries' {} a -> s {nextToken = a} :: DescribeIpamResourceDiscoveries)

instance
  Core.AWSPager
    DescribeIpamResourceDiscoveries
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeIpamResourceDiscoveriesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeIpamResourceDiscoveriesResponse_ipamResourceDiscoveries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeIpamResourceDiscoveries_nextToken
          Lens..~ rs
          Lens.^? describeIpamResourceDiscoveriesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeIpamResourceDiscoveries
  where
  type
    AWSResponse DescribeIpamResourceDiscoveries =
      DescribeIpamResourceDiscoveriesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeIpamResourceDiscoveriesResponse'
            Prelude.<$> ( x
                            Data..@? "ipamResourceDiscoverySet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeIpamResourceDiscoveries
  where
  hashWithSalt
    _salt
    DescribeIpamResourceDiscoveries' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` ipamResourceDiscoveryIds
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeIpamResourceDiscoveries
  where
  rnf DescribeIpamResourceDiscoveries' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    DescribeIpamResourceDiscoveries
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIpamResourceDiscoveries where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIpamResourceDiscoveries where
  toQuery DescribeIpamResourceDiscoveries' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeIpamResourceDiscoveries" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          ( Data.toQueryList "IpamResourceDiscoveryId"
              Prelude.<$> ipamResourceDiscoveryIds
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeIpamResourceDiscoveriesResponse' smart constructor.
data DescribeIpamResourceDiscoveriesResponse = DescribeIpamResourceDiscoveriesResponse'
  { -- | The resource discoveries.
    ipamResourceDiscoveries :: Prelude.Maybe [IpamResourceDiscovery],
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIpamResourceDiscoveriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamResourceDiscoveries', 'describeIpamResourceDiscoveriesResponse_ipamResourceDiscoveries' - The resource discoveries.
--
-- 'nextToken', 'describeIpamResourceDiscoveriesResponse_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'httpStatus', 'describeIpamResourceDiscoveriesResponse_httpStatus' - The response's http status code.
newDescribeIpamResourceDiscoveriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIpamResourceDiscoveriesResponse
newDescribeIpamResourceDiscoveriesResponse
  pHttpStatus_ =
    DescribeIpamResourceDiscoveriesResponse'
      { ipamResourceDiscoveries =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The resource discoveries.
describeIpamResourceDiscoveriesResponse_ipamResourceDiscoveries :: Lens.Lens' DescribeIpamResourceDiscoveriesResponse (Prelude.Maybe [IpamResourceDiscovery])
describeIpamResourceDiscoveriesResponse_ipamResourceDiscoveries = Lens.lens (\DescribeIpamResourceDiscoveriesResponse' {ipamResourceDiscoveries} -> ipamResourceDiscoveries) (\s@DescribeIpamResourceDiscoveriesResponse' {} a -> s {ipamResourceDiscoveries = a} :: DescribeIpamResourceDiscoveriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
describeIpamResourceDiscoveriesResponse_nextToken :: Lens.Lens' DescribeIpamResourceDiscoveriesResponse (Prelude.Maybe Prelude.Text)
describeIpamResourceDiscoveriesResponse_nextToken = Lens.lens (\DescribeIpamResourceDiscoveriesResponse' {nextToken} -> nextToken) (\s@DescribeIpamResourceDiscoveriesResponse' {} a -> s {nextToken = a} :: DescribeIpamResourceDiscoveriesResponse)

-- | The response's http status code.
describeIpamResourceDiscoveriesResponse_httpStatus :: Lens.Lens' DescribeIpamResourceDiscoveriesResponse Prelude.Int
describeIpamResourceDiscoveriesResponse_httpStatus = Lens.lens (\DescribeIpamResourceDiscoveriesResponse' {httpStatus} -> httpStatus) (\s@DescribeIpamResourceDiscoveriesResponse' {} a -> s {httpStatus = a} :: DescribeIpamResourceDiscoveriesResponse)

instance
  Prelude.NFData
    DescribeIpamResourceDiscoveriesResponse
  where
  rnf DescribeIpamResourceDiscoveriesResponse' {..} =
    Prelude.rnf ipamResourceDiscoveries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
