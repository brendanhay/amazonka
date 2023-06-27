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
-- Module      : Amazonka.EC2.GetIpamDiscoveredAccounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets IPAM discovered accounts. A discovered account is an Amazon Web
-- Services account that is monitored under a resource discovery. If you
-- have integrated IPAM with Amazon Web Services Organizations, all
-- accounts in the organization are discovered accounts. Only the IPAM
-- account can get all discovered accounts in the organization.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetIpamDiscoveredAccounts
  ( -- * Creating a Request
    GetIpamDiscoveredAccounts (..),
    newGetIpamDiscoveredAccounts,

    -- * Request Lenses
    getIpamDiscoveredAccounts_dryRun,
    getIpamDiscoveredAccounts_filters,
    getIpamDiscoveredAccounts_maxResults,
    getIpamDiscoveredAccounts_nextToken,
    getIpamDiscoveredAccounts_ipamResourceDiscoveryId,
    getIpamDiscoveredAccounts_discoveryRegion,

    -- * Destructuring the Response
    GetIpamDiscoveredAccountsResponse (..),
    newGetIpamDiscoveredAccountsResponse,

    -- * Response Lenses
    getIpamDiscoveredAccountsResponse_ipamDiscoveredAccounts,
    getIpamDiscoveredAccountsResponse_nextToken,
    getIpamDiscoveredAccountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIpamDiscoveredAccounts' smart constructor.
data GetIpamDiscoveredAccounts = GetIpamDiscoveredAccounts'
  { -- | A check for whether you have the required permissions for the action
    -- without actually making the request and provides an error response. If
    -- you have the required permissions, the error response is
    -- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Discovered account filters.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of discovered accounts to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A resource discovery ID.
    ipamResourceDiscoveryId :: Prelude.Text,
    -- | The Amazon Web Services Region that the account information is returned
    -- from.
    discoveryRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamDiscoveredAccounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getIpamDiscoveredAccounts_dryRun' - A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'getIpamDiscoveredAccounts_filters' - Discovered account filters.
--
-- 'maxResults', 'getIpamDiscoveredAccounts_maxResults' - The maximum number of discovered accounts to return in one page of
-- results.
--
-- 'nextToken', 'getIpamDiscoveredAccounts_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'ipamResourceDiscoveryId', 'getIpamDiscoveredAccounts_ipamResourceDiscoveryId' - A resource discovery ID.
--
-- 'discoveryRegion', 'getIpamDiscoveredAccounts_discoveryRegion' - The Amazon Web Services Region that the account information is returned
-- from.
newGetIpamDiscoveredAccounts ::
  -- | 'ipamResourceDiscoveryId'
  Prelude.Text ->
  -- | 'discoveryRegion'
  Prelude.Text ->
  GetIpamDiscoveredAccounts
newGetIpamDiscoveredAccounts
  pIpamResourceDiscoveryId_
  pDiscoveryRegion_ =
    GetIpamDiscoveredAccounts'
      { dryRun =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        ipamResourceDiscoveryId =
          pIpamResourceDiscoveryId_,
        discoveryRegion = pDiscoveryRegion_
      }

-- | A check for whether you have the required permissions for the action
-- without actually making the request and provides an error response. If
-- you have the required permissions, the error response is
-- @DryRunOperation@. Otherwise, it is @UnauthorizedOperation@.
getIpamDiscoveredAccounts_dryRun :: Lens.Lens' GetIpamDiscoveredAccounts (Prelude.Maybe Prelude.Bool)
getIpamDiscoveredAccounts_dryRun = Lens.lens (\GetIpamDiscoveredAccounts' {dryRun} -> dryRun) (\s@GetIpamDiscoveredAccounts' {} a -> s {dryRun = a} :: GetIpamDiscoveredAccounts)

-- | Discovered account filters.
getIpamDiscoveredAccounts_filters :: Lens.Lens' GetIpamDiscoveredAccounts (Prelude.Maybe [Filter])
getIpamDiscoveredAccounts_filters = Lens.lens (\GetIpamDiscoveredAccounts' {filters} -> filters) (\s@GetIpamDiscoveredAccounts' {} a -> s {filters = a} :: GetIpamDiscoveredAccounts) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of discovered accounts to return in one page of
-- results.
getIpamDiscoveredAccounts_maxResults :: Lens.Lens' GetIpamDiscoveredAccounts (Prelude.Maybe Prelude.Natural)
getIpamDiscoveredAccounts_maxResults = Lens.lens (\GetIpamDiscoveredAccounts' {maxResults} -> maxResults) (\s@GetIpamDiscoveredAccounts' {} a -> s {maxResults = a} :: GetIpamDiscoveredAccounts)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
getIpamDiscoveredAccounts_nextToken :: Lens.Lens' GetIpamDiscoveredAccounts (Prelude.Maybe Prelude.Text)
getIpamDiscoveredAccounts_nextToken = Lens.lens (\GetIpamDiscoveredAccounts' {nextToken} -> nextToken) (\s@GetIpamDiscoveredAccounts' {} a -> s {nextToken = a} :: GetIpamDiscoveredAccounts)

-- | A resource discovery ID.
getIpamDiscoveredAccounts_ipamResourceDiscoveryId :: Lens.Lens' GetIpamDiscoveredAccounts Prelude.Text
getIpamDiscoveredAccounts_ipamResourceDiscoveryId = Lens.lens (\GetIpamDiscoveredAccounts' {ipamResourceDiscoveryId} -> ipamResourceDiscoveryId) (\s@GetIpamDiscoveredAccounts' {} a -> s {ipamResourceDiscoveryId = a} :: GetIpamDiscoveredAccounts)

-- | The Amazon Web Services Region that the account information is returned
-- from.
getIpamDiscoveredAccounts_discoveryRegion :: Lens.Lens' GetIpamDiscoveredAccounts Prelude.Text
getIpamDiscoveredAccounts_discoveryRegion = Lens.lens (\GetIpamDiscoveredAccounts' {discoveryRegion} -> discoveryRegion) (\s@GetIpamDiscoveredAccounts' {} a -> s {discoveryRegion = a} :: GetIpamDiscoveredAccounts)

instance Core.AWSPager GetIpamDiscoveredAccounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getIpamDiscoveredAccountsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getIpamDiscoveredAccountsResponse_ipamDiscoveredAccounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getIpamDiscoveredAccounts_nextToken
          Lens..~ rs
          Lens.^? getIpamDiscoveredAccountsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetIpamDiscoveredAccounts where
  type
    AWSResponse GetIpamDiscoveredAccounts =
      GetIpamDiscoveredAccountsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetIpamDiscoveredAccountsResponse'
            Prelude.<$> ( x
                            Data..@? "ipamDiscoveredAccountSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIpamDiscoveredAccounts where
  hashWithSalt _salt GetIpamDiscoveredAccounts' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` ipamResourceDiscoveryId
      `Prelude.hashWithSalt` discoveryRegion

instance Prelude.NFData GetIpamDiscoveredAccounts where
  rnf GetIpamDiscoveredAccounts' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ipamResourceDiscoveryId
      `Prelude.seq` Prelude.rnf discoveryRegion

instance Data.ToHeaders GetIpamDiscoveredAccounts where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetIpamDiscoveredAccounts where
  toPath = Prelude.const "/"

instance Data.ToQuery GetIpamDiscoveredAccounts where
  toQuery GetIpamDiscoveredAccounts' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetIpamDiscoveredAccounts" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "IpamResourceDiscoveryId"
          Data.=: ipamResourceDiscoveryId,
        "DiscoveryRegion" Data.=: discoveryRegion
      ]

-- | /See:/ 'newGetIpamDiscoveredAccountsResponse' smart constructor.
data GetIpamDiscoveredAccountsResponse = GetIpamDiscoveredAccountsResponse'
  { -- | Discovered accounts.
    ipamDiscoveredAccounts :: Prelude.Maybe [IpamDiscoveredAccount],
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpamDiscoveredAccountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipamDiscoveredAccounts', 'getIpamDiscoveredAccountsResponse_ipamDiscoveredAccounts' - Discovered accounts.
--
-- 'nextToken', 'getIpamDiscoveredAccountsResponse_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'httpStatus', 'getIpamDiscoveredAccountsResponse_httpStatus' - The response's http status code.
newGetIpamDiscoveredAccountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpamDiscoveredAccountsResponse
newGetIpamDiscoveredAccountsResponse pHttpStatus_ =
  GetIpamDiscoveredAccountsResponse'
    { ipamDiscoveredAccounts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Discovered accounts.
getIpamDiscoveredAccountsResponse_ipamDiscoveredAccounts :: Lens.Lens' GetIpamDiscoveredAccountsResponse (Prelude.Maybe [IpamDiscoveredAccount])
getIpamDiscoveredAccountsResponse_ipamDiscoveredAccounts = Lens.lens (\GetIpamDiscoveredAccountsResponse' {ipamDiscoveredAccounts} -> ipamDiscoveredAccounts) (\s@GetIpamDiscoveredAccountsResponse' {} a -> s {ipamDiscoveredAccounts = a} :: GetIpamDiscoveredAccountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
getIpamDiscoveredAccountsResponse_nextToken :: Lens.Lens' GetIpamDiscoveredAccountsResponse (Prelude.Maybe Prelude.Text)
getIpamDiscoveredAccountsResponse_nextToken = Lens.lens (\GetIpamDiscoveredAccountsResponse' {nextToken} -> nextToken) (\s@GetIpamDiscoveredAccountsResponse' {} a -> s {nextToken = a} :: GetIpamDiscoveredAccountsResponse)

-- | The response's http status code.
getIpamDiscoveredAccountsResponse_httpStatus :: Lens.Lens' GetIpamDiscoveredAccountsResponse Prelude.Int
getIpamDiscoveredAccountsResponse_httpStatus = Lens.lens (\GetIpamDiscoveredAccountsResponse' {httpStatus} -> httpStatus) (\s@GetIpamDiscoveredAccountsResponse' {} a -> s {httpStatus = a} :: GetIpamDiscoveredAccountsResponse)

instance
  Prelude.NFData
    GetIpamDiscoveredAccountsResponse
  where
  rnf GetIpamDiscoveredAccountsResponse' {..} =
    Prelude.rnf ipamDiscoveredAccounts
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
