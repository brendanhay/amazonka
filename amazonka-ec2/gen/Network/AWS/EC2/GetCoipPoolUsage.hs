{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.GetCoipPoolUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the allocations from the specified customer-owned address
-- pool.
module Network.AWS.EC2.GetCoipPoolUsage
  ( -- * Creating a Request
    GetCoipPoolUsage (..),
    newGetCoipPoolUsage,

    -- * Request Lenses
    getCoipPoolUsage_nextToken,
    getCoipPoolUsage_dryRun,
    getCoipPoolUsage_maxResults,
    getCoipPoolUsage_filters,
    getCoipPoolUsage_poolId,

    -- * Destructuring the Response
    GetCoipPoolUsageResponse (..),
    newGetCoipPoolUsageResponse,

    -- * Response Lenses
    getCoipPoolUsageResponse_coipAddressUsages,
    getCoipPoolUsageResponse_localGatewayRouteTableId,
    getCoipPoolUsageResponse_coipPoolId,
    getCoipPoolUsageResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCoipPoolUsage' smart constructor.
data GetCoipPoolUsage = GetCoipPoolUsage'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The filters. The following are the possible values:
    --
    -- -   @coip-address-usage.allocation-id@
    --
    -- -   @coip-address-usage.aws-account-id@
    --
    -- -   @coip-address-usage.aws-service@
    --
    -- -   @coip-address-usage.co-ip@
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the address pool.
    poolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCoipPoolUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCoipPoolUsage_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getCoipPoolUsage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getCoipPoolUsage_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'getCoipPoolUsage_filters' - The filters. The following are the possible values:
--
-- -   @coip-address-usage.allocation-id@
--
-- -   @coip-address-usage.aws-account-id@
--
-- -   @coip-address-usage.aws-service@
--
-- -   @coip-address-usage.co-ip@
--
-- 'poolId', 'getCoipPoolUsage_poolId' - The ID of the address pool.
newGetCoipPoolUsage ::
  -- | 'poolId'
  Prelude.Text ->
  GetCoipPoolUsage
newGetCoipPoolUsage pPoolId_ =
  GetCoipPoolUsage'
    { nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing,
      poolId = pPoolId_
    }

-- | The token for the next page of results.
getCoipPoolUsage_nextToken :: Lens.Lens' GetCoipPoolUsage (Prelude.Maybe Prelude.Text)
getCoipPoolUsage_nextToken = Lens.lens (\GetCoipPoolUsage' {nextToken} -> nextToken) (\s@GetCoipPoolUsage' {} a -> s {nextToken = a} :: GetCoipPoolUsage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getCoipPoolUsage_dryRun :: Lens.Lens' GetCoipPoolUsage (Prelude.Maybe Prelude.Bool)
getCoipPoolUsage_dryRun = Lens.lens (\GetCoipPoolUsage' {dryRun} -> dryRun) (\s@GetCoipPoolUsage' {} a -> s {dryRun = a} :: GetCoipPoolUsage)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getCoipPoolUsage_maxResults :: Lens.Lens' GetCoipPoolUsage (Prelude.Maybe Prelude.Natural)
getCoipPoolUsage_maxResults = Lens.lens (\GetCoipPoolUsage' {maxResults} -> maxResults) (\s@GetCoipPoolUsage' {} a -> s {maxResults = a} :: GetCoipPoolUsage)

-- | The filters. The following are the possible values:
--
-- -   @coip-address-usage.allocation-id@
--
-- -   @coip-address-usage.aws-account-id@
--
-- -   @coip-address-usage.aws-service@
--
-- -   @coip-address-usage.co-ip@
getCoipPoolUsage_filters :: Lens.Lens' GetCoipPoolUsage (Prelude.Maybe [Filter])
getCoipPoolUsage_filters = Lens.lens (\GetCoipPoolUsage' {filters} -> filters) (\s@GetCoipPoolUsage' {} a -> s {filters = a} :: GetCoipPoolUsage) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the address pool.
getCoipPoolUsage_poolId :: Lens.Lens' GetCoipPoolUsage Prelude.Text
getCoipPoolUsage_poolId = Lens.lens (\GetCoipPoolUsage' {poolId} -> poolId) (\s@GetCoipPoolUsage' {} a -> s {poolId = a} :: GetCoipPoolUsage)

instance Prelude.AWSRequest GetCoipPoolUsage where
  type Rs GetCoipPoolUsage = GetCoipPoolUsageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetCoipPoolUsageResponse'
            Prelude.<$> ( x Prelude..@? "coipAddressUsageSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (x Prelude..@? "localGatewayRouteTableId")
            Prelude.<*> (x Prelude..@? "coipPoolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoipPoolUsage

instance Prelude.NFData GetCoipPoolUsage

instance Prelude.ToHeaders GetCoipPoolUsage where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetCoipPoolUsage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCoipPoolUsage where
  toQuery GetCoipPoolUsage' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetCoipPoolUsage" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        Prelude.toQuery
          (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "PoolId" Prelude.=: poolId
      ]

-- | /See:/ 'newGetCoipPoolUsageResponse' smart constructor.
data GetCoipPoolUsageResponse = GetCoipPoolUsageResponse'
  { -- | Information about the address usage.
    coipAddressUsages :: Prelude.Maybe [CoipAddressUsage],
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the customer-owned address pool.
    coipPoolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCoipPoolUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coipAddressUsages', 'getCoipPoolUsageResponse_coipAddressUsages' - Information about the address usage.
--
-- 'localGatewayRouteTableId', 'getCoipPoolUsageResponse_localGatewayRouteTableId' - The ID of the local gateway route table.
--
-- 'coipPoolId', 'getCoipPoolUsageResponse_coipPoolId' - The ID of the customer-owned address pool.
--
-- 'httpStatus', 'getCoipPoolUsageResponse_httpStatus' - The response's http status code.
newGetCoipPoolUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoipPoolUsageResponse
newGetCoipPoolUsageResponse pHttpStatus_ =
  GetCoipPoolUsageResponse'
    { coipAddressUsages =
        Prelude.Nothing,
      localGatewayRouteTableId = Prelude.Nothing,
      coipPoolId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the address usage.
getCoipPoolUsageResponse_coipAddressUsages :: Lens.Lens' GetCoipPoolUsageResponse (Prelude.Maybe [CoipAddressUsage])
getCoipPoolUsageResponse_coipAddressUsages = Lens.lens (\GetCoipPoolUsageResponse' {coipAddressUsages} -> coipAddressUsages) (\s@GetCoipPoolUsageResponse' {} a -> s {coipAddressUsages = a} :: GetCoipPoolUsageResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the local gateway route table.
getCoipPoolUsageResponse_localGatewayRouteTableId :: Lens.Lens' GetCoipPoolUsageResponse (Prelude.Maybe Prelude.Text)
getCoipPoolUsageResponse_localGatewayRouteTableId = Lens.lens (\GetCoipPoolUsageResponse' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@GetCoipPoolUsageResponse' {} a -> s {localGatewayRouteTableId = a} :: GetCoipPoolUsageResponse)

-- | The ID of the customer-owned address pool.
getCoipPoolUsageResponse_coipPoolId :: Lens.Lens' GetCoipPoolUsageResponse (Prelude.Maybe Prelude.Text)
getCoipPoolUsageResponse_coipPoolId = Lens.lens (\GetCoipPoolUsageResponse' {coipPoolId} -> coipPoolId) (\s@GetCoipPoolUsageResponse' {} a -> s {coipPoolId = a} :: GetCoipPoolUsageResponse)

-- | The response's http status code.
getCoipPoolUsageResponse_httpStatus :: Lens.Lens' GetCoipPoolUsageResponse Prelude.Int
getCoipPoolUsageResponse_httpStatus = Lens.lens (\GetCoipPoolUsageResponse' {httpStatus} -> httpStatus) (\s@GetCoipPoolUsageResponse' {} a -> s {httpStatus = a} :: GetCoipPoolUsageResponse)

instance Prelude.NFData GetCoipPoolUsageResponse
