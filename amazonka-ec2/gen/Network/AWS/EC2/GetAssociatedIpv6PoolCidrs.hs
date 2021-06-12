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
-- Module      : Network.AWS.EC2.GetAssociatedIpv6PoolCidrs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the IPv6 CIDR block associations for a specified
-- IPv6 address pool.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetAssociatedIpv6PoolCidrs
  ( -- * Creating a Request
    GetAssociatedIpv6PoolCidrs (..),
    newGetAssociatedIpv6PoolCidrs,

    -- * Request Lenses
    getAssociatedIpv6PoolCidrs_nextToken,
    getAssociatedIpv6PoolCidrs_dryRun,
    getAssociatedIpv6PoolCidrs_maxResults,
    getAssociatedIpv6PoolCidrs_poolId,

    -- * Destructuring the Response
    GetAssociatedIpv6PoolCidrsResponse (..),
    newGetAssociatedIpv6PoolCidrsResponse,

    -- * Response Lenses
    getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations,
    getAssociatedIpv6PoolCidrsResponse_nextToken,
    getAssociatedIpv6PoolCidrsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAssociatedIpv6PoolCidrs' smart constructor.
data GetAssociatedIpv6PoolCidrs = GetAssociatedIpv6PoolCidrs'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the IPv6 address pool.
    poolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAssociatedIpv6PoolCidrs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getAssociatedIpv6PoolCidrs_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getAssociatedIpv6PoolCidrs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getAssociatedIpv6PoolCidrs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'poolId', 'getAssociatedIpv6PoolCidrs_poolId' - The ID of the IPv6 address pool.
newGetAssociatedIpv6PoolCidrs ::
  -- | 'poolId'
  Core.Text ->
  GetAssociatedIpv6PoolCidrs
newGetAssociatedIpv6PoolCidrs pPoolId_ =
  GetAssociatedIpv6PoolCidrs'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      poolId = pPoolId_
    }

-- | The token for the next page of results.
getAssociatedIpv6PoolCidrs_nextToken :: Lens.Lens' GetAssociatedIpv6PoolCidrs (Core.Maybe Core.Text)
getAssociatedIpv6PoolCidrs_nextToken = Lens.lens (\GetAssociatedIpv6PoolCidrs' {nextToken} -> nextToken) (\s@GetAssociatedIpv6PoolCidrs' {} a -> s {nextToken = a} :: GetAssociatedIpv6PoolCidrs)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getAssociatedIpv6PoolCidrs_dryRun :: Lens.Lens' GetAssociatedIpv6PoolCidrs (Core.Maybe Core.Bool)
getAssociatedIpv6PoolCidrs_dryRun = Lens.lens (\GetAssociatedIpv6PoolCidrs' {dryRun} -> dryRun) (\s@GetAssociatedIpv6PoolCidrs' {} a -> s {dryRun = a} :: GetAssociatedIpv6PoolCidrs)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getAssociatedIpv6PoolCidrs_maxResults :: Lens.Lens' GetAssociatedIpv6PoolCidrs (Core.Maybe Core.Natural)
getAssociatedIpv6PoolCidrs_maxResults = Lens.lens (\GetAssociatedIpv6PoolCidrs' {maxResults} -> maxResults) (\s@GetAssociatedIpv6PoolCidrs' {} a -> s {maxResults = a} :: GetAssociatedIpv6PoolCidrs)

-- | The ID of the IPv6 address pool.
getAssociatedIpv6PoolCidrs_poolId :: Lens.Lens' GetAssociatedIpv6PoolCidrs Core.Text
getAssociatedIpv6PoolCidrs_poolId = Lens.lens (\GetAssociatedIpv6PoolCidrs' {poolId} -> poolId) (\s@GetAssociatedIpv6PoolCidrs' {} a -> s {poolId = a} :: GetAssociatedIpv6PoolCidrs)

instance Core.AWSPager GetAssociatedIpv6PoolCidrs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAssociatedIpv6PoolCidrsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getAssociatedIpv6PoolCidrs_nextToken
          Lens..~ rs
          Lens.^? getAssociatedIpv6PoolCidrsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetAssociatedIpv6PoolCidrs where
  type
    AWSResponse GetAssociatedIpv6PoolCidrs =
      GetAssociatedIpv6PoolCidrsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetAssociatedIpv6PoolCidrsResponse'
            Core.<$> ( x Core..@? "ipv6CidrAssociationSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAssociatedIpv6PoolCidrs

instance Core.NFData GetAssociatedIpv6PoolCidrs

instance Core.ToHeaders GetAssociatedIpv6PoolCidrs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetAssociatedIpv6PoolCidrs where
  toPath = Core.const "/"

instance Core.ToQuery GetAssociatedIpv6PoolCidrs where
  toQuery GetAssociatedIpv6PoolCidrs' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetAssociatedIpv6PoolCidrs" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "PoolId" Core.=: poolId
      ]

-- | /See:/ 'newGetAssociatedIpv6PoolCidrsResponse' smart constructor.
data GetAssociatedIpv6PoolCidrsResponse = GetAssociatedIpv6PoolCidrsResponse'
  { -- | Information about the IPv6 CIDR block associations.
    ipv6CidrAssociations :: Core.Maybe [Ipv6CidrAssociation],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAssociatedIpv6PoolCidrsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrAssociations', 'getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations' - Information about the IPv6 CIDR block associations.
--
-- 'nextToken', 'getAssociatedIpv6PoolCidrsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'getAssociatedIpv6PoolCidrsResponse_httpStatus' - The response's http status code.
newGetAssociatedIpv6PoolCidrsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAssociatedIpv6PoolCidrsResponse
newGetAssociatedIpv6PoolCidrsResponse pHttpStatus_ =
  GetAssociatedIpv6PoolCidrsResponse'
    { ipv6CidrAssociations =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv6 CIDR block associations.
getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations :: Lens.Lens' GetAssociatedIpv6PoolCidrsResponse (Core.Maybe [Ipv6CidrAssociation])
getAssociatedIpv6PoolCidrsResponse_ipv6CidrAssociations = Lens.lens (\GetAssociatedIpv6PoolCidrsResponse' {ipv6CidrAssociations} -> ipv6CidrAssociations) (\s@GetAssociatedIpv6PoolCidrsResponse' {} a -> s {ipv6CidrAssociations = a} :: GetAssociatedIpv6PoolCidrsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getAssociatedIpv6PoolCidrsResponse_nextToken :: Lens.Lens' GetAssociatedIpv6PoolCidrsResponse (Core.Maybe Core.Text)
getAssociatedIpv6PoolCidrsResponse_nextToken = Lens.lens (\GetAssociatedIpv6PoolCidrsResponse' {nextToken} -> nextToken) (\s@GetAssociatedIpv6PoolCidrsResponse' {} a -> s {nextToken = a} :: GetAssociatedIpv6PoolCidrsResponse)

-- | The response's http status code.
getAssociatedIpv6PoolCidrsResponse_httpStatus :: Lens.Lens' GetAssociatedIpv6PoolCidrsResponse Core.Int
getAssociatedIpv6PoolCidrsResponse_httpStatus = Lens.lens (\GetAssociatedIpv6PoolCidrsResponse' {httpStatus} -> httpStatus) (\s@GetAssociatedIpv6PoolCidrsResponse' {} a -> s {httpStatus = a} :: GetAssociatedIpv6PoolCidrsResponse)

instance
  Core.NFData
    GetAssociatedIpv6PoolCidrsResponse
