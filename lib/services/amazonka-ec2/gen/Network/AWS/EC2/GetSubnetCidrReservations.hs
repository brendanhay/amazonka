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
-- Module      : Network.AWS.EC2.GetSubnetCidrReservations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the subnet CIDR reservations.
module Network.AWS.EC2.GetSubnetCidrReservations
  ( -- * Creating a Request
    GetSubnetCidrReservations (..),
    newGetSubnetCidrReservations,

    -- * Request Lenses
    getSubnetCidrReservations_nextToken,
    getSubnetCidrReservations_maxResults,
    getSubnetCidrReservations_dryRun,
    getSubnetCidrReservations_filters,
    getSubnetCidrReservations_subnetId,

    -- * Destructuring the Response
    GetSubnetCidrReservationsResponse (..),
    newGetSubnetCidrReservationsResponse,

    -- * Response Lenses
    getSubnetCidrReservationsResponse_nextToken,
    getSubnetCidrReservationsResponse_subnetIpv4CidrReservations,
    getSubnetCidrReservationsResponse_subnetIpv6CidrReservations,
    getSubnetCidrReservationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSubnetCidrReservations' smart constructor.
data GetSubnetCidrReservations = GetSubnetCidrReservations'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @reservationType@ - The type of reservation (@prefix@ | @explicit@).
    --
    -- -   @subnet-id@ - The ID of the subnet.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the subnet.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubnetCidrReservations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSubnetCidrReservations_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'getSubnetCidrReservations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'dryRun', 'getSubnetCidrReservations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'getSubnetCidrReservations_filters' - One or more filters.
--
-- -   @reservationType@ - The type of reservation (@prefix@ | @explicit@).
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- 'subnetId', 'getSubnetCidrReservations_subnetId' - The ID of the subnet.
newGetSubnetCidrReservations ::
  -- | 'subnetId'
  Prelude.Text ->
  GetSubnetCidrReservations
newGetSubnetCidrReservations pSubnetId_ =
  GetSubnetCidrReservations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | The token for the next page of results.
getSubnetCidrReservations_nextToken :: Lens.Lens' GetSubnetCidrReservations (Prelude.Maybe Prelude.Text)
getSubnetCidrReservations_nextToken = Lens.lens (\GetSubnetCidrReservations' {nextToken} -> nextToken) (\s@GetSubnetCidrReservations' {} a -> s {nextToken = a} :: GetSubnetCidrReservations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getSubnetCidrReservations_maxResults :: Lens.Lens' GetSubnetCidrReservations (Prelude.Maybe Prelude.Natural)
getSubnetCidrReservations_maxResults = Lens.lens (\GetSubnetCidrReservations' {maxResults} -> maxResults) (\s@GetSubnetCidrReservations' {} a -> s {maxResults = a} :: GetSubnetCidrReservations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getSubnetCidrReservations_dryRun :: Lens.Lens' GetSubnetCidrReservations (Prelude.Maybe Prelude.Bool)
getSubnetCidrReservations_dryRun = Lens.lens (\GetSubnetCidrReservations' {dryRun} -> dryRun) (\s@GetSubnetCidrReservations' {} a -> s {dryRun = a} :: GetSubnetCidrReservations)

-- | One or more filters.
--
-- -   @reservationType@ - The type of reservation (@prefix@ | @explicit@).
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
getSubnetCidrReservations_filters :: Lens.Lens' GetSubnetCidrReservations (Prelude.Maybe [Filter])
getSubnetCidrReservations_filters = Lens.lens (\GetSubnetCidrReservations' {filters} -> filters) (\s@GetSubnetCidrReservations' {} a -> s {filters = a} :: GetSubnetCidrReservations) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the subnet.
getSubnetCidrReservations_subnetId :: Lens.Lens' GetSubnetCidrReservations Prelude.Text
getSubnetCidrReservations_subnetId = Lens.lens (\GetSubnetCidrReservations' {subnetId} -> subnetId) (\s@GetSubnetCidrReservations' {} a -> s {subnetId = a} :: GetSubnetCidrReservations)

instance Core.AWSRequest GetSubnetCidrReservations where
  type
    AWSResponse GetSubnetCidrReservations =
      GetSubnetCidrReservationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetSubnetCidrReservationsResponse'
            Prelude.<$> (x Core..@? "nextToken")
            Prelude.<*> ( x Core..@? "subnetIpv4CidrReservationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "subnetIpv6CidrReservationSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSubnetCidrReservations

instance Prelude.NFData GetSubnetCidrReservations

instance Core.ToHeaders GetSubnetCidrReservations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetSubnetCidrReservations where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSubnetCidrReservations where
  toQuery GetSubnetCidrReservations' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetSubnetCidrReservations" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "SubnetId" Core.=: subnetId
      ]

-- | /See:/ 'newGetSubnetCidrReservationsResponse' smart constructor.
data GetSubnetCidrReservationsResponse = GetSubnetCidrReservationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv4 subnet CIDR reservations.
    subnetIpv4CidrReservations :: Prelude.Maybe [SubnetCidrReservation],
    -- | Information about the IPv6 subnet CIDR reservations.
    subnetIpv6CidrReservations :: Prelude.Maybe [SubnetCidrReservation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubnetCidrReservationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSubnetCidrReservationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'subnetIpv4CidrReservations', 'getSubnetCidrReservationsResponse_subnetIpv4CidrReservations' - Information about the IPv4 subnet CIDR reservations.
--
-- 'subnetIpv6CidrReservations', 'getSubnetCidrReservationsResponse_subnetIpv6CidrReservations' - Information about the IPv6 subnet CIDR reservations.
--
-- 'httpStatus', 'getSubnetCidrReservationsResponse_httpStatus' - The response's http status code.
newGetSubnetCidrReservationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSubnetCidrReservationsResponse
newGetSubnetCidrReservationsResponse pHttpStatus_ =
  GetSubnetCidrReservationsResponse'
    { nextToken =
        Prelude.Nothing,
      subnetIpv4CidrReservations =
        Prelude.Nothing,
      subnetIpv6CidrReservations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getSubnetCidrReservationsResponse_nextToken :: Lens.Lens' GetSubnetCidrReservationsResponse (Prelude.Maybe Prelude.Text)
getSubnetCidrReservationsResponse_nextToken = Lens.lens (\GetSubnetCidrReservationsResponse' {nextToken} -> nextToken) (\s@GetSubnetCidrReservationsResponse' {} a -> s {nextToken = a} :: GetSubnetCidrReservationsResponse)

-- | Information about the IPv4 subnet CIDR reservations.
getSubnetCidrReservationsResponse_subnetIpv4CidrReservations :: Lens.Lens' GetSubnetCidrReservationsResponse (Prelude.Maybe [SubnetCidrReservation])
getSubnetCidrReservationsResponse_subnetIpv4CidrReservations = Lens.lens (\GetSubnetCidrReservationsResponse' {subnetIpv4CidrReservations} -> subnetIpv4CidrReservations) (\s@GetSubnetCidrReservationsResponse' {} a -> s {subnetIpv4CidrReservations = a} :: GetSubnetCidrReservationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the IPv6 subnet CIDR reservations.
getSubnetCidrReservationsResponse_subnetIpv6CidrReservations :: Lens.Lens' GetSubnetCidrReservationsResponse (Prelude.Maybe [SubnetCidrReservation])
getSubnetCidrReservationsResponse_subnetIpv6CidrReservations = Lens.lens (\GetSubnetCidrReservationsResponse' {subnetIpv6CidrReservations} -> subnetIpv6CidrReservations) (\s@GetSubnetCidrReservationsResponse' {} a -> s {subnetIpv6CidrReservations = a} :: GetSubnetCidrReservationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getSubnetCidrReservationsResponse_httpStatus :: Lens.Lens' GetSubnetCidrReservationsResponse Prelude.Int
getSubnetCidrReservationsResponse_httpStatus = Lens.lens (\GetSubnetCidrReservationsResponse' {httpStatus} -> httpStatus) (\s@GetSubnetCidrReservationsResponse' {} a -> s {httpStatus = a} :: GetSubnetCidrReservationsResponse)

instance
  Prelude.NFData
    GetSubnetCidrReservationsResponse
