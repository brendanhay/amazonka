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
-- Module      : Network.AWS.EC2.SearchTransitGatewayMulticastGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches one or more transit gateway multicast groups and returns the
-- group membership information.
--
-- This operation returns paginated results.
module Network.AWS.EC2.SearchTransitGatewayMulticastGroups
  ( -- * Creating a Request
    SearchTransitGatewayMulticastGroups (..),
    newSearchTransitGatewayMulticastGroups,

    -- * Request Lenses
    searchTransitGatewayMulticastGroups_nextToken,
    searchTransitGatewayMulticastGroups_dryRun,
    searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId,
    searchTransitGatewayMulticastGroups_maxResults,
    searchTransitGatewayMulticastGroups_filters,

    -- * Destructuring the Response
    SearchTransitGatewayMulticastGroupsResponse (..),
    newSearchTransitGatewayMulticastGroupsResponse,

    -- * Response Lenses
    searchTransitGatewayMulticastGroupsResponse_nextToken,
    searchTransitGatewayMulticastGroupsResponse_multicastGroups,
    searchTransitGatewayMulticastGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchTransitGatewayMulticastGroups' smart constructor.
data SearchTransitGatewayMulticastGroups = SearchTransitGatewayMulticastGroups'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters. The possible values are:
    --
    -- -   @group-ip-address@ - The IP address of the transit gateway multicast
    --     group.
    --
    -- -   @is-group-member@ - The resource is a group member. Valid values are
    --     @true@ | @false@.
    --
    -- -   @is-group-source@ - The resource is a group source. Valid values are
    --     @true@ | @false@.
    --
    -- -   @member-type@ - The member type. Valid values are @igmp@ | @static@.
    --
    -- -   @resource-id@ - The ID of the resource.
    --
    -- -   @resource-type@ - The type of resource. Valid values are @vpc@ |
    --     @vpn@ | @direct-connect-gateway@ | @tgw-peering@.
    --
    -- -   @source-type@ - The source type. Valid values are @igmp@ | @static@.
    --
    -- -   @state@ - The state of the subnet association. Valid values are
    --     @associated@ | @associated@ | @disassociated@ | @disassociating@.
    --
    -- -   @subnet-id@ - The ID of the subnet.
    --
    -- -   @transit-gateway-attachment-id@ - The id of the transit gateway
    --     attachment.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTransitGatewayMulticastGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchTransitGatewayMulticastGroups_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'searchTransitGatewayMulticastGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'maxResults', 'searchTransitGatewayMulticastGroups_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'searchTransitGatewayMulticastGroups_filters' - One or more filters. The possible values are:
--
-- -   @group-ip-address@ - The IP address of the transit gateway multicast
--     group.
--
-- -   @is-group-member@ - The resource is a group member. Valid values are
--     @true@ | @false@.
--
-- -   @is-group-source@ - The resource is a group source. Valid values are
--     @true@ | @false@.
--
-- -   @member-type@ - The member type. Valid values are @igmp@ | @static@.
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The type of resource. Valid values are @vpc@ |
--     @vpn@ | @direct-connect-gateway@ | @tgw-peering@.
--
-- -   @source-type@ - The source type. Valid values are @igmp@ | @static@.
--
-- -   @state@ - The state of the subnet association. Valid values are
--     @associated@ | @associated@ | @disassociated@ | @disassociating@.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @transit-gateway-attachment-id@ - The id of the transit gateway
--     attachment.
newSearchTransitGatewayMulticastGroups ::
  SearchTransitGatewayMulticastGroups
newSearchTransitGatewayMulticastGroups =
  SearchTransitGatewayMulticastGroups'
    { nextToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The token for the next page of results.
searchTransitGatewayMulticastGroups_nextToken :: Lens.Lens' SearchTransitGatewayMulticastGroups (Prelude.Maybe Prelude.Text)
searchTransitGatewayMulticastGroups_nextToken = Lens.lens (\SearchTransitGatewayMulticastGroups' {nextToken} -> nextToken) (\s@SearchTransitGatewayMulticastGroups' {} a -> s {nextToken = a} :: SearchTransitGatewayMulticastGroups)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
searchTransitGatewayMulticastGroups_dryRun :: Lens.Lens' SearchTransitGatewayMulticastGroups (Prelude.Maybe Prelude.Bool)
searchTransitGatewayMulticastGroups_dryRun = Lens.lens (\SearchTransitGatewayMulticastGroups' {dryRun} -> dryRun) (\s@SearchTransitGatewayMulticastGroups' {} a -> s {dryRun = a} :: SearchTransitGatewayMulticastGroups)

-- | The ID of the transit gateway multicast domain.
searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId :: Lens.Lens' SearchTransitGatewayMulticastGroups (Prelude.Maybe Prelude.Text)
searchTransitGatewayMulticastGroups_transitGatewayMulticastDomainId = Lens.lens (\SearchTransitGatewayMulticastGroups' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@SearchTransitGatewayMulticastGroups' {} a -> s {transitGatewayMulticastDomainId = a} :: SearchTransitGatewayMulticastGroups)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
searchTransitGatewayMulticastGroups_maxResults :: Lens.Lens' SearchTransitGatewayMulticastGroups (Prelude.Maybe Prelude.Natural)
searchTransitGatewayMulticastGroups_maxResults = Lens.lens (\SearchTransitGatewayMulticastGroups' {maxResults} -> maxResults) (\s@SearchTransitGatewayMulticastGroups' {} a -> s {maxResults = a} :: SearchTransitGatewayMulticastGroups)

-- | One or more filters. The possible values are:
--
-- -   @group-ip-address@ - The IP address of the transit gateway multicast
--     group.
--
-- -   @is-group-member@ - The resource is a group member. Valid values are
--     @true@ | @false@.
--
-- -   @is-group-source@ - The resource is a group source. Valid values are
--     @true@ | @false@.
--
-- -   @member-type@ - The member type. Valid values are @igmp@ | @static@.
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The type of resource. Valid values are @vpc@ |
--     @vpn@ | @direct-connect-gateway@ | @tgw-peering@.
--
-- -   @source-type@ - The source type. Valid values are @igmp@ | @static@.
--
-- -   @state@ - The state of the subnet association. Valid values are
--     @associated@ | @associated@ | @disassociated@ | @disassociating@.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @transit-gateway-attachment-id@ - The id of the transit gateway
--     attachment.
searchTransitGatewayMulticastGroups_filters :: Lens.Lens' SearchTransitGatewayMulticastGroups (Prelude.Maybe [Filter])
searchTransitGatewayMulticastGroups_filters = Lens.lens (\SearchTransitGatewayMulticastGroups' {filters} -> filters) (\s@SearchTransitGatewayMulticastGroups' {} a -> s {filters = a} :: SearchTransitGatewayMulticastGroups) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    SearchTransitGatewayMulticastGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchTransitGatewayMulticastGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchTransitGatewayMulticastGroupsResponse_multicastGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchTransitGatewayMulticastGroups_nextToken
          Lens..~ rs
          Lens.^? searchTransitGatewayMulticastGroupsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    SearchTransitGatewayMulticastGroups
  where
  type
    AWSResponse SearchTransitGatewayMulticastGroups =
      SearchTransitGatewayMulticastGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          SearchTransitGatewayMulticastGroupsResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "multicastGroups" Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SearchTransitGatewayMulticastGroups

instance
  Prelude.NFData
    SearchTransitGatewayMulticastGroups

instance
  Core.ToHeaders
    SearchTransitGatewayMulticastGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    SearchTransitGatewayMulticastGroups
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    SearchTransitGatewayMulticastGroups
  where
  toQuery SearchTransitGatewayMulticastGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "SearchTransitGatewayMulticastGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "TransitGatewayMulticastDomainId"
          Core.=: transitGatewayMulticastDomainId,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newSearchTransitGatewayMulticastGroupsResponse' smart constructor.
data SearchTransitGatewayMulticastGroupsResponse = SearchTransitGatewayMulticastGroupsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the transit gateway multicast group.
    multicastGroups :: Prelude.Maybe [TransitGatewayMulticastGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTransitGatewayMulticastGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchTransitGatewayMulticastGroupsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'multicastGroups', 'searchTransitGatewayMulticastGroupsResponse_multicastGroups' - Information about the transit gateway multicast group.
--
-- 'httpStatus', 'searchTransitGatewayMulticastGroupsResponse_httpStatus' - The response's http status code.
newSearchTransitGatewayMulticastGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchTransitGatewayMulticastGroupsResponse
newSearchTransitGatewayMulticastGroupsResponse
  pHttpStatus_ =
    SearchTransitGatewayMulticastGroupsResponse'
      { nextToken =
          Prelude.Nothing,
        multicastGroups =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
searchTransitGatewayMulticastGroupsResponse_nextToken :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Prelude.Maybe Prelude.Text)
searchTransitGatewayMulticastGroupsResponse_nextToken = Lens.lens (\SearchTransitGatewayMulticastGroupsResponse' {nextToken} -> nextToken) (\s@SearchTransitGatewayMulticastGroupsResponse' {} a -> s {nextToken = a} :: SearchTransitGatewayMulticastGroupsResponse)

-- | Information about the transit gateway multicast group.
searchTransitGatewayMulticastGroupsResponse_multicastGroups :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Prelude.Maybe [TransitGatewayMulticastGroup])
searchTransitGatewayMulticastGroupsResponse_multicastGroups = Lens.lens (\SearchTransitGatewayMulticastGroupsResponse' {multicastGroups} -> multicastGroups) (\s@SearchTransitGatewayMulticastGroupsResponse' {} a -> s {multicastGroups = a} :: SearchTransitGatewayMulticastGroupsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchTransitGatewayMulticastGroupsResponse_httpStatus :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse Prelude.Int
searchTransitGatewayMulticastGroupsResponse_httpStatus = Lens.lens (\SearchTransitGatewayMulticastGroupsResponse' {httpStatus} -> httpStatus) (\s@SearchTransitGatewayMulticastGroupsResponse' {} a -> s {httpStatus = a} :: SearchTransitGatewayMulticastGroupsResponse)

instance
  Prelude.NFData
    SearchTransitGatewayMulticastGroupsResponse
