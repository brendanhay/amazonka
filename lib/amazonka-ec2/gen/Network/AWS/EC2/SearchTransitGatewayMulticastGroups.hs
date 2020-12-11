{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.SearchTransitGatewayMulticastGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches one or more transit gateway multicast groups and returns the group membership information.
--
-- This operation returns paginated results.
module Network.AWS.EC2.SearchTransitGatewayMulticastGroups
  ( -- * Creating a request
    SearchTransitGatewayMulticastGroups (..),
    mkSearchTransitGatewayMulticastGroups,

    -- ** Request lenses
    stgmgFilters,
    stgmgTransitGatewayMulticastDomainId,
    stgmgNextToken,
    stgmgDryRun,
    stgmgMaxResults,

    -- * Destructuring the response
    SearchTransitGatewayMulticastGroupsResponse (..),
    mkSearchTransitGatewayMulticastGroupsResponse,

    -- ** Response lenses
    stgmgrsNextToken,
    stgmgrsMulticastGroups,
    stgmgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchTransitGatewayMulticastGroups' smart constructor.
data SearchTransitGatewayMulticastGroups = SearchTransitGatewayMulticastGroups'
  { filters ::
      Lude.Maybe [Filter],
    transitGatewayMulticastDomainId ::
      Lude.Maybe
        Lude.Text,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    maxResults ::
      Lude.Maybe
        Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchTransitGatewayMulticastGroups' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @group-ip-address@ - The IP address of the transit gateway multicast group.
--
--
--     * @is-group-member@ - The resource is a group member. Valid values are @true@ | @false@ .
--
--
--     * @is-group-source@ - The resource is a group source. Valid values are @true@ | @false@ .
--
--
--     * @member-type@ - The member type. Valid values are @igmp@ | @static@ .
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The type of resource. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @tgw-peering@ .
--
--
--     * @source-type@ - The source type. Valid values are @igmp@ | @static@ .
--
--
--     * @state@ - The state of the subnet association. Valid values are @associated@ | @associated@ | @disassociated@ | @disassociating@ .
--
--
--     * @subnet-id@ - The ID of the subnet.
--
--
--     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
mkSearchTransitGatewayMulticastGroups ::
  SearchTransitGatewayMulticastGroups
mkSearchTransitGatewayMulticastGroups =
  SearchTransitGatewayMulticastGroups'
    { filters = Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. The possible values are:
--
--
--     * @group-ip-address@ - The IP address of the transit gateway multicast group.
--
--
--     * @is-group-member@ - The resource is a group member. Valid values are @true@ | @false@ .
--
--
--     * @is-group-source@ - The resource is a group source. Valid values are @true@ | @false@ .
--
--
--     * @member-type@ - The member type. Valid values are @igmp@ | @static@ .
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The type of resource. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @tgw-peering@ .
--
--
--     * @source-type@ - The source type. Valid values are @igmp@ | @static@ .
--
--
--     * @state@ - The state of the subnet association. Valid values are @associated@ | @associated@ | @disassociated@ | @disassociating@ .
--
--
--     * @subnet-id@ - The ID of the subnet.
--
--
--     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgFilters :: Lens.Lens' SearchTransitGatewayMulticastGroups (Lude.Maybe [Filter])
stgmgFilters = Lens.lens (filters :: SearchTransitGatewayMulticastGroups -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchTransitGatewayMulticastGroups)
{-# DEPRECATED stgmgFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgTransitGatewayMulticastDomainId :: Lens.Lens' SearchTransitGatewayMulticastGroups (Lude.Maybe Lude.Text)
stgmgTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: SearchTransitGatewayMulticastGroups -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: SearchTransitGatewayMulticastGroups)
{-# DEPRECATED stgmgTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgNextToken :: Lens.Lens' SearchTransitGatewayMulticastGroups (Lude.Maybe Lude.Text)
stgmgNextToken = Lens.lens (nextToken :: SearchTransitGatewayMulticastGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchTransitGatewayMulticastGroups)
{-# DEPRECATED stgmgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgDryRun :: Lens.Lens' SearchTransitGatewayMulticastGroups (Lude.Maybe Lude.Bool)
stgmgDryRun = Lens.lens (dryRun :: SearchTransitGatewayMulticastGroups -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: SearchTransitGatewayMulticastGroups)
{-# DEPRECATED stgmgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgMaxResults :: Lens.Lens' SearchTransitGatewayMulticastGroups (Lude.Maybe Lude.Natural)
stgmgMaxResults = Lens.lens (maxResults :: SearchTransitGatewayMulticastGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchTransitGatewayMulticastGroups)
{-# DEPRECATED stgmgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchTransitGatewayMulticastGroups where
  page rq rs
    | Page.stop (rs Lens.^. stgmgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. stgmgrsMulticastGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& stgmgNextToken Lens..~ rs Lens.^. stgmgrsNextToken

instance Lude.AWSRequest SearchTransitGatewayMulticastGroups where
  type
    Rs SearchTransitGatewayMulticastGroups =
      SearchTransitGatewayMulticastGroupsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          SearchTransitGatewayMulticastGroupsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "multicastGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchTransitGatewayMulticastGroups where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SearchTransitGatewayMulticastGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchTransitGatewayMulticastGroups where
  toQuery SearchTransitGatewayMulticastGroups' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SearchTransitGatewayMulticastGroups" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "TransitGatewayMulticastDomainId"
          Lude.=: transitGatewayMulticastDomainId,
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkSearchTransitGatewayMulticastGroupsResponse' smart constructor.
data SearchTransitGatewayMulticastGroupsResponse = SearchTransitGatewayMulticastGroupsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    multicastGroups ::
      Lude.Maybe
        [TransitGatewayMulticastGroup],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchTransitGatewayMulticastGroupsResponse' with the minimum fields required to make a request.
--
-- * 'multicastGroups' - Information about the transit gateway multicast group.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkSearchTransitGatewayMulticastGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchTransitGatewayMulticastGroupsResponse
mkSearchTransitGatewayMulticastGroupsResponse pResponseStatus_ =
  SearchTransitGatewayMulticastGroupsResponse'
    { nextToken =
        Lude.Nothing,
      multicastGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrsNextToken :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Lude.Maybe Lude.Text)
stgmgrsNextToken = Lens.lens (nextToken :: SearchTransitGatewayMulticastGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchTransitGatewayMulticastGroupsResponse)
{-# DEPRECATED stgmgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the transit gateway multicast group.
--
-- /Note:/ Consider using 'multicastGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrsMulticastGroups :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse (Lude.Maybe [TransitGatewayMulticastGroup])
stgmgrsMulticastGroups = Lens.lens (multicastGroups :: SearchTransitGatewayMulticastGroupsResponse -> Lude.Maybe [TransitGatewayMulticastGroup]) (\s a -> s {multicastGroups = a} :: SearchTransitGatewayMulticastGroupsResponse)
{-# DEPRECATED stgmgrsMulticastGroups "Use generic-lens or generic-optics with 'multicastGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stgmgrsResponseStatus :: Lens.Lens' SearchTransitGatewayMulticastGroupsResponse Lude.Int
stgmgrsResponseStatus = Lens.lens (responseStatus :: SearchTransitGatewayMulticastGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchTransitGatewayMulticastGroupsResponse)
{-# DEPRECATED stgmgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
