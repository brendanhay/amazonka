{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations between virtual interface groups and local gateway route tables.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  ( -- * Creating a request
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (..),
    mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations,

    -- ** Request lenses
    dlgrtvigaFilters,
    dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds,
    dlgrtvigaNextToken,
    dlgrtvigaDryRun,
    dlgrtvigaMaxResults,

    -- * Destructuring the response
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (..),
    mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse,

    -- ** Response lenses
    dlgrtvigarsNextToken,
    dlgrtvigarsLocalGatewayRouteTableVirtualInterfaceGroupAssociations,
    dlgrtvigarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
  { -- | One or more filters.
    --
    --
    --     * @local-gateway-id@ - The ID of a local gateway.
    --
    --
    --     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
    --
    --
    --     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
    --
    --
    --     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
    --
    --
    --     * @state@ - The state of the association.
    filters :: Lude.Maybe [Filter],
    -- | The IDs of the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Lude.Maybe [Lude.Text],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
--
--
--     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--     * @state@ - The state of the association.
--
--
-- * 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds' - The IDs of the associations.
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations ::
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations'
    { filters =
        Lude.Nothing,
      localGatewayRouteTableVirtualInterfaceGroupAssociationIds =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-virtual-interface-group-association-id@ - The ID of the association.
--
--
--     * @local-gateway-route-table-virtual-interface-group-id@ - The ID of the virtual interface group.
--
--
--     * @state@ - The state of the association.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaFilters :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Lude.Maybe [Filter])
dlgrtvigaFilters = Lens.lens (filters :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)
{-# DEPRECATED dlgrtvigaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Lude.Maybe [Lude.Text])
dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds = Lens.lens (localGatewayRouteTableVirtualInterfaceGroupAssociationIds :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> Lude.Maybe [Lude.Text]) (\s a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociationIds = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)
{-# DEPRECATED dlgrtvigaLocalGatewayRouteTableVirtualInterfaceGroupAssociationIds "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociationIds' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Lude.Maybe Lude.Text)
dlgrtvigaNextToken = Lens.lens (nextToken :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)
{-# DEPRECATED dlgrtvigaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaDryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Lude.Maybe Lude.Bool)
dlgrtvigaDryRun = Lens.lens (dryRun :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)
{-# DEPRECATED dlgrtvigaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigaMaxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations (Lude.Maybe Lude.Natural)
dlgrtvigaMaxResults = Lens.lens (maxResults :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations)
{-# DEPRECATED dlgrtvigaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance
  Page.AWSPager
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  page rq rs
    | Page.stop (rs Lens.^. dlgrtvigarsNextToken) = Lude.Nothing
    | Page.stop
        ( rs
            Lens.^. dlgrtvigarsLocalGatewayRouteTableVirtualInterfaceGroupAssociations
        ) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlgrtvigaNextToken Lens..~ rs Lens.^. dlgrtvigarsNextToken

instance
  Lude.AWSRequest
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  type
    Rs
      DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations =
      DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
              Lude.<*> ( x
                           Lude..@? "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"
                           Lude..!@ Lude.mempty
                           Lude.>>= Lude.may (Lude.parseXMLList "item")
                       )
              Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toHeaders = Lude.const Lude.mempty

instance
  Lude.ToPath
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations
  where
  toQuery
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations' {..} =
      Lude.mconcat
        [ "Action"
            Lude.=: ( "DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociations" ::
                        Lude.ByteString
                    ),
          "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
          Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
          Lude.toQuery
            ( Lude.toQueryList
                "LocalGatewayRouteTableVirtualInterfaceGroupAssociationId"
                Lude.<$> localGatewayRouteTableVirtualInterfaceGroupAssociationIds
            ),
          "NextToken" Lude.=: nextToken,
          "DryRun" Lude.=: dryRun,
          "MaxResults" Lude.=: maxResults
        ]

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse = DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the associations.
    localGatewayRouteTableVirtualInterfaceGroupAssociations :: Lude.Maybe [LocalGatewayRouteTableVirtualInterfaceGroupAssociation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'localGatewayRouteTableVirtualInterfaceGroupAssociations' - Information about the associations.
-- * 'responseStatus' - The response status code.
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
mkDescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse
  pResponseStatus_ =
    DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse'
      { nextToken =
          Lude.Nothing,
        localGatewayRouteTableVirtualInterfaceGroupAssociations =
          Lude.Nothing,
        responseStatus =
          pResponseStatus_
      }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarsNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Lude.Maybe Lude.Text)
dlgrtvigarsNextToken = Lens.lens (nextToken :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse)
{-# DEPRECATED dlgrtvigarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVirtualInterfaceGroupAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarsLocalGatewayRouteTableVirtualInterfaceGroupAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse (Lude.Maybe [LocalGatewayRouteTableVirtualInterfaceGroupAssociation])
dlgrtvigarsLocalGatewayRouteTableVirtualInterfaceGroupAssociations = Lens.lens (localGatewayRouteTableVirtualInterfaceGroupAssociations :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> Lude.Maybe [LocalGatewayRouteTableVirtualInterfaceGroupAssociation]) (\s a -> s {localGatewayRouteTableVirtualInterfaceGroupAssociations = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse)
{-# DEPRECATED dlgrtvigarsLocalGatewayRouteTableVirtualInterfaceGroupAssociations "Use generic-lens or generic-optics with 'localGatewayRouteTableVirtualInterfaceGroupAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvigarsResponseStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse Lude.Int
dlgrtvigarsResponseStatus = Lens.lens (responseStatus :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLocalGatewayRouteTableVirtualInterfaceGroupAssociationsResponse)
{-# DEPRECATED dlgrtvigarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
