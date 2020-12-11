{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVPCAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified associations between VPCs and local gateway route tables.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVPCAssociations
  ( -- * Creating a request
    DescribeLocalGatewayRouteTableVPCAssociations (..),
    mkDescribeLocalGatewayRouteTableVPCAssociations,

    -- ** Request lenses
    dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds,
    dlgrtvpcaFilters,
    dlgrtvpcaNextToken,
    dlgrtvpcaDryRun,
    dlgrtvpcaMaxResults,

    -- * Destructuring the response
    DescribeLocalGatewayRouteTableVPCAssociationsResponse (..),
    mkDescribeLocalGatewayRouteTableVPCAssociationsResponse,

    -- ** Response lenses
    dlgrtvpcarsLocalGatewayRouteTableVPCAssociations,
    dlgrtvpcarsNextToken,
    dlgrtvpcarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVPCAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVPCAssociations = DescribeLocalGatewayRouteTableVPCAssociations'
  { localGatewayRouteTableVPCAssociationIds ::
      Lude.Maybe
        [Lude.Text],
    filters ::
      Lude.Maybe
        [Filter],
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeLocalGatewayRouteTableVPCAssociations' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-vpc-association-id@ - The ID of the association.
--
--
--     * @state@ - The state of the association.
--
--
--     * @vpc-id@ - The ID of the VPC.
--
--
-- * 'localGatewayRouteTableVPCAssociationIds' - The IDs of the associations.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
mkDescribeLocalGatewayRouteTableVPCAssociations ::
  DescribeLocalGatewayRouteTableVPCAssociations
mkDescribeLocalGatewayRouteTableVPCAssociations =
  DescribeLocalGatewayRouteTableVPCAssociations'
    { localGatewayRouteTableVPCAssociationIds =
        Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The IDs of the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVPCAssociationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociations (Lude.Maybe [Lude.Text])
dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds = Lens.lens (localGatewayRouteTableVPCAssociationIds :: DescribeLocalGatewayRouteTableVPCAssociations -> Lude.Maybe [Lude.Text]) (\s a -> s {localGatewayRouteTableVPCAssociationIds = a} :: DescribeLocalGatewayRouteTableVPCAssociations)
{-# DEPRECATED dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds "Use generic-lens or generic-optics with 'localGatewayRouteTableVPCAssociationIds' instead." #-}

-- | One or more filters.
--
--
--     * @local-gateway-id@ - The ID of a local gateway.
--
--
--     * @local-gateway-route-table-id@ - The ID of the local gateway route table.
--
--
--     * @local-gateway-route-table-vpc-association-id@ - The ID of the association.
--
--
--     * @state@ - The state of the association.
--
--
--     * @vpc-id@ - The ID of the VPC.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcaFilters :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociations (Lude.Maybe [Filter])
dlgrtvpcaFilters = Lens.lens (filters :: DescribeLocalGatewayRouteTableVPCAssociations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeLocalGatewayRouteTableVPCAssociations)
{-# DEPRECATED dlgrtvpcaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcaNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociations (Lude.Maybe Lude.Text)
dlgrtvpcaNextToken = Lens.lens (nextToken :: DescribeLocalGatewayRouteTableVPCAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVPCAssociations)
{-# DEPRECATED dlgrtvpcaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcaDryRun :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociations (Lude.Maybe Lude.Bool)
dlgrtvpcaDryRun = Lens.lens (dryRun :: DescribeLocalGatewayRouteTableVPCAssociations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeLocalGatewayRouteTableVPCAssociations)
{-# DEPRECATED dlgrtvpcaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcaMaxResults :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociations (Lude.Maybe Lude.Natural)
dlgrtvpcaMaxResults = Lens.lens (maxResults :: DescribeLocalGatewayRouteTableVPCAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeLocalGatewayRouteTableVPCAssociations)
{-# DEPRECATED dlgrtvpcaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance
  Page.AWSPager
    DescribeLocalGatewayRouteTableVPCAssociations
  where
  page rq rs
    | Page.stop (rs Lens.^. dlgrtvpcarsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. dlgrtvpcarsLocalGatewayRouteTableVPCAssociations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlgrtvpcaNextToken Lens..~ rs Lens.^. dlgrtvpcarsNextToken

instance
  Lude.AWSRequest
    DescribeLocalGatewayRouteTableVPCAssociations
  where
  type
    Rs DescribeLocalGatewayRouteTableVPCAssociations =
      DescribeLocalGatewayRouteTableVPCAssociationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVPCAssociationsResponse'
            Lude.<$> ( x Lude..@? "localGatewayRouteTableVpcAssociationSet"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeLocalGatewayRouteTableVPCAssociations
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLocalGatewayRouteTableVPCAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLocalGatewayRouteTableVPCAssociations where
  toQuery DescribeLocalGatewayRouteTableVPCAssociations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "DescribeLocalGatewayRouteTableVpcAssociations" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "LocalGatewayRouteTableVpcAssociationId"
              Lude.<$> localGatewayRouteTableVPCAssociationIds
          ),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeLocalGatewayRouteTableVPCAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVPCAssociationsResponse = DescribeLocalGatewayRouteTableVPCAssociationsResponse'
  { localGatewayRouteTableVPCAssociations ::
      Lude.Maybe
        [LocalGatewayRouteTableVPCAssociation],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeLocalGatewayRouteTableVPCAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'localGatewayRouteTableVPCAssociations' - Information about the associations.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeLocalGatewayRouteTableVPCAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLocalGatewayRouteTableVPCAssociationsResponse
mkDescribeLocalGatewayRouteTableVPCAssociationsResponse
  pResponseStatus_ =
    DescribeLocalGatewayRouteTableVPCAssociationsResponse'
      { localGatewayRouteTableVPCAssociations =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the associations.
--
-- /Note:/ Consider using 'localGatewayRouteTableVPCAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcarsLocalGatewayRouteTableVPCAssociations :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociationsResponse (Lude.Maybe [LocalGatewayRouteTableVPCAssociation])
dlgrtvpcarsLocalGatewayRouteTableVPCAssociations = Lens.lens (localGatewayRouteTableVPCAssociations :: DescribeLocalGatewayRouteTableVPCAssociationsResponse -> Lude.Maybe [LocalGatewayRouteTableVPCAssociation]) (\s a -> s {localGatewayRouteTableVPCAssociations = a} :: DescribeLocalGatewayRouteTableVPCAssociationsResponse)
{-# DEPRECATED dlgrtvpcarsLocalGatewayRouteTableVPCAssociations "Use generic-lens or generic-optics with 'localGatewayRouteTableVPCAssociations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcarsNextToken :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociationsResponse (Lude.Maybe Lude.Text)
dlgrtvpcarsNextToken = Lens.lens (nextToken :: DescribeLocalGatewayRouteTableVPCAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLocalGatewayRouteTableVPCAssociationsResponse)
{-# DEPRECATED dlgrtvpcarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrtvpcarsResponseStatus :: Lens.Lens' DescribeLocalGatewayRouteTableVPCAssociationsResponse Lude.Int
dlgrtvpcarsResponseStatus = Lens.lens (responseStatus :: DescribeLocalGatewayRouteTableVPCAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLocalGatewayRouteTableVPCAssociationsResponse)
{-# DEPRECATED dlgrtvpcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
