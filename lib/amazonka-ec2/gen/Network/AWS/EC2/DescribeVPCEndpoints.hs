{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC endpoints.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCEndpoints
  ( -- * Creating a request
    DescribeVPCEndpoints (..),
    mkDescribeVPCEndpoints,

    -- ** Request lenses
    dvpceFilters,
    dvpceNextToken,
    dvpceVPCEndpointIds,
    dvpceDryRun,
    dvpceMaxResults,

    -- * Destructuring the response
    DescribeVPCEndpointsResponse (..),
    mkDescribeVPCEndpointsResponse,

    -- ** Response lenses
    dvpcersNextToken,
    dvpcersVPCEndpoints,
    dvpcersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeVpcEndpoints.
--
-- /See:/ 'mkDescribeVPCEndpoints' smart constructor.
data DescribeVPCEndpoints = DescribeVPCEndpoints'
  { filters ::
      Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    vpcEndpointIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpoints' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @vpc-id@ - The ID of the VPC in which the endpoint resides.
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ | @Gateway@ | @GatewayLoadBalancer@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
-- * 'maxResults' - The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a prior call.)
-- * 'vpcEndpointIds' - One or more endpoint IDs.
mkDescribeVPCEndpoints ::
  DescribeVPCEndpoints
mkDescribeVPCEndpoints =
  DescribeVPCEndpoints'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      vpcEndpointIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @service-name@ - The name of the service.
--
--
--     * @vpc-id@ - The ID of the VPC in which the endpoint resides.
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-type@ - The type of VPC endpoint (@Interface@ | @Gateway@ | @GatewayLoadBalancer@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpceFilters :: Lens.Lens' DescribeVPCEndpoints (Lude.Maybe [Filter])
dvpceFilters = Lens.lens (filters :: DescribeVPCEndpoints -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCEndpoints)
{-# DEPRECATED dvpceFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a prior call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpceNextToken :: Lens.Lens' DescribeVPCEndpoints (Lude.Maybe Lude.Text)
dvpceNextToken = Lens.lens (nextToken :: DescribeVPCEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpoints)
{-# DEPRECATED dvpceNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One or more endpoint IDs.
--
-- /Note:/ Consider using 'vpcEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpceVPCEndpointIds :: Lens.Lens' DescribeVPCEndpoints (Lude.Maybe [Lude.Text])
dvpceVPCEndpointIds = Lens.lens (vpcEndpointIds :: DescribeVPCEndpoints -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcEndpointIds = a} :: DescribeVPCEndpoints)
{-# DEPRECATED dvpceVPCEndpointIds "Use generic-lens or generic-optics with 'vpcEndpointIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpceDryRun :: Lens.Lens' DescribeVPCEndpoints (Lude.Maybe Lude.Bool)
dvpceDryRun = Lens.lens (dryRun :: DescribeVPCEndpoints -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCEndpoints)
{-# DEPRECATED dvpceDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of items to return for this request. The request returns a token that you can specify in a subsequent call to get the next set of results.
--
-- Constraint: If the value is greater than 1,000, we return only 1,000 items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpceMaxResults :: Lens.Lens' DescribeVPCEndpoints (Lude.Maybe Lude.Int)
dvpceMaxResults = Lens.lens (maxResults :: DescribeVPCEndpoints -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVPCEndpoints)
{-# DEPRECATED dvpceMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. dvpcersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvpcersVPCEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvpceNextToken Lens..~ rs Lens.^. dvpcersNextToken

instance Lude.AWSRequest DescribeVPCEndpoints where
  type Rs DescribeVPCEndpoints = DescribeVPCEndpointsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCEndpointsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "vpcEndpointSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCEndpoints where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCEndpoints where
  toQuery DescribeVPCEndpoints' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVpcEndpoints" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "VpcEndpointId" Lude.<$> vpcEndpointIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | Contains the output of DescribeVpcEndpoints.
--
-- /See:/ 'mkDescribeVPCEndpointsResponse' smart constructor.
data DescribeVPCEndpointsResponse = DescribeVPCEndpointsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    vpcEndpoints ::
      Lude.Maybe [VPCEndpoint],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'vpcEndpoints' - Information about the endpoints.
mkDescribeVPCEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCEndpointsResponse
mkDescribeVPCEndpointsResponse pResponseStatus_ =
  DescribeVPCEndpointsResponse'
    { nextToken = Lude.Nothing,
      vpcEndpoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcersNextToken :: Lens.Lens' DescribeVPCEndpointsResponse (Lude.Maybe Lude.Text)
dvpcersNextToken = Lens.lens (nextToken :: DescribeVPCEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointsResponse)
{-# DEPRECATED dvpcersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the endpoints.
--
-- /Note:/ Consider using 'vpcEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcersVPCEndpoints :: Lens.Lens' DescribeVPCEndpointsResponse (Lude.Maybe [VPCEndpoint])
dvpcersVPCEndpoints = Lens.lens (vpcEndpoints :: DescribeVPCEndpointsResponse -> Lude.Maybe [VPCEndpoint]) (\s a -> s {vpcEndpoints = a} :: DescribeVPCEndpointsResponse)
{-# DEPRECATED dvpcersVPCEndpoints "Use generic-lens or generic-optics with 'vpcEndpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcersResponseStatus :: Lens.Lens' DescribeVPCEndpointsResponse Lude.Int
dvpcersResponseStatus = Lens.lens (responseStatus :: DescribeVPCEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCEndpointsResponse)
{-# DEPRECATED dvpcersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
