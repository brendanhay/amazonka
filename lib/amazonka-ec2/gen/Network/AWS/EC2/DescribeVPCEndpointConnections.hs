{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpointConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the VPC endpoint connections to your VPC endpoint services, including any endpoints that are pending your acceptance.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCEndpointConnections
  ( -- * Creating a request
    DescribeVPCEndpointConnections (..),
    mkDescribeVPCEndpointConnections,

    -- ** Request lenses
    dvecFilters,
    dvecNextToken,
    dvecDryRun,
    dvecMaxResults,

    -- * Destructuring the response
    DescribeVPCEndpointConnectionsResponse (..),
    mkDescribeVPCEndpointConnectionsResponse,

    -- ** Response lenses
    dvecrsVPCEndpointConnections,
    dvecrsNextToken,
    dvecrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVPCEndpointConnections' smart constructor.
data DescribeVPCEndpointConnections = DescribeVPCEndpointConnections'
  { -- | One or more filters.
    --
    --
    --     * @service-id@ - The ID of the service.
    --
    --
    --     * @vpc-endpoint-owner@ - The AWS account number of the owner of the endpoint.
    --
    --
    --     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
    --
    --
    --     * @vpc-endpoint-id@ - The ID of the endpoint.
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointConnections' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @vpc-endpoint-owner@ - The AWS account number of the owner of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
mkDescribeVPCEndpointConnections ::
  DescribeVPCEndpointConnections
mkDescribeVPCEndpointConnections =
  DescribeVPCEndpointConnections'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters.
--
--
--     * @service-id@ - The ID of the service.
--
--
--     * @vpc-endpoint-owner@ - The AWS account number of the owner of the endpoint.
--
--
--     * @vpc-endpoint-state@ - The state of the endpoint (@pendingAcceptance@ | @pending@ | @available@ | @deleting@ | @deleted@ | @rejected@ | @failed@ ).
--
--
--     * @vpc-endpoint-id@ - The ID of the endpoint.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecFilters :: Lens.Lens' DescribeVPCEndpointConnections (Lude.Maybe [Filter])
dvecFilters = Lens.lens (filters :: DescribeVPCEndpointConnections -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeVPCEndpointConnections)
{-# DEPRECATED dvecFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecNextToken :: Lens.Lens' DescribeVPCEndpointConnections (Lude.Maybe Lude.Text)
dvecNextToken = Lens.lens (nextToken :: DescribeVPCEndpointConnections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointConnections)
{-# DEPRECATED dvecNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecDryRun :: Lens.Lens' DescribeVPCEndpointConnections (Lude.Maybe Lude.Bool)
dvecDryRun = Lens.lens (dryRun :: DescribeVPCEndpointConnections -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVPCEndpointConnections)
{-# DEPRECATED dvecDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1,000; if @MaxResults@ is given a value larger than 1,000, only 1,000 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecMaxResults :: Lens.Lens' DescribeVPCEndpointConnections (Lude.Maybe Lude.Int)
dvecMaxResults = Lens.lens (maxResults :: DescribeVPCEndpointConnections -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeVPCEndpointConnections)
{-# DEPRECATED dvecMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeVPCEndpointConnections where
  page rq rs
    | Page.stop (rs Lens.^. dvecrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dvecrsVPCEndpointConnections) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvecNextToken Lens..~ rs Lens.^. dvecrsNextToken

instance Lude.AWSRequest DescribeVPCEndpointConnections where
  type
    Rs DescribeVPCEndpointConnections =
      DescribeVPCEndpointConnectionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVPCEndpointConnectionsResponse'
            Lude.<$> ( x Lude..@? "vpcEndpointConnectionSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCEndpointConnections where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVPCEndpointConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCEndpointConnections where
  toQuery DescribeVPCEndpointConnections' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeVpcEndpointConnections" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeVPCEndpointConnectionsResponse' smart constructor.
data DescribeVPCEndpointConnectionsResponse = DescribeVPCEndpointConnectionsResponse'
  { -- | Information about one or more VPC endpoint connections.
    vpcEndpointConnections :: Lude.Maybe [VPCEndpointConnection],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCEndpointConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'vpcEndpointConnections' - Information about one or more VPC endpoint connections.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeVPCEndpointConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCEndpointConnectionsResponse
mkDescribeVPCEndpointConnectionsResponse pResponseStatus_ =
  DescribeVPCEndpointConnectionsResponse'
    { vpcEndpointConnections =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about one or more VPC endpoint connections.
--
-- /Note:/ Consider using 'vpcEndpointConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecrsVPCEndpointConnections :: Lens.Lens' DescribeVPCEndpointConnectionsResponse (Lude.Maybe [VPCEndpointConnection])
dvecrsVPCEndpointConnections = Lens.lens (vpcEndpointConnections :: DescribeVPCEndpointConnectionsResponse -> Lude.Maybe [VPCEndpointConnection]) (\s a -> s {vpcEndpointConnections = a} :: DescribeVPCEndpointConnectionsResponse)
{-# DEPRECATED dvecrsVPCEndpointConnections "Use generic-lens or generic-optics with 'vpcEndpointConnections' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecrsNextToken :: Lens.Lens' DescribeVPCEndpointConnectionsResponse (Lude.Maybe Lude.Text)
dvecrsNextToken = Lens.lens (nextToken :: DescribeVPCEndpointConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeVPCEndpointConnectionsResponse)
{-# DEPRECATED dvecrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecrsResponseStatus :: Lens.Lens' DescribeVPCEndpointConnectionsResponse Lude.Int
dvecrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCEndpointConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCEndpointConnectionsResponse)
{-# DEPRECATED dvecrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
