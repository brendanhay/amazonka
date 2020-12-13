{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVPNEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Client VPN endpoints in the account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNEndpoints
  ( -- * Creating a request
    DescribeClientVPNEndpoints (..),
    mkDescribeClientVPNEndpoints,

    -- ** Request lenses
    dcvpneFilters,
    dcvpneClientVPNEndpointIds,
    dcvpneNextToken,
    dcvpneDryRun,
    dcvpneMaxResults,

    -- * Destructuring the response
    DescribeClientVPNEndpointsResponse (..),
    mkDescribeClientVPNEndpointsResponse,

    -- ** Response lenses
    dcvpnersNextToken,
    dcvpnersClientVPNEndpoints,
    dcvpnersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClientVPNEndpoints' smart constructor.
data DescribeClientVPNEndpoints = DescribeClientVPNEndpoints'
  { -- | One or more filters. Filter names and values are case-sensitive.
    --
    --
    --     * @endpoint-id@ - The ID of the Client VPN endpoint.
    --
    --
    --     * @transport-protocol@ - The transport protocol (@tcp@ | @udp@ ).
    filters :: Lude.Maybe [Filter],
    -- | The ID of the Client VPN endpoint.
    clientVPNEndpointIds :: Lude.Maybe [Lude.Text],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNEndpoints' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @endpoint-id@ - The ID of the Client VPN endpoint.
--
--
--     * @transport-protocol@ - The transport protocol (@tcp@ | @udp@ ).
--
--
-- * 'clientVPNEndpointIds' - The ID of the Client VPN endpoint.
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
mkDescribeClientVPNEndpoints ::
  DescribeClientVPNEndpoints
mkDescribeClientVPNEndpoints =
  DescribeClientVPNEndpoints'
    { filters = Lude.Nothing,
      clientVPNEndpointIds = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @endpoint-id@ - The ID of the Client VPN endpoint.
--
--
--     * @transport-protocol@ - The transport protocol (@tcp@ | @udp@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpneFilters :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe [Filter])
dcvpneFilters = Lens.lens (filters :: DescribeClientVPNEndpoints -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcvpneFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpneClientVPNEndpointIds :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe [Lude.Text])
dcvpneClientVPNEndpointIds = Lens.lens (clientVPNEndpointIds :: DescribeClientVPNEndpoints -> Lude.Maybe [Lude.Text]) (\s a -> s {clientVPNEndpointIds = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcvpneClientVPNEndpointIds "Use generic-lens or generic-optics with 'clientVPNEndpointIds' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpneNextToken :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe Lude.Text)
dcvpneNextToken = Lens.lens (nextToken :: DescribeClientVPNEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcvpneNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpneDryRun :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe Lude.Bool)
dcvpneDryRun = Lens.lens (dryRun :: DescribeClientVPNEndpoints -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcvpneDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpneMaxResults :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe Lude.Natural)
dcvpneMaxResults = Lens.lens (maxResults :: DescribeClientVPNEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcvpneMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeClientVPNEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. dcvpnersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcvpnersClientVPNEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcvpneNextToken Lens..~ rs Lens.^. dcvpnersNextToken

instance Lude.AWSRequest DescribeClientVPNEndpoints where
  type
    Rs DescribeClientVPNEndpoints =
      DescribeClientVPNEndpointsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeClientVPNEndpointsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "clientVpnEndpoint" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClientVPNEndpoints where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClientVPNEndpoints where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClientVPNEndpoints where
  toQuery DescribeClientVPNEndpoints' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClientVpnEndpoints" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          ( Lude.toQueryList "ClientVpnEndpointId"
              Lude.<$> clientVPNEndpointIds
          ),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeClientVPNEndpointsResponse' smart constructor.
data DescribeClientVPNEndpointsResponse = DescribeClientVPNEndpointsResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the Client VPN endpoints.
    clientVPNEndpoints :: Lude.Maybe [ClientVPNEndpoint],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'clientVPNEndpoints' - Information about the Client VPN endpoints.
-- * 'responseStatus' - The response status code.
mkDescribeClientVPNEndpointsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClientVPNEndpointsResponse
mkDescribeClientVPNEndpointsResponse pResponseStatus_ =
  DescribeClientVPNEndpointsResponse'
    { nextToken = Lude.Nothing,
      clientVPNEndpoints = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnersNextToken :: Lens.Lens' DescribeClientVPNEndpointsResponse (Lude.Maybe Lude.Text)
dcvpnersNextToken = Lens.lens (nextToken :: DescribeClientVPNEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNEndpointsResponse)
{-# DEPRECATED dcvpnersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the Client VPN endpoints.
--
-- /Note:/ Consider using 'clientVPNEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnersClientVPNEndpoints :: Lens.Lens' DescribeClientVPNEndpointsResponse (Lude.Maybe [ClientVPNEndpoint])
dcvpnersClientVPNEndpoints = Lens.lens (clientVPNEndpoints :: DescribeClientVPNEndpointsResponse -> Lude.Maybe [ClientVPNEndpoint]) (\s a -> s {clientVPNEndpoints = a} :: DescribeClientVPNEndpointsResponse)
{-# DEPRECATED dcvpnersClientVPNEndpoints "Use generic-lens or generic-optics with 'clientVPNEndpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnersResponseStatus :: Lens.Lens' DescribeClientVPNEndpointsResponse Lude.Int
dcvpnersResponseStatus = Lens.lens (responseStatus :: DescribeClientVPNEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClientVPNEndpointsResponse)
{-# DEPRECATED dcvpnersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
