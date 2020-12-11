{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dcveFilters,
    dcveClientVPNEndpointIds,
    dcveNextToken,
    dcveDryRun,
    dcveMaxResults,

    -- * Destructuring the response
    DescribeClientVPNEndpointsResponse (..),
    mkDescribeClientVPNEndpointsResponse,

    -- ** Response lenses
    dcversNextToken,
    dcversClientVPNEndpoints,
    dcversResponseStatus,
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
  { filters ::
      Lude.Maybe [Filter],
    clientVPNEndpointIds ::
      Lude.Maybe [Lude.Text],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNEndpoints' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointIds' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @endpoint-id@ - The ID of the Client VPN endpoint.
--
--
--     * @transport-protocol@ - The transport protocol (@tcp@ | @udp@ ).
--
--
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
-- * 'nextToken' - The token to retrieve the next page of results.
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
dcveFilters :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe [Filter])
dcveFilters = Lens.lens (filters :: DescribeClientVPNEndpoints -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcveFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveClientVPNEndpointIds :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe [Lude.Text])
dcveClientVPNEndpointIds = Lens.lens (clientVPNEndpointIds :: DescribeClientVPNEndpoints -> Lude.Maybe [Lude.Text]) (\s a -> s {clientVPNEndpointIds = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcveClientVPNEndpointIds "Use generic-lens or generic-optics with 'clientVPNEndpointIds' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveNextToken :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe Lude.Text)
dcveNextToken = Lens.lens (nextToken :: DescribeClientVPNEndpoints -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcveNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveDryRun :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe Lude.Bool)
dcveDryRun = Lens.lens (dryRun :: DescribeClientVPNEndpoints -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcveDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveMaxResults :: Lens.Lens' DescribeClientVPNEndpoints (Lude.Maybe Lude.Natural)
dcveMaxResults = Lens.lens (maxResults :: DescribeClientVPNEndpoints -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClientVPNEndpoints)
{-# DEPRECATED dcveMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeClientVPNEndpoints where
  page rq rs
    | Page.stop (rs Lens.^. dcversNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcversClientVPNEndpoints) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcveNextToken Lens..~ rs Lens.^. dcversNextToken

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
  { nextToken ::
      Lude.Maybe Lude.Text,
    clientVPNEndpoints ::
      Lude.Maybe
        [ClientVPNEndpoint],
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

-- | Creates a value of 'DescribeClientVPNEndpointsResponse' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpoints' - Information about the Client VPN endpoints.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
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
dcversNextToken :: Lens.Lens' DescribeClientVPNEndpointsResponse (Lude.Maybe Lude.Text)
dcversNextToken = Lens.lens (nextToken :: DescribeClientVPNEndpointsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNEndpointsResponse)
{-# DEPRECATED dcversNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the Client VPN endpoints.
--
-- /Note:/ Consider using 'clientVPNEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcversClientVPNEndpoints :: Lens.Lens' DescribeClientVPNEndpointsResponse (Lude.Maybe [ClientVPNEndpoint])
dcversClientVPNEndpoints = Lens.lens (clientVPNEndpoints :: DescribeClientVPNEndpointsResponse -> Lude.Maybe [ClientVPNEndpoint]) (\s a -> s {clientVPNEndpoints = a} :: DescribeClientVPNEndpointsResponse)
{-# DEPRECATED dcversClientVPNEndpoints "Use generic-lens or generic-optics with 'clientVPNEndpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcversResponseStatus :: Lens.Lens' DescribeClientVPNEndpointsResponse Lude.Int
dcversResponseStatus = Lens.lens (responseStatus :: DescribeClientVPNEndpointsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClientVPNEndpointsResponse)
{-# DEPRECATED dcversResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
