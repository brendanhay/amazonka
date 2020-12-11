{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVPNConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes active client connections and connections that have been terminated within the last 60 minutes for the specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNConnections
  ( -- * Creating a request
    DescribeClientVPNConnections (..),
    mkDescribeClientVPNConnections,

    -- ** Request lenses
    dcvcFilters,
    dcvcNextToken,
    dcvcDryRun,
    dcvcMaxResults,
    dcvcClientVPNEndpointId,

    -- * Destructuring the response
    DescribeClientVPNConnectionsResponse (..),
    mkDescribeClientVPNConnectionsResponse,

    -- ** Response lenses
    dcvcrsConnections,
    dcvcrsNextToken,
    dcvcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClientVPNConnections' smart constructor.
data DescribeClientVPNConnections = DescribeClientVPNConnections'
  { filters ::
      Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Natural,
    clientVPNEndpointId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNConnections' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @connection-id@ - The ID of the connection.
--
--
--     * @username@ - For Active Directory client authentication, the user name of the client who established the client connection.
--
--
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
-- * 'nextToken' - The token to retrieve the next page of results.
mkDescribeClientVPNConnections ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  DescribeClientVPNConnections
mkDescribeClientVPNConnections pClientVPNEndpointId_ =
  DescribeClientVPNConnections'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_
    }

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @connection-id@ - The ID of the connection.
--
--
--     * @username@ - For Active Directory client authentication, the user name of the client who established the client connection.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcFilters :: Lens.Lens' DescribeClientVPNConnections (Lude.Maybe [Filter])
dcvcFilters = Lens.lens (filters :: DescribeClientVPNConnections -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeClientVPNConnections)
{-# DEPRECATED dcvcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcNextToken :: Lens.Lens' DescribeClientVPNConnections (Lude.Maybe Lude.Text)
dcvcNextToken = Lens.lens (nextToken :: DescribeClientVPNConnections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNConnections)
{-# DEPRECATED dcvcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcDryRun :: Lens.Lens' DescribeClientVPNConnections (Lude.Maybe Lude.Bool)
dcvcDryRun = Lens.lens (dryRun :: DescribeClientVPNConnections -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeClientVPNConnections)
{-# DEPRECATED dcvcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcMaxResults :: Lens.Lens' DescribeClientVPNConnections (Lude.Maybe Lude.Natural)
dcvcMaxResults = Lens.lens (maxResults :: DescribeClientVPNConnections -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClientVPNConnections)
{-# DEPRECATED dcvcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcClientVPNEndpointId :: Lens.Lens' DescribeClientVPNConnections Lude.Text
dcvcClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DescribeClientVPNConnections -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DescribeClientVPNConnections)
{-# DEPRECATED dcvcClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

instance Page.AWSPager DescribeClientVPNConnections where
  page rq rs
    | Page.stop (rs Lens.^. dcvcrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcvcrsConnections) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcvcNextToken Lens..~ rs Lens.^. dcvcrsNextToken

instance Lude.AWSRequest DescribeClientVPNConnections where
  type
    Rs DescribeClientVPNConnections =
      DescribeClientVPNConnectionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeClientVPNConnectionsResponse'
            Lude.<$> ( x Lude..@? "connections" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClientVPNConnections where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClientVPNConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClientVPNConnections where
  toQuery DescribeClientVPNConnections' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClientVpnConnections" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId
      ]

-- | /See:/ 'mkDescribeClientVPNConnectionsResponse' smart constructor.
data DescribeClientVPNConnectionsResponse = DescribeClientVPNConnectionsResponse'
  { connections ::
      Lude.Maybe
        [ClientVPNConnection],
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'connections' - Information about the active and terminated client connections.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeClientVPNConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClientVPNConnectionsResponse
mkDescribeClientVPNConnectionsResponse pResponseStatus_ =
  DescribeClientVPNConnectionsResponse'
    { connections = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the active and terminated client connections.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcrsConnections :: Lens.Lens' DescribeClientVPNConnectionsResponse (Lude.Maybe [ClientVPNConnection])
dcvcrsConnections = Lens.lens (connections :: DescribeClientVPNConnectionsResponse -> Lude.Maybe [ClientVPNConnection]) (\s a -> s {connections = a} :: DescribeClientVPNConnectionsResponse)
{-# DEPRECATED dcvcrsConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcrsNextToken :: Lens.Lens' DescribeClientVPNConnectionsResponse (Lude.Maybe Lude.Text)
dcvcrsNextToken = Lens.lens (nextToken :: DescribeClientVPNConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNConnectionsResponse)
{-# DEPRECATED dcvcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvcrsResponseStatus :: Lens.Lens' DescribeClientVPNConnectionsResponse Lude.Int
dcvcrsResponseStatus = Lens.lens (responseStatus :: DescribeClientVPNConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClientVPNConnectionsResponse)
{-# DEPRECATED dcvcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
