{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClientVPNTargetNetworks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target networks associated with the specified Client VPN endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNTargetNetworks
  ( -- * Creating a request
    DescribeClientVPNTargetNetworks (..),
    mkDescribeClientVPNTargetNetworks,

    -- ** Request lenses
    dcvtnFilters,
    dcvtnNextToken,
    dcvtnAssociationIds,
    dcvtnDryRun,
    dcvtnMaxResults,
    dcvtnClientVPNEndpointId,

    -- * Destructuring the response
    DescribeClientVPNTargetNetworksResponse (..),
    mkDescribeClientVPNTargetNetworksResponse,

    -- ** Response lenses
    dcvtnrsClientVPNTargetNetworks,
    dcvtnrsNextToken,
    dcvtnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClientVPNTargetNetworks' smart constructor.
data DescribeClientVPNTargetNetworks = DescribeClientVPNTargetNetworks'
  { filters ::
      Lude.Maybe [Filter],
    nextToken ::
      Lude.Maybe Lude.Text,
    associationIds ::
      Lude.Maybe [Lude.Text],
    dryRun ::
      Lude.Maybe Lude.Bool,
    maxResults ::
      Lude.Maybe Lude.Natural,
    clientVPNEndpointId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClientVPNTargetNetworks' with the minimum fields required to make a request.
--
-- * 'associationIds' - The IDs of the target network associations.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @association-id@ - The ID of the association.
--
--
--     * @target-network-id@ - The ID of the subnet specified as the target network.
--
--
--     * @vpc-id@ - The ID of the VPC in which the target network is located.
--
--
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
-- * 'nextToken' - The token to retrieve the next page of results.
mkDescribeClientVPNTargetNetworks ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  DescribeClientVPNTargetNetworks
mkDescribeClientVPNTargetNetworks pClientVPNEndpointId_ =
  DescribeClientVPNTargetNetworks'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      associationIds = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_
    }

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @association-id@ - The ID of the association.
--
--
--     * @target-network-id@ - The ID of the subnet specified as the target network.
--
--
--     * @vpc-id@ - The ID of the VPC in which the target network is located.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnFilters :: Lens.Lens' DescribeClientVPNTargetNetworks (Lude.Maybe [Filter])
dcvtnFilters = Lens.lens (filters :: DescribeClientVPNTargetNetworks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeClientVPNTargetNetworks)
{-# DEPRECATED dcvtnFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnNextToken :: Lens.Lens' DescribeClientVPNTargetNetworks (Lude.Maybe Lude.Text)
dcvtnNextToken = Lens.lens (nextToken :: DescribeClientVPNTargetNetworks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNTargetNetworks)
{-# DEPRECATED dcvtnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the target network associations.
--
-- /Note:/ Consider using 'associationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnAssociationIds :: Lens.Lens' DescribeClientVPNTargetNetworks (Lude.Maybe [Lude.Text])
dcvtnAssociationIds = Lens.lens (associationIds :: DescribeClientVPNTargetNetworks -> Lude.Maybe [Lude.Text]) (\s a -> s {associationIds = a} :: DescribeClientVPNTargetNetworks)
{-# DEPRECATED dcvtnAssociationIds "Use generic-lens or generic-optics with 'associationIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnDryRun :: Lens.Lens' DescribeClientVPNTargetNetworks (Lude.Maybe Lude.Bool)
dcvtnDryRun = Lens.lens (dryRun :: DescribeClientVPNTargetNetworks -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeClientVPNTargetNetworks)
{-# DEPRECATED dcvtnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnMaxResults :: Lens.Lens' DescribeClientVPNTargetNetworks (Lude.Maybe Lude.Natural)
dcvtnMaxResults = Lens.lens (maxResults :: DescribeClientVPNTargetNetworks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeClientVPNTargetNetworks)
{-# DEPRECATED dcvtnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnClientVPNEndpointId :: Lens.Lens' DescribeClientVPNTargetNetworks Lude.Text
dcvtnClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DescribeClientVPNTargetNetworks -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DescribeClientVPNTargetNetworks)
{-# DEPRECATED dcvtnClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

instance Page.AWSPager DescribeClientVPNTargetNetworks where
  page rq rs
    | Page.stop (rs Lens.^. dcvtnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dcvtnrsClientVPNTargetNetworks) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dcvtnNextToken Lens..~ rs Lens.^. dcvtnrsNextToken

instance Lude.AWSRequest DescribeClientVPNTargetNetworks where
  type
    Rs DescribeClientVPNTargetNetworks =
      DescribeClientVPNTargetNetworksResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeClientVPNTargetNetworksResponse'
            Lude.<$> ( x Lude..@? "clientVpnTargetNetworks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClientVPNTargetNetworks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeClientVPNTargetNetworks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClientVPNTargetNetworks where
  toQuery DescribeClientVPNTargetNetworks' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeClientVpnTargetNetworks" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        Lude.toQuery
          (Lude.toQueryList "AssociationIds" Lude.<$> associationIds),
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId
      ]

-- | /See:/ 'mkDescribeClientVPNTargetNetworksResponse' smart constructor.
data DescribeClientVPNTargetNetworksResponse = DescribeClientVPNTargetNetworksResponse'
  { clientVPNTargetNetworks ::
      Lude.Maybe
        [TargetNetwork],
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

-- | Creates a value of 'DescribeClientVPNTargetNetworksResponse' with the minimum fields required to make a request.
--
-- * 'clientVPNTargetNetworks' - Information about the associated target networks.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeClientVPNTargetNetworksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClientVPNTargetNetworksResponse
mkDescribeClientVPNTargetNetworksResponse pResponseStatus_ =
  DescribeClientVPNTargetNetworksResponse'
    { clientVPNTargetNetworks =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the associated target networks.
--
-- /Note:/ Consider using 'clientVPNTargetNetworks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrsClientVPNTargetNetworks :: Lens.Lens' DescribeClientVPNTargetNetworksResponse (Lude.Maybe [TargetNetwork])
dcvtnrsClientVPNTargetNetworks = Lens.lens (clientVPNTargetNetworks :: DescribeClientVPNTargetNetworksResponse -> Lude.Maybe [TargetNetwork]) (\s a -> s {clientVPNTargetNetworks = a} :: DescribeClientVPNTargetNetworksResponse)
{-# DEPRECATED dcvtnrsClientVPNTargetNetworks "Use generic-lens or generic-optics with 'clientVPNTargetNetworks' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrsNextToken :: Lens.Lens' DescribeClientVPNTargetNetworksResponse (Lude.Maybe Lude.Text)
dcvtnrsNextToken = Lens.lens (nextToken :: DescribeClientVPNTargetNetworksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeClientVPNTargetNetworksResponse)
{-# DEPRECATED dcvtnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvtnrsResponseStatus :: Lens.Lens' DescribeClientVPNTargetNetworksResponse Lude.Int
dcvtnrsResponseStatus = Lens.lens (responseStatus :: DescribeClientVPNTargetNetworksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClientVPNTargetNetworksResponse)
{-# DEPRECATED dcvtnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
