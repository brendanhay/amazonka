{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway multicast domains.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayMulticastDomains
  ( -- * Creating a request
    DescribeTransitGatewayMulticastDomains (..),
    mkDescribeTransitGatewayMulticastDomains,

    -- ** Request lenses
    dtgmdsTransitGatewayMulticastDomainIds,
    dtgmdsFilters,
    dtgmdsNextToken,
    dtgmdsDryRun,
    dtgmdsMaxResults,

    -- * Destructuring the response
    DescribeTransitGatewayMulticastDomainsResponse (..),
    mkDescribeTransitGatewayMulticastDomainsResponse,

    -- ** Response lenses
    dtgmdsrsTransitGatewayMulticastDomains,
    dtgmdsrsNextToken,
    dtgmdsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTransitGatewayMulticastDomains' smart constructor.
data DescribeTransitGatewayMulticastDomains = DescribeTransitGatewayMulticastDomains'
  { transitGatewayMulticastDomainIds ::
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTransitGatewayMulticastDomains' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @state@ - The state of the transit gateway multicast domain. Valid values are @pending@ | @available@ | @deleting@ | @deleted@ .
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--     * @transit-gateway-multicast-domain-id@ - The ID of the transit gateway multicast domain.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayMulticastDomainIds' - The ID of the transit gateway multicast domain.
mkDescribeTransitGatewayMulticastDomains ::
  DescribeTransitGatewayMulticastDomains
mkDescribeTransitGatewayMulticastDomains =
  DescribeTransitGatewayMulticastDomains'
    { transitGatewayMulticastDomainIds =
        Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsTransitGatewayMulticastDomainIds :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Lude.Maybe [Lude.Text])
dtgmdsTransitGatewayMulticastDomainIds = Lens.lens (transitGatewayMulticastDomainIds :: DescribeTransitGatewayMulticastDomains -> Lude.Maybe [Lude.Text]) (\s a -> s {transitGatewayMulticastDomainIds = a} :: DescribeTransitGatewayMulticastDomains)
{-# DEPRECATED dtgmdsTransitGatewayMulticastDomainIds "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainIds' instead." #-}

-- | One or more filters. The possible values are:
--
--
--     * @state@ - The state of the transit gateway multicast domain. Valid values are @pending@ | @available@ | @deleting@ | @deleted@ .
--
--
--     * @transit-gateway-id@ - The ID of the transit gateway.
--
--
--     * @transit-gateway-multicast-domain-id@ - The ID of the transit gateway multicast domain.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsFilters :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Lude.Maybe [Filter])
dtgmdsFilters = Lens.lens (filters :: DescribeTransitGatewayMulticastDomains -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeTransitGatewayMulticastDomains)
{-# DEPRECATED dtgmdsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsNextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Lude.Maybe Lude.Text)
dtgmdsNextToken = Lens.lens (nextToken :: DescribeTransitGatewayMulticastDomains -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayMulticastDomains)
{-# DEPRECATED dtgmdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsDryRun :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Lude.Maybe Lude.Bool)
dtgmdsDryRun = Lens.lens (dryRun :: DescribeTransitGatewayMulticastDomains -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeTransitGatewayMulticastDomains)
{-# DEPRECATED dtgmdsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsMaxResults :: Lens.Lens' DescribeTransitGatewayMulticastDomains (Lude.Maybe Lude.Natural)
dtgmdsMaxResults = Lens.lens (maxResults :: DescribeTransitGatewayMulticastDomains -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeTransitGatewayMulticastDomains)
{-# DEPRECATED dtgmdsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeTransitGatewayMulticastDomains where
  page rq rs
    | Page.stop (rs Lens.^. dtgmdsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dtgmdsrsTransitGatewayMulticastDomains) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dtgmdsNextToken Lens..~ rs Lens.^. dtgmdsrsNextToken

instance Lude.AWSRequest DescribeTransitGatewayMulticastDomains where
  type
    Rs DescribeTransitGatewayMulticastDomains =
      DescribeTransitGatewayMulticastDomainsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeTransitGatewayMulticastDomainsResponse'
            Lude.<$> ( x Lude..@? "transitGatewayMulticastDomains" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTransitGatewayMulticastDomains where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTransitGatewayMulticastDomains where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTransitGatewayMulticastDomains where
  toQuery DescribeTransitGatewayMulticastDomains' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTransitGatewayMulticastDomains" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          ( Lude.toQueryList "TransitGatewayMulticastDomainIds"
              Lude.<$> transitGatewayMulticastDomainIds
          ),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeTransitGatewayMulticastDomainsResponse' smart constructor.
data DescribeTransitGatewayMulticastDomainsResponse = DescribeTransitGatewayMulticastDomainsResponse'
  { transitGatewayMulticastDomains ::
      Lude.Maybe
        [TransitGatewayMulticastDomain],
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

-- | Creates a value of 'DescribeTransitGatewayMulticastDomainsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayMulticastDomains' - Information about the transit gateway multicast domains.
mkDescribeTransitGatewayMulticastDomainsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTransitGatewayMulticastDomainsResponse
mkDescribeTransitGatewayMulticastDomainsResponse pResponseStatus_ =
  DescribeTransitGatewayMulticastDomainsResponse'
    { transitGatewayMulticastDomains =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the transit gateway multicast domains.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsrsTransitGatewayMulticastDomains :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Lude.Maybe [TransitGatewayMulticastDomain])
dtgmdsrsTransitGatewayMulticastDomains = Lens.lens (transitGatewayMulticastDomains :: DescribeTransitGatewayMulticastDomainsResponse -> Lude.Maybe [TransitGatewayMulticastDomain]) (\s a -> s {transitGatewayMulticastDomains = a} :: DescribeTransitGatewayMulticastDomainsResponse)
{-# DEPRECATED dtgmdsrsTransitGatewayMulticastDomains "Use generic-lens or generic-optics with 'transitGatewayMulticastDomains' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsrsNextToken :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse (Lude.Maybe Lude.Text)
dtgmdsrsNextToken = Lens.lens (nextToken :: DescribeTransitGatewayMulticastDomainsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeTransitGatewayMulticastDomainsResponse)
{-# DEPRECATED dtgmdsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgmdsrsResponseStatus :: Lens.Lens' DescribeTransitGatewayMulticastDomainsResponse Lude.Int
dtgmdsrsResponseStatus = Lens.lens (responseStatus :: DescribeTransitGatewayMulticastDomainsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTransitGatewayMulticastDomainsResponse)
{-# DEPRECATED dtgmdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
