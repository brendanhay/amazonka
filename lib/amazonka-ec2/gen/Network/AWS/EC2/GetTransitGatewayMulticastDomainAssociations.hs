{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the transit gateway multicast domain.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
  ( -- * Creating a request
    GetTransitGatewayMulticastDomainAssociations (..),
    mkGetTransitGatewayMulticastDomainAssociations,

    -- ** Request lenses
    gtgmdaFilters,
    gtgmdaTransitGatewayMulticastDomainId,
    gtgmdaNextToken,
    gtgmdaDryRun,
    gtgmdaMaxResults,

    -- * Destructuring the response
    GetTransitGatewayMulticastDomainAssociationsResponse (..),
    mkGetTransitGatewayMulticastDomainAssociationsResponse,

    -- ** Response lenses
    gtgmdarsNextToken,
    gtgmdarsMulticastDomainAssociations,
    gtgmdarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTransitGatewayMulticastDomainAssociations' smart constructor.
data GetTransitGatewayMulticastDomainAssociations = GetTransitGatewayMulticastDomainAssociations'
  { filters ::
      Lude.Maybe
        [Filter],
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

-- | Creates a value of 'GetTransitGatewayMulticastDomainAssociations' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The type of resource. The valid value is: @vpc@ .
--
--
--     * @state@ - The state of the subnet association. Valid values are @associated@ | @associating@ | @disassociated@ | @disassociating@ .
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
mkGetTransitGatewayMulticastDomainAssociations ::
  GetTransitGatewayMulticastDomainAssociations
mkGetTransitGatewayMulticastDomainAssociations =
  GetTransitGatewayMulticastDomainAssociations'
    { filters =
        Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. The possible values are:
--
--
--     * @resource-id@ - The ID of the resource.
--
--
--     * @resource-type@ - The type of resource. The valid value is: @vpc@ .
--
--
--     * @state@ - The state of the subnet association. Valid values are @associated@ | @associating@ | @disassociated@ | @disassociating@ .
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
gtgmdaFilters :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Lude.Maybe [Filter])
gtgmdaFilters = Lens.lens (filters :: GetTransitGatewayMulticastDomainAssociations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: GetTransitGatewayMulticastDomainAssociations)
{-# DEPRECATED gtgmdaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaTransitGatewayMulticastDomainId :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Lude.Maybe Lude.Text)
gtgmdaTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: GetTransitGatewayMulticastDomainAssociations -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: GetTransitGatewayMulticastDomainAssociations)
{-# DEPRECATED gtgmdaTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaNextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Lude.Maybe Lude.Text)
gtgmdaNextToken = Lens.lens (nextToken :: GetTransitGatewayMulticastDomainAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociations)
{-# DEPRECATED gtgmdaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaDryRun :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Lude.Maybe Lude.Bool)
gtgmdaDryRun = Lens.lens (dryRun :: GetTransitGatewayMulticastDomainAssociations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetTransitGatewayMulticastDomainAssociations)
{-# DEPRECATED gtgmdaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdaMaxResults :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Lude.Maybe Lude.Natural)
gtgmdaMaxResults = Lens.lens (maxResults :: GetTransitGatewayMulticastDomainAssociations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTransitGatewayMulticastDomainAssociations)
{-# DEPRECATED gtgmdaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager GetTransitGatewayMulticastDomainAssociations where
  page rq rs
    | Page.stop (rs Lens.^. gtgmdarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtgmdarsMulticastDomainAssociations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtgmdaNextToken Lens..~ rs Lens.^. gtgmdarsNextToken

instance
  Lude.AWSRequest
    GetTransitGatewayMulticastDomainAssociations
  where
  type
    Rs GetTransitGatewayMulticastDomainAssociations =
      GetTransitGatewayMulticastDomainAssociationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTransitGatewayMulticastDomainAssociationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "multicastDomainAssociations" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    GetTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTransitGatewayMulticastDomainAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTransitGatewayMulticastDomainAssociations where
  toQuery GetTransitGatewayMulticastDomainAssociations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "GetTransitGatewayMulticastDomainAssociations" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "TransitGatewayMulticastDomainId"
          Lude.=: transitGatewayMulticastDomainId,
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data GetTransitGatewayMulticastDomainAssociationsResponse = GetTransitGatewayMulticastDomainAssociationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    multicastDomainAssociations ::
      Lude.Maybe
        [TransitGatewayMulticastDomainAssociation],
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

-- | Creates a value of 'GetTransitGatewayMulticastDomainAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'multicastDomainAssociations' - Information about the multicast domain associations.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkGetTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTransitGatewayMulticastDomainAssociationsResponse
mkGetTransitGatewayMulticastDomainAssociationsResponse
  pResponseStatus_ =
    GetTransitGatewayMulticastDomainAssociationsResponse'
      { nextToken =
          Lude.Nothing,
        multicastDomainAssociations =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarsNextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Lude.Maybe Lude.Text)
gtgmdarsNextToken = Lens.lens (nextToken :: GetTransitGatewayMulticastDomainAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)
{-# DEPRECATED gtgmdarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the multicast domain associations.
--
-- /Note:/ Consider using 'multicastDomainAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarsMulticastDomainAssociations :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Lude.Maybe [TransitGatewayMulticastDomainAssociation])
gtgmdarsMulticastDomainAssociations = Lens.lens (multicastDomainAssociations :: GetTransitGatewayMulticastDomainAssociationsResponse -> Lude.Maybe [TransitGatewayMulticastDomainAssociation]) (\s a -> s {multicastDomainAssociations = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)
{-# DEPRECATED gtgmdarsMulticastDomainAssociations "Use generic-lens or generic-optics with 'multicastDomainAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgmdarsResponseStatus :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse Lude.Int
gtgmdarsResponseStatus = Lens.lens (responseStatus :: GetTransitGatewayMulticastDomainAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)
{-# DEPRECATED gtgmdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
