{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the route tables to which the specified resource attachment propagates routes.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
  ( -- * Creating a request
    GetTransitGatewayAttachmentPropagations (..),
    mkGetTransitGatewayAttachmentPropagations,

    -- ** Request lenses
    gtgapFilters,
    gtgapNextToken,
    gtgapDryRun,
    gtgapMaxResults,
    gtgapTransitGatewayAttachmentId,

    -- * Destructuring the response
    GetTransitGatewayAttachmentPropagationsResponse (..),
    mkGetTransitGatewayAttachmentPropagationsResponse,

    -- ** Response lenses
    gtgaprsNextToken,
    gtgaprsTransitGatewayAttachmentPropagations,
    gtgaprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTransitGatewayAttachmentPropagations' smart constructor.
data GetTransitGatewayAttachmentPropagations = GetTransitGatewayAttachmentPropagations'
  { filters ::
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
        Lude.Natural,
    transitGatewayAttachmentId ::
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

-- | Creates a value of 'GetTransitGatewayAttachmentPropagations' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - One or more filters. The possible values are:
--
--
--     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'transitGatewayAttachmentId' - The ID of the attachment.
mkGetTransitGatewayAttachmentPropagations ::
  -- | 'transitGatewayAttachmentId'
  Lude.Text ->
  GetTransitGatewayAttachmentPropagations
mkGetTransitGatewayAttachmentPropagations
  pTransitGatewayAttachmentId_ =
    GetTransitGatewayAttachmentPropagations'
      { filters = Lude.Nothing,
        nextToken = Lude.Nothing,
        dryRun = Lude.Nothing,
        maxResults = Lude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | One or more filters. The possible values are:
--
--
--     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapFilters :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Lude.Maybe [Filter])
gtgapFilters = Lens.lens (filters :: GetTransitGatewayAttachmentPropagations -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: GetTransitGatewayAttachmentPropagations)
{-# DEPRECATED gtgapFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapNextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Lude.Maybe Lude.Text)
gtgapNextToken = Lens.lens (nextToken :: GetTransitGatewayAttachmentPropagations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayAttachmentPropagations)
{-# DEPRECATED gtgapNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapDryRun :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Lude.Maybe Lude.Bool)
gtgapDryRun = Lens.lens (dryRun :: GetTransitGatewayAttachmentPropagations -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetTransitGatewayAttachmentPropagations)
{-# DEPRECATED gtgapDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapMaxResults :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Lude.Maybe Lude.Natural)
gtgapMaxResults = Lens.lens (maxResults :: GetTransitGatewayAttachmentPropagations -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetTransitGatewayAttachmentPropagations)
{-# DEPRECATED gtgapMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgapTransitGatewayAttachmentId :: Lens.Lens' GetTransitGatewayAttachmentPropagations Lude.Text
gtgapTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: GetTransitGatewayAttachmentPropagations -> Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: GetTransitGatewayAttachmentPropagations)
{-# DEPRECATED gtgapTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Page.AWSPager GetTransitGatewayAttachmentPropagations where
  page rq rs
    | Page.stop (rs Lens.^. gtgaprsNextToken) = Lude.Nothing
    | Page.stop
        (rs Lens.^. gtgaprsTransitGatewayAttachmentPropagations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtgapNextToken Lens..~ rs Lens.^. gtgaprsNextToken

instance Lude.AWSRequest GetTransitGatewayAttachmentPropagations where
  type
    Rs GetTransitGatewayAttachmentPropagations =
      GetTransitGatewayAttachmentPropagationsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetTransitGatewayAttachmentPropagationsResponse'
            Lude.<$> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "transitGatewayAttachmentPropagations"
                         Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTransitGatewayAttachmentPropagations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTransitGatewayAttachmentPropagations where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTransitGatewayAttachmentPropagations where
  toQuery GetTransitGatewayAttachmentPropagations' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetTransitGatewayAttachmentPropagations" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "TransitGatewayAttachmentId" Lude.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'mkGetTransitGatewayAttachmentPropagationsResponse' smart constructor.
data GetTransitGatewayAttachmentPropagationsResponse = GetTransitGatewayAttachmentPropagationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    transitGatewayAttachmentPropagations ::
      Lude.Maybe
        [TransitGatewayAttachmentPropagation],
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

-- | Creates a value of 'GetTransitGatewayAttachmentPropagationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
-- * 'transitGatewayAttachmentPropagations' - Information about the propagation route tables.
mkGetTransitGatewayAttachmentPropagationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTransitGatewayAttachmentPropagationsResponse
mkGetTransitGatewayAttachmentPropagationsResponse pResponseStatus_ =
  GetTransitGatewayAttachmentPropagationsResponse'
    { nextToken =
        Lude.Nothing,
      transitGatewayAttachmentPropagations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprsNextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Lude.Maybe Lude.Text)
gtgaprsNextToken = Lens.lens (nextToken :: GetTransitGatewayAttachmentPropagationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTransitGatewayAttachmentPropagationsResponse)
{-# DEPRECATED gtgaprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the propagation route tables.
--
-- /Note:/ Consider using 'transitGatewayAttachmentPropagations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprsTransitGatewayAttachmentPropagations :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Lude.Maybe [TransitGatewayAttachmentPropagation])
gtgaprsTransitGatewayAttachmentPropagations = Lens.lens (transitGatewayAttachmentPropagations :: GetTransitGatewayAttachmentPropagationsResponse -> Lude.Maybe [TransitGatewayAttachmentPropagation]) (\s a -> s {transitGatewayAttachmentPropagations = a} :: GetTransitGatewayAttachmentPropagationsResponse)
{-# DEPRECATED gtgaprsTransitGatewayAttachmentPropagations "Use generic-lens or generic-optics with 'transitGatewayAttachmentPropagations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtgaprsResponseStatus :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse Lude.Int
gtgaprsResponseStatus = Lens.lens (responseStatus :: GetTransitGatewayAttachmentPropagationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTransitGatewayAttachmentPropagationsResponse)
{-# DEPRECATED gtgaprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
