{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the associations between your Direct Connect gateways and virtual private gateways. You must specify a Direct Connect gateway, a virtual private gateway, or both. If you specify a Direct Connect gateway, the response contains all virtual private gateways associated with the Direct Connect gateway. If you specify a virtual private gateway, the response contains all Direct Connect gateways associated with the virtual private gateway. If you specify both, the response contains the association between the Direct Connect gateway and the virtual private gateway.
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
  ( -- * Creating a request
    DescribeDirectConnectGatewayAssociations (..),
    mkDescribeDirectConnectGatewayAssociations,

    -- ** Request lenses
    ddcgaVirtualGatewayId,
    ddcgaAssociationId,
    ddcgaAssociatedGatewayId,
    ddcgaDirectConnectGatewayId,
    ddcgaNextToken,
    ddcgaMaxResults,

    -- * Destructuring the response
    DescribeDirectConnectGatewayAssociationsResponse (..),
    mkDescribeDirectConnectGatewayAssociationsResponse,

    -- ** Response lenses
    ddcgarsNextToken,
    ddcgarsDirectConnectGatewayAssociations,
    ddcgarsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDirectConnectGatewayAssociations' smart constructor.
data DescribeDirectConnectGatewayAssociations = DescribeDirectConnectGatewayAssociations'
  { virtualGatewayId ::
      Lude.Maybe
        Lude.Text,
    associationId ::
      Lude.Maybe
        Lude.Text,
    associatedGatewayId ::
      Lude.Maybe
        Lude.Text,
    directConnectGatewayId ::
      Lude.Maybe
        Lude.Text,
    nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
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

-- | Creates a value of 'DescribeDirectConnectGatewayAssociations' with the minimum fields required to make a request.
--
-- * 'associatedGatewayId' - The ID of the associated gateway.
-- * 'associationId' - The ID of the Direct Connect gateway association.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
-- * 'nextToken' - The token provided in the previous call to retrieve the next page.
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
mkDescribeDirectConnectGatewayAssociations ::
  DescribeDirectConnectGatewayAssociations
mkDescribeDirectConnectGatewayAssociations =
  DescribeDirectConnectGatewayAssociations'
    { virtualGatewayId =
        Lude.Nothing,
      associationId = Lude.Nothing,
      associatedGatewayId = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaVirtualGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgaVirtualGatewayId = Lens.lens (virtualGatewayId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgaVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaAssociationId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgaAssociationId = Lens.lens (associationId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'associatedGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaAssociatedGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgaAssociatedGatewayId = Lens.lens (associatedGatewayId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {associatedGatewayId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgaAssociatedGatewayId "Use generic-lens or generic-optics with 'associatedGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgaDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgaDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The token provided in the previous call to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgaNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaMaxResults :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Int)
ddcgaMaxResults = Lens.lens (maxResults :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeDirectConnectGatewayAssociations where
  page rq rs
    | Page.stop (rs Lens.^. ddcgarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcgarsDirectConnectGatewayAssociations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcgaNextToken Lens..~ rs Lens.^. ddcgarsNextToken

instance Lude.AWSRequest DescribeDirectConnectGatewayAssociations where
  type
    Rs DescribeDirectConnectGatewayAssociations =
      DescribeDirectConnectGatewayAssociationsResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAssociationsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> ( x Lude..?> "directConnectGatewayAssociations"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDirectConnectGatewayAssociations where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.DescribeDirectConnectGatewayAssociations" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDirectConnectGatewayAssociations where
  toJSON DescribeDirectConnectGatewayAssociations' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("virtualGatewayId" Lude..=) Lude.<$> virtualGatewayId,
            ("associationId" Lude..=) Lude.<$> associationId,
            ("associatedGatewayId" Lude..=) Lude.<$> associatedGatewayId,
            ("directConnectGatewayId" Lude..=) Lude.<$> directConnectGatewayId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeDirectConnectGatewayAssociations where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDirectConnectGatewayAssociations where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDirectConnectGatewayAssociationsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationsResponse = DescribeDirectConnectGatewayAssociationsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    directConnectGatewayAssociations ::
      Lude.Maybe
        [DirectConnectGatewayAssociation],
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

-- | Creates a value of 'DescribeDirectConnectGatewayAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociations' - Information about the associations.
-- * 'nextToken' - The token to retrieve the next page.
-- * 'responseStatus' - The response status code.
mkDescribeDirectConnectGatewayAssociationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDirectConnectGatewayAssociationsResponse
mkDescribeDirectConnectGatewayAssociationsResponse pResponseStatus_ =
  DescribeDirectConnectGatewayAssociationsResponse'
    { nextToken =
        Lude.Nothing,
      directConnectGatewayAssociations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgarsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Lude.Maybe Lude.Text)
ddcgarsNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationsResponse)
{-# DEPRECATED ddcgarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the associations.
--
-- /Note:/ Consider using 'directConnectGatewayAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgarsDirectConnectGatewayAssociations :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Lude.Maybe [DirectConnectGatewayAssociation])
ddcgarsDirectConnectGatewayAssociations = Lens.lens (directConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociationsResponse -> Lude.Maybe [DirectConnectGatewayAssociation]) (\s a -> s {directConnectGatewayAssociations = a} :: DescribeDirectConnectGatewayAssociationsResponse)
{-# DEPRECATED ddcgarsDirectConnectGatewayAssociations "Use generic-lens or generic-optics with 'directConnectGatewayAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgarsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse Lude.Int
ddcgarsResponseStatus = Lens.lens (responseStatus :: DescribeDirectConnectGatewayAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDirectConnectGatewayAssociationsResponse)
{-# DEPRECATED ddcgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
