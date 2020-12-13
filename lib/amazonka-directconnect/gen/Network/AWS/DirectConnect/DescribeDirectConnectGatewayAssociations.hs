{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ddcgafVirtualGatewayId,
    ddcgafAssociationId,
    ddcgafAssociatedGatewayId,
    ddcgafDirectConnectGatewayId,
    ddcgafNextToken,
    ddcgafMaxResults,

    -- * Destructuring the response
    DescribeDirectConnectGatewayAssociationsResponse (..),
    mkDescribeDirectConnectGatewayAssociationsResponse,

    -- ** Response lenses
    drsNextToken,
    drsDirectConnectGatewayAssociations,
    drsResponseStatus,
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
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The ID of the associated gateway.
    associatedGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text,
    -- | The token provided in the previous call to retrieve the next page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectConnectGatewayAssociations' with the minimum fields required to make a request.
--
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
-- * 'associationId' - The ID of the Direct Connect gateway association.
-- * 'associatedGatewayId' - The ID of the associated gateway.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'nextToken' - The token provided in the previous call to retrieve the next page.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
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
ddcgafVirtualGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgafVirtualGatewayId = Lens.lens (virtualGatewayId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgafVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafAssociationId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgafAssociationId = Lens.lens (associationId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgafAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the associated gateway.
--
-- /Note:/ Consider using 'associatedGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafAssociatedGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgafAssociatedGatewayId = Lens.lens (associatedGatewayId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {associatedGatewayId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgafAssociatedGatewayId "Use generic-lens or generic-optics with 'associatedGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgafDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgafDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The token provided in the previous call to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Text)
ddcgafNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgafNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgafMaxResults :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Lude.Maybe Lude.Int)
ddcgafMaxResults = Lens.lens (maxResults :: DescribeDirectConnectGatewayAssociations -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeDirectConnectGatewayAssociations)
{-# DEPRECATED ddcgafMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeDirectConnectGatewayAssociations where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsDirectConnectGatewayAssociations) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcgafNextToken Lens..~ rs Lens.^. drsNextToken

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
  { -- | The token to retrieve the next page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the associations.
    directConnectGatewayAssociations :: Lude.Maybe [DirectConnectGatewayAssociation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectConnectGatewayAssociationsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to retrieve the next page.
-- * 'directConnectGatewayAssociations' - Information about the associations.
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
drsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAssociationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationsResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the associations.
--
-- /Note:/ Consider using 'directConnectGatewayAssociations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDirectConnectGatewayAssociations :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Lude.Maybe [DirectConnectGatewayAssociation])
drsDirectConnectGatewayAssociations = Lens.lens (directConnectGatewayAssociations :: DescribeDirectConnectGatewayAssociationsResponse -> Lude.Maybe [DirectConnectGatewayAssociation]) (\s a -> s {directConnectGatewayAssociations = a} :: DescribeDirectConnectGatewayAssociationsResponse)
{-# DEPRECATED drsDirectConnectGatewayAssociations "Use generic-lens or generic-optics with 'directConnectGatewayAssociations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeDirectConnectGatewayAssociationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDirectConnectGatewayAssociationsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
