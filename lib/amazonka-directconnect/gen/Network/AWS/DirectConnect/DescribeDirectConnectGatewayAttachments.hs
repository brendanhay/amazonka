{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attachments between your Direct Connect gateways and virtual interfaces. You must specify a Direct Connect gateway, a virtual interface, or both. If you specify a Direct Connect gateway, the response contains all virtual interfaces attached to the Direct Connect gateway. If you specify a virtual interface, the response contains all Direct Connect gateways attached to the virtual interface. If you specify both, the response contains the attachment between the Direct Connect gateway and the virtual interface.
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
  ( -- * Creating a request
    DescribeDirectConnectGatewayAttachments (..),
    mkDescribeDirectConnectGatewayAttachments,

    -- ** Request lenses
    ddcgasDirectConnectGatewayId,
    ddcgasNextToken,
    ddcgasMaxResults,
    ddcgasVirtualInterfaceId,

    -- * Destructuring the response
    DescribeDirectConnectGatewayAttachmentsResponse (..),
    mkDescribeDirectConnectGatewayAttachmentsResponse,

    -- ** Response lenses
    ddcgasrsNextToken,
    ddcgasrsDirectConnectGatewayAttachments,
    ddcgasrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDirectConnectGatewayAttachments' smart constructor.
data DescribeDirectConnectGatewayAttachments = DescribeDirectConnectGatewayAttachments'
  { -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text,
    -- | The token provided in the previous call to retrieve the next page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
    maxResults :: Lude.Maybe Lude.Int,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectConnectGatewayAttachments' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'nextToken' - The token provided in the previous call to retrieve the next page.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkDescribeDirectConnectGatewayAttachments ::
  DescribeDirectConnectGatewayAttachments
mkDescribeDirectConnectGatewayAttachments =
  DescribeDirectConnectGatewayAttachments'
    { directConnectGatewayId =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      virtualInterfaceId = Lude.Nothing
    }

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Lude.Maybe Lude.Text)
ddcgasDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DescribeDirectConnectGatewayAttachments -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAttachments)
{-# DEPRECATED ddcgasDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The token provided in the previous call to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasNextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Lude.Maybe Lude.Text)
ddcgasNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAttachments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAttachments)
{-# DEPRECATED ddcgasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasMaxResults :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Lude.Maybe Lude.Int)
ddcgasMaxResults = Lens.lens (maxResults :: DescribeDirectConnectGatewayAttachments -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeDirectConnectGatewayAttachments)
{-# DEPRECATED ddcgasMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasVirtualInterfaceId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Lude.Maybe Lude.Text)
ddcgasVirtualInterfaceId = Lens.lens (virtualInterfaceId :: DescribeDirectConnectGatewayAttachments -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceId = a} :: DescribeDirectConnectGatewayAttachments)
{-# DEPRECATED ddcgasVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Page.AWSPager DescribeDirectConnectGatewayAttachments where
  page rq rs
    | Page.stop (rs Lens.^. ddcgasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcgasrsDirectConnectGatewayAttachments) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcgasNextToken Lens..~ rs Lens.^. ddcgasrsNextToken

instance Lude.AWSRequest DescribeDirectConnectGatewayAttachments where
  type
    Rs DescribeDirectConnectGatewayAttachments =
      DescribeDirectConnectGatewayAttachmentsResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAttachmentsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "directConnectGatewayAttachments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDirectConnectGatewayAttachments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.DescribeDirectConnectGatewayAttachments" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDirectConnectGatewayAttachments where
  toJSON DescribeDirectConnectGatewayAttachments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("directConnectGatewayId" Lude..=)
              Lude.<$> directConnectGatewayId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("virtualInterfaceId" Lude..=) Lude.<$> virtualInterfaceId
          ]
      )

instance Lude.ToPath DescribeDirectConnectGatewayAttachments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDirectConnectGatewayAttachments where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDirectConnectGatewayAttachmentsResponse' smart constructor.
data DescribeDirectConnectGatewayAttachmentsResponse = DescribeDirectConnectGatewayAttachmentsResponse'
  { -- | The token to retrieve the next page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The attachments.
    directConnectGatewayAttachments :: Lude.Maybe [DirectConnectGatewayAttachment],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectConnectGatewayAttachmentsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to retrieve the next page.
-- * 'directConnectGatewayAttachments' - The attachments.
-- * 'responseStatus' - The response status code.
mkDescribeDirectConnectGatewayAttachmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDirectConnectGatewayAttachmentsResponse
mkDescribeDirectConnectGatewayAttachmentsResponse pResponseStatus_ =
  DescribeDirectConnectGatewayAttachmentsResponse'
    { nextToken =
        Lude.Nothing,
      directConnectGatewayAttachments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasrsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Lude.Maybe Lude.Text)
ddcgasrsNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewayAttachmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewayAttachmentsResponse)
{-# DEPRECATED ddcgasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The attachments.
--
-- /Note:/ Consider using 'directConnectGatewayAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasrsDirectConnectGatewayAttachments :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Lude.Maybe [DirectConnectGatewayAttachment])
ddcgasrsDirectConnectGatewayAttachments = Lens.lens (directConnectGatewayAttachments :: DescribeDirectConnectGatewayAttachmentsResponse -> Lude.Maybe [DirectConnectGatewayAttachment]) (\s a -> s {directConnectGatewayAttachments = a} :: DescribeDirectConnectGatewayAttachmentsResponse)
{-# DEPRECATED ddcgasrsDirectConnectGatewayAttachments "Use generic-lens or generic-optics with 'directConnectGatewayAttachments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasrsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse Lude.Int
ddcgasrsResponseStatus = Lens.lens (responseStatus :: DescribeDirectConnectGatewayAttachmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDirectConnectGatewayAttachmentsResponse)
{-# DEPRECATED ddcgasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
