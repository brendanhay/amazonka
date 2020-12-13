{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your Direct Connect gateways or only the specified Direct Connect gateway. Deleted Direct Connect gateways are not returned.
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGateways
  ( -- * Creating a request
    DescribeDirectConnectGateways (..),
    mkDescribeDirectConnectGateways,

    -- ** Request lenses
    ddcgDirectConnectGatewayId,
    ddcgNextToken,
    ddcgMaxResults,

    -- * Destructuring the response
    DescribeDirectConnectGatewaysResponse (..),
    mkDescribeDirectConnectGatewaysResponse,

    -- ** Response lenses
    ddcgrsDirectConnectGateways,
    ddcgrsNextToken,
    ddcgrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDirectConnectGateways' smart constructor.
data DescribeDirectConnectGateways = DescribeDirectConnectGateways'
  { -- | The ID of the Direct Connect gateway.
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

-- | Creates a value of 'DescribeDirectConnectGateways' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'nextToken' - The token provided in the previous call to retrieve the next page.
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
mkDescribeDirectConnectGateways ::
  DescribeDirectConnectGateways
mkDescribeDirectConnectGateways =
  DescribeDirectConnectGateways'
    { directConnectGatewayId =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGateways (Lude.Maybe Lude.Text)
ddcgDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DescribeDirectConnectGateways -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGateways)
{-# DEPRECATED ddcgDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The token provided in the previous call to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgNextToken :: Lens.Lens' DescribeDirectConnectGateways (Lude.Maybe Lude.Text)
ddcgNextToken = Lens.lens (nextToken :: DescribeDirectConnectGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGateways)
{-# DEPRECATED ddcgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgMaxResults :: Lens.Lens' DescribeDirectConnectGateways (Lude.Maybe Lude.Int)
ddcgMaxResults = Lens.lens (maxResults :: DescribeDirectConnectGateways -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeDirectConnectGateways)
{-# DEPRECATED ddcgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeDirectConnectGateways where
  page rq rs
    | Page.stop (rs Lens.^. ddcgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ddcgrsDirectConnectGateways) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ddcgNextToken Lens..~ rs Lens.^. ddcgrsNextToken

instance Lude.AWSRequest DescribeDirectConnectGateways where
  type
    Rs DescribeDirectConnectGateways =
      DescribeDirectConnectGatewaysResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewaysResponse'
            Lude.<$> (x Lude..?> "directConnectGateways" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDirectConnectGateways where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.DescribeDirectConnectGateways" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDirectConnectGateways where
  toJSON DescribeDirectConnectGateways' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("directConnectGatewayId" Lude..=)
              Lude.<$> directConnectGatewayId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeDirectConnectGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDirectConnectGateways where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDirectConnectGatewaysResponse' smart constructor.
data DescribeDirectConnectGatewaysResponse = DescribeDirectConnectGatewaysResponse'
  { -- | The Direct Connect gateways.
    directConnectGateways :: Lude.Maybe [DirectConnectGateway],
    -- | The token to retrieve the next page.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDirectConnectGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGateways' - The Direct Connect gateways.
-- * 'nextToken' - The token to retrieve the next page.
-- * 'responseStatus' - The response status code.
mkDescribeDirectConnectGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDirectConnectGatewaysResponse
mkDescribeDirectConnectGatewaysResponse pResponseStatus_ =
  DescribeDirectConnectGatewaysResponse'
    { directConnectGateways =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Direct Connect gateways.
--
-- /Note:/ Consider using 'directConnectGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrsDirectConnectGateways :: Lens.Lens' DescribeDirectConnectGatewaysResponse (Lude.Maybe [DirectConnectGateway])
ddcgrsDirectConnectGateways = Lens.lens (directConnectGateways :: DescribeDirectConnectGatewaysResponse -> Lude.Maybe [DirectConnectGateway]) (\s a -> s {directConnectGateways = a} :: DescribeDirectConnectGatewaysResponse)
{-# DEPRECATED ddcgrsDirectConnectGateways "Use generic-lens or generic-optics with 'directConnectGateways' instead." #-}

-- | The token to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrsNextToken :: Lens.Lens' DescribeDirectConnectGatewaysResponse (Lude.Maybe Lude.Text)
ddcgrsNextToken = Lens.lens (nextToken :: DescribeDirectConnectGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeDirectConnectGatewaysResponse)
{-# DEPRECATED ddcgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewaysResponse Lude.Int
ddcgrsResponseStatus = Lens.lens (responseStatus :: DescribeDirectConnectGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDirectConnectGatewaysResponse)
{-# DEPRECATED ddcgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
