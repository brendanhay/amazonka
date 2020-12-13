{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway summaries. Use GetGateway to retrieve details of a specific gateway. An optional gateway group ARN can be provided to only retrieve gateway summaries of gateways that are associated with that gateway group ARN.
module Network.AWS.AlexaBusiness.ListGateways
  ( -- * Creating a request
    ListGateways (..),
    mkListGateways,

    -- ** Request lenses
    lgNextToken,
    lgGatewayGroupARN,
    lgMaxResults,

    -- * Destructuring the response
    ListGatewaysResponse (..),
    mkListGatewaysResponse,

    -- ** Response lenses
    lgrsNextToken,
    lgrsGateways,
    lgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The gateway group ARN for which to list gateways.
    gatewayGroupARN :: Lude.Maybe Lude.Text,
    -- | The maximum number of gateway summaries to return. The default is 50.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGateways' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token used to paginate though multiple pages of gateway summaries.
-- * 'gatewayGroupARN' - The gateway group ARN for which to list gateways.
-- * 'maxResults' - The maximum number of gateway summaries to return. The default is 50.
mkListGateways ::
  ListGateways
mkListGateways =
  ListGateways'
    { nextToken = Lude.Nothing,
      gatewayGroupARN = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token used to paginate though multiple pages of gateway summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGateways (Lude.Maybe Lude.Text)
lgNextToken = Lens.lens (nextToken :: ListGateways -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGateways)
{-# DEPRECATED lgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The gateway group ARN for which to list gateways.
--
-- /Note:/ Consider using 'gatewayGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgGatewayGroupARN :: Lens.Lens' ListGateways (Lude.Maybe Lude.Text)
lgGatewayGroupARN = Lens.lens (gatewayGroupARN :: ListGateways -> Lude.Maybe Lude.Text) (\s a -> s {gatewayGroupARN = a} :: ListGateways)
{-# DEPRECATED lgGatewayGroupARN "Use generic-lens or generic-optics with 'gatewayGroupARN' instead." #-}

-- | The maximum number of gateway summaries to return. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxResults :: Lens.Lens' ListGateways (Lude.Maybe Lude.Natural)
lgMaxResults = Lens.lens (maxResults :: ListGateways -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListGateways)
{-# DEPRECATED lgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListGateways where
  type Rs ListGateways = ListGatewaysResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Gateways" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGateways where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ListGateways" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGateways where
  toJSON ListGateways' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("GatewayGroupArn" Lude..=) Lude.<$> gatewayGroupARN,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListGateways where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGateways where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The gateways in the list.
    gateways :: Lude.Maybe [GatewaySummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGatewaysResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token used to paginate though multiple pages of gateway summaries.
-- * 'gateways' - The gateways in the list.
-- * 'responseStatus' - The response status code.
mkListGatewaysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGatewaysResponse
mkListGatewaysResponse pResponseStatus_ =
  ListGatewaysResponse'
    { nextToken = Lude.Nothing,
      gateways = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token used to paginate though multiple pages of gateway summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsNextToken :: Lens.Lens' ListGatewaysResponse (Lude.Maybe Lude.Text)
lgrsNextToken = Lens.lens (nextToken :: ListGatewaysResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGatewaysResponse)
{-# DEPRECATED lgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The gateways in the list.
--
-- /Note:/ Consider using 'gateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsGateways :: Lens.Lens' ListGatewaysResponse (Lude.Maybe [GatewaySummary])
lgrsGateways = Lens.lens (gateways :: ListGatewaysResponse -> Lude.Maybe [GatewaySummary]) (\s a -> s {gateways = a} :: ListGatewaysResponse)
{-# DEPRECATED lgrsGateways "Use generic-lens or generic-optics with 'gateways' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrsResponseStatus :: Lens.Lens' ListGatewaysResponse Lude.Int
lgrsResponseStatus = Lens.lens (responseStatus :: ListGatewaysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGatewaysResponse)
{-# DEPRECATED lgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
