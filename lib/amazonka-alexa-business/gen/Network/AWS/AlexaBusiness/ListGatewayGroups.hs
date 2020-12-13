{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListGatewayGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway group summaries. Use GetGatewayGroup to retrieve details of a specific gateway group.
module Network.AWS.AlexaBusiness.ListGatewayGroups
  ( -- * Creating a request
    ListGatewayGroups (..),
    mkListGatewayGroups,

    -- ** Request lenses
    lggNextToken,
    lggMaxResults,

    -- * Destructuring the response
    ListGatewayGroupsResponse (..),
    mkListGatewayGroupsResponse,

    -- ** Response lenses
    lggrsGatewayGroups,
    lggrsNextToken,
    lggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGatewayGroups' smart constructor.
data ListGatewayGroups = ListGatewayGroups'
  { -- | The token used to paginate though multiple pages of gateway group summaries.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of gateway group summaries to return. The default is 50.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGatewayGroups' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token used to paginate though multiple pages of gateway group summaries.
-- * 'maxResults' - The maximum number of gateway group summaries to return. The default is 50.
mkListGatewayGroups ::
  ListGatewayGroups
mkListGatewayGroups =
  ListGatewayGroups'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token used to paginate though multiple pages of gateway group summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggNextToken :: Lens.Lens' ListGatewayGroups (Lude.Maybe Lude.Text)
lggNextToken = Lens.lens (nextToken :: ListGatewayGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGatewayGroups)
{-# DEPRECATED lggNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of gateway group summaries to return. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggMaxResults :: Lens.Lens' ListGatewayGroups (Lude.Maybe Lude.Natural)
lggMaxResults = Lens.lens (maxResults :: ListGatewayGroups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListGatewayGroups)
{-# DEPRECATED lggMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListGatewayGroups where
  type Rs ListGatewayGroups = ListGatewayGroupsResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListGatewayGroupsResponse'
            Lude.<$> (x Lude..?> "GatewayGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListGatewayGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ListGatewayGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListGatewayGroups where
  toJSON ListGatewayGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListGatewayGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGatewayGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListGatewayGroupsResponse' smart constructor.
data ListGatewayGroupsResponse = ListGatewayGroupsResponse'
  { -- | The gateway groups in the list.
    gatewayGroups :: Lude.Maybe [GatewayGroupSummary],
    -- | The token used to paginate though multiple pages of gateway group summaries.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGatewayGroupsResponse' with the minimum fields required to make a request.
--
-- * 'gatewayGroups' - The gateway groups in the list.
-- * 'nextToken' - The token used to paginate though multiple pages of gateway group summaries.
-- * 'responseStatus' - The response status code.
mkListGatewayGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGatewayGroupsResponse
mkListGatewayGroupsResponse pResponseStatus_ =
  ListGatewayGroupsResponse'
    { gatewayGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The gateway groups in the list.
--
-- /Note:/ Consider using 'gatewayGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggrsGatewayGroups :: Lens.Lens' ListGatewayGroupsResponse (Lude.Maybe [GatewayGroupSummary])
lggrsGatewayGroups = Lens.lens (gatewayGroups :: ListGatewayGroupsResponse -> Lude.Maybe [GatewayGroupSummary]) (\s a -> s {gatewayGroups = a} :: ListGatewayGroupsResponse)
{-# DEPRECATED lggrsGatewayGroups "Use generic-lens or generic-optics with 'gatewayGroups' instead." #-}

-- | The token used to paginate though multiple pages of gateway group summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggrsNextToken :: Lens.Lens' ListGatewayGroupsResponse (Lude.Maybe Lude.Text)
lggrsNextToken = Lens.lens (nextToken :: ListGatewayGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListGatewayGroupsResponse)
{-# DEPRECATED lggrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggrsResponseStatus :: Lens.Lens' ListGatewayGroupsResponse Lude.Int
lggrsResponseStatus = Lens.lens (responseStatus :: ListGatewayGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGatewayGroupsResponse)
{-# DEPRECATED lggrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
