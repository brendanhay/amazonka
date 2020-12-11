{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a history of deployments for the group.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeployments
  ( -- * Creating a request
    ListDeployments (..),
    mkListDeployments,

    -- ** Request lenses
    ldNextToken,
    ldMaxResults,
    ldGroupId,

    -- * Destructuring the response
    ListDeploymentsResponse (..),
    mkListDeploymentsResponse,

    -- ** Response lenses
    ldrsNextToken,
    ldrsDeployments,
    ldrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Text,
    groupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeployments' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListDeployments ::
  -- | 'groupId'
  Lude.Text ->
  ListDeployments
mkListDeployments pGroupId_ =
  ListDeployments'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      groupId = pGroupId_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDeployments (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDeployments -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeployments)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldMaxResults :: Lens.Lens' ListDeployments (Lude.Maybe Lude.Text)
ldMaxResults = Lens.lens (maxResults :: ListDeployments -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListDeployments)
{-# DEPRECATED ldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldGroupId :: Lens.Lens' ListDeployments Lude.Text
ldGroupId = Lens.lens (groupId :: ListDeployments -> Lude.Text) (\s a -> s {groupId = a} :: ListDeployments)
{-# DEPRECATED ldGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Page.AWSPager ListDeployments where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDeployments) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDeployments where
  type Rs ListDeployments = ListDeploymentsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Deployments" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeployments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListDeployments where
  toPath ListDeployments' {..} =
    Lude.mconcat
      ["/greengrass/groups/", Lude.toBS groupId, "/deployments"]

instance Lude.ToQuery ListDeployments where
  toQuery ListDeployments' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    deployments :: Lude.Maybe [Deployment],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeploymentsResponse' with the minimum fields required to make a request.
--
-- * 'deployments' - A list of deployments for the requested groups.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListDeploymentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeploymentsResponse
mkListDeploymentsResponse pResponseStatus_ =
  ListDeploymentsResponse'
    { nextToken = Lude.Nothing,
      deployments = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDeploymentsResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDeploymentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeploymentsResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of deployments for the requested groups.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDeployments :: Lens.Lens' ListDeploymentsResponse (Lude.Maybe [Deployment])
ldrsDeployments = Lens.lens (deployments :: ListDeploymentsResponse -> Lude.Maybe [Deployment]) (\s a -> s {deployments = a} :: ListDeploymentsResponse)
{-# DEPRECATED ldrsDeployments "Use generic-lens or generic-optics with 'deployments' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDeploymentsResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDeploymentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeploymentsResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
