{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a subscription definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
  ( -- * Creating a request
    ListSubscriptionDefinitionVersions (..),
    mkListSubscriptionDefinitionVersions,

    -- ** Request lenses
    lsdvSubscriptionDefinitionId,
    lsdvNextToken,
    lsdvMaxResults,

    -- * Destructuring the response
    ListSubscriptionDefinitionVersionsResponse (..),
    mkListSubscriptionDefinitionVersionsResponse,

    -- ** Response lenses
    lsdvrsVersions,
    lsdvrsNextToken,
    lsdvrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSubscriptionDefinitionVersions' smart constructor.
data ListSubscriptionDefinitionVersions = ListSubscriptionDefinitionVersions'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Lude.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptionDefinitionVersions' with the minimum fields required to make a request.
--
-- * 'subscriptionDefinitionId' - The ID of the subscription definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListSubscriptionDefinitionVersions ::
  -- | 'subscriptionDefinitionId'
  Lude.Text ->
  ListSubscriptionDefinitionVersions
mkListSubscriptionDefinitionVersions pSubscriptionDefinitionId_ =
  ListSubscriptionDefinitionVersions'
    { subscriptionDefinitionId =
        pSubscriptionDefinitionId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvSubscriptionDefinitionId :: Lens.Lens' ListSubscriptionDefinitionVersions Lude.Text
lsdvSubscriptionDefinitionId = Lens.lens (subscriptionDefinitionId :: ListSubscriptionDefinitionVersions -> Lude.Text) (\s a -> s {subscriptionDefinitionId = a} :: ListSubscriptionDefinitionVersions)
{-# DEPRECATED lsdvSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvNextToken :: Lens.Lens' ListSubscriptionDefinitionVersions (Lude.Maybe Lude.Text)
lsdvNextToken = Lens.lens (nextToken :: ListSubscriptionDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptionDefinitionVersions)
{-# DEPRECATED lsdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvMaxResults :: Lens.Lens' ListSubscriptionDefinitionVersions (Lude.Maybe Lude.Text)
lsdvMaxResults = Lens.lens (maxResults :: ListSubscriptionDefinitionVersions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListSubscriptionDefinitionVersions)
{-# DEPRECATED lsdvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSubscriptionDefinitionVersions where
  page rq rs
    | Page.stop (rs Lens.^. lsdvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsdvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsdvNextToken Lens..~ rs Lens.^. lsdvrsNextToken

instance Lude.AWSRequest ListSubscriptionDefinitionVersions where
  type
    Rs ListSubscriptionDefinitionVersions =
      ListSubscriptionDefinitionVersionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionVersionsResponse'
            Lude.<$> (x Lude..?> "Versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSubscriptionDefinitionVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListSubscriptionDefinitionVersions where
  toPath ListSubscriptionDefinitionVersions' {..} =
    Lude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Lude.toBS subscriptionDefinitionId,
        "/versions"
      ]

instance Lude.ToQuery ListSubscriptionDefinitionVersions where
  toQuery ListSubscriptionDefinitionVersions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListSubscriptionDefinitionVersionsResponse' smart constructor.
data ListSubscriptionDefinitionVersionsResponse = ListSubscriptionDefinitionVersionsResponse'
  { -- | Information about a version.
    versions :: Lude.Maybe [VersionInformation],
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptionDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- * 'versions' - Information about a version.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListSubscriptionDefinitionVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSubscriptionDefinitionVersionsResponse
mkListSubscriptionDefinitionVersionsResponse pResponseStatus_ =
  ListSubscriptionDefinitionVersionsResponse'
    { versions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrsVersions :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Lude.Maybe [VersionInformation])
lsdvrsVersions = Lens.lens (versions :: ListSubscriptionDefinitionVersionsResponse -> Lude.Maybe [VersionInformation]) (\s a -> s {versions = a} :: ListSubscriptionDefinitionVersionsResponse)
{-# DEPRECATED lsdvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrsNextToken :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse (Lude.Maybe Lude.Text)
lsdvrsNextToken = Lens.lens (nextToken :: ListSubscriptionDefinitionVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptionDefinitionVersionsResponse)
{-# DEPRECATED lsdvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdvrsResponseStatus :: Lens.Lens' ListSubscriptionDefinitionVersionsResponse Lude.Int
lsdvrsResponseStatus = Lens.lens (responseStatus :: ListSubscriptionDefinitionVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSubscriptionDefinitionVersionsResponse)
{-# DEPRECATED lsdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
