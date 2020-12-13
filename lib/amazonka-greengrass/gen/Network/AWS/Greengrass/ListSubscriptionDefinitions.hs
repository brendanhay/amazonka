{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListSubscriptionDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of subscription definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListSubscriptionDefinitions
  ( -- * Creating a request
    ListSubscriptionDefinitions (..),
    mkListSubscriptionDefinitions,

    -- ** Request lenses
    lsdNextToken,
    lsdMaxResults,

    -- * Destructuring the response
    ListSubscriptionDefinitionsResponse (..),
    mkListSubscriptionDefinitionsResponse,

    -- ** Response lenses
    lsdrsNextToken,
    lsdrsDefinitions,
    lsdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListSubscriptionDefinitions' smart constructor.
data ListSubscriptionDefinitions = ListSubscriptionDefinitions'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptionDefinitions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListSubscriptionDefinitions ::
  ListSubscriptionDefinitions
mkListSubscriptionDefinitions =
  ListSubscriptionDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdNextToken :: Lens.Lens' ListSubscriptionDefinitions (Lude.Maybe Lude.Text)
lsdNextToken = Lens.lens (nextToken :: ListSubscriptionDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptionDefinitions)
{-# DEPRECATED lsdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdMaxResults :: Lens.Lens' ListSubscriptionDefinitions (Lude.Maybe Lude.Text)
lsdMaxResults = Lens.lens (maxResults :: ListSubscriptionDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListSubscriptionDefinitions)
{-# DEPRECATED lsdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListSubscriptionDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lsdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lsdrsDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lsdNextToken Lens..~ rs Lens.^. lsdrsNextToken

instance Lude.AWSRequest ListSubscriptionDefinitions where
  type
    Rs ListSubscriptionDefinitions =
      ListSubscriptionDefinitionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListSubscriptionDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Definitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListSubscriptionDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListSubscriptionDefinitions where
  toPath = Lude.const "/greengrass/definition/subscriptions"

instance Lude.ToQuery ListSubscriptionDefinitions where
  toQuery ListSubscriptionDefinitions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListSubscriptionDefinitionsResponse' smart constructor.
data ListSubscriptionDefinitionsResponse = ListSubscriptionDefinitionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about a definition.
    definitions :: Lude.Maybe [DefinitionInformation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListSubscriptionDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'definitions' - Information about a definition.
-- * 'responseStatus' - The response status code.
mkListSubscriptionDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListSubscriptionDefinitionsResponse
mkListSubscriptionDefinitionsResponse pResponseStatus_ =
  ListSubscriptionDefinitionsResponse'
    { nextToken = Lude.Nothing,
      definitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrsNextToken :: Lens.Lens' ListSubscriptionDefinitionsResponse (Lude.Maybe Lude.Text)
lsdrsNextToken = Lens.lens (nextToken :: ListSubscriptionDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListSubscriptionDefinitionsResponse)
{-# DEPRECATED lsdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrsDefinitions :: Lens.Lens' ListSubscriptionDefinitionsResponse (Lude.Maybe [DefinitionInformation])
lsdrsDefinitions = Lens.lens (definitions :: ListSubscriptionDefinitionsResponse -> Lude.Maybe [DefinitionInformation]) (\s a -> s {definitions = a} :: ListSubscriptionDefinitionsResponse)
{-# DEPRECATED lsdrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsdrsResponseStatus :: Lens.Lens' ListSubscriptionDefinitionsResponse Lude.Int
lsdrsResponseStatus = Lens.lens (responseStatus :: ListSubscriptionDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListSubscriptionDefinitionsResponse)
{-# DEPRECATED lsdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
