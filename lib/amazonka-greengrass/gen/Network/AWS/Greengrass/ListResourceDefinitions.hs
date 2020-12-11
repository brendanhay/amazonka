{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListResourceDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of resource definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListResourceDefinitions
  ( -- * Creating a request
    ListResourceDefinitions (..),
    mkListResourceDefinitions,

    -- ** Request lenses
    lrdNextToken,
    lrdMaxResults,

    -- * Destructuring the response
    ListResourceDefinitionsResponse (..),
    mkListResourceDefinitionsResponse,

    -- ** Response lenses
    lrdrsNextToken,
    lrdrsDefinitions,
    lrdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListResourceDefinitions' smart constructor.
data ListResourceDefinitions = ListResourceDefinitions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceDefinitions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListResourceDefinitions ::
  ListResourceDefinitions
mkListResourceDefinitions =
  ListResourceDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdNextToken :: Lens.Lens' ListResourceDefinitions (Lude.Maybe Lude.Text)
lrdNextToken = Lens.lens (nextToken :: ListResourceDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDefinitions)
{-# DEPRECATED lrdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdMaxResults :: Lens.Lens' ListResourceDefinitions (Lude.Maybe Lude.Text)
lrdMaxResults = Lens.lens (maxResults :: ListResourceDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListResourceDefinitions)
{-# DEPRECATED lrdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListResourceDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lrdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrdrsDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrdNextToken Lens..~ rs Lens.^. lrdrsNextToken

instance Lude.AWSRequest ListResourceDefinitions where
  type Rs ListResourceDefinitions = ListResourceDefinitionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourceDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Definitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourceDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListResourceDefinitions where
  toPath = Lude.const "/greengrass/definition/resources"

instance Lude.ToQuery ListResourceDefinitions where
  toQuery ListResourceDefinitions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListResourceDefinitionsResponse' smart constructor.
data ListResourceDefinitionsResponse = ListResourceDefinitionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    definitions ::
      Lude.Maybe
        [DefinitionInformation],
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

-- | Creates a value of 'ListResourceDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'definitions' - Information about a definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListResourceDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourceDefinitionsResponse
mkListResourceDefinitionsResponse pResponseStatus_ =
  ListResourceDefinitionsResponse'
    { nextToken = Lude.Nothing,
      definitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrsNextToken :: Lens.Lens' ListResourceDefinitionsResponse (Lude.Maybe Lude.Text)
lrdrsNextToken = Lens.lens (nextToken :: ListResourceDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListResourceDefinitionsResponse)
{-# DEPRECATED lrdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrsDefinitions :: Lens.Lens' ListResourceDefinitionsResponse (Lude.Maybe [DefinitionInformation])
lrdrsDefinitions = Lens.lens (definitions :: ListResourceDefinitionsResponse -> Lude.Maybe [DefinitionInformation]) (\s a -> s {definitions = a} :: ListResourceDefinitionsResponse)
{-# DEPRECATED lrdrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrdrsResponseStatus :: Lens.Lens' ListResourceDefinitionsResponse Lude.Int
lrdrsResponseStatus = Lens.lens (responseStatus :: ListResourceDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceDefinitionsResponse)
{-# DEPRECATED lrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
