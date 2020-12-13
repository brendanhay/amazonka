{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListCoreDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of core definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitions
  ( -- * Creating a request
    ListCoreDefinitions (..),
    mkListCoreDefinitions,

    -- ** Request lenses
    lcdsNextToken,
    lcdsMaxResults,

    -- * Destructuring the response
    ListCoreDefinitionsResponse (..),
    mkListCoreDefinitionsResponse,

    -- ** Response lenses
    lcdrsNextToken,
    lcdrsDefinitions,
    lcdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCoreDefinitions' smart constructor.
data ListCoreDefinitions = ListCoreDefinitions'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCoreDefinitions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListCoreDefinitions ::
  ListCoreDefinitions
mkListCoreDefinitions =
  ListCoreDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdsNextToken :: Lens.Lens' ListCoreDefinitions (Lude.Maybe Lude.Text)
lcdsNextToken = Lens.lens (nextToken :: ListCoreDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCoreDefinitions)
{-# DEPRECATED lcdsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdsMaxResults :: Lens.Lens' ListCoreDefinitions (Lude.Maybe Lude.Text)
lcdsMaxResults = Lens.lens (maxResults :: ListCoreDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListCoreDefinitions)
{-# DEPRECATED lcdsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCoreDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lcdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcdrsDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lcdsNextToken Lens..~ rs Lens.^. lcdrsNextToken

instance Lude.AWSRequest ListCoreDefinitions where
  type Rs ListCoreDefinitions = ListCoreDefinitionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCoreDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Definitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCoreDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListCoreDefinitions where
  toPath = Lude.const "/greengrass/definition/cores"

instance Lude.ToQuery ListCoreDefinitions where
  toQuery ListCoreDefinitions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListCoreDefinitionsResponse' smart constructor.
data ListCoreDefinitionsResponse = ListCoreDefinitionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about a definition.
    definitions :: Lude.Maybe [DefinitionInformation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCoreDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'definitions' - Information about a definition.
-- * 'responseStatus' - The response status code.
mkListCoreDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCoreDefinitionsResponse
mkListCoreDefinitionsResponse pResponseStatus_ =
  ListCoreDefinitionsResponse'
    { nextToken = Lude.Nothing,
      definitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrsNextToken :: Lens.Lens' ListCoreDefinitionsResponse (Lude.Maybe Lude.Text)
lcdrsNextToken = Lens.lens (nextToken :: ListCoreDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCoreDefinitionsResponse)
{-# DEPRECATED lcdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrsDefinitions :: Lens.Lens' ListCoreDefinitionsResponse (Lude.Maybe [DefinitionInformation])
lcdrsDefinitions = Lens.lens (definitions :: ListCoreDefinitionsResponse -> Lude.Maybe [DefinitionInformation]) (\s a -> s {definitions = a} :: ListCoreDefinitionsResponse)
{-# DEPRECATED lcdrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdrsResponseStatus :: Lens.Lens' ListCoreDefinitionsResponse Lude.Int
lcdrsResponseStatus = Lens.lens (responseStatus :: ListCoreDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCoreDefinitionsResponse)
{-# DEPRECATED lcdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
