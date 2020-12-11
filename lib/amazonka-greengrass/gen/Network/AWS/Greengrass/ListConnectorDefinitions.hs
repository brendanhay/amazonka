{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListConnectorDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connector definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListConnectorDefinitions
  ( -- * Creating a request
    ListConnectorDefinitions (..),
    mkListConnectorDefinitions,

    -- ** Request lenses
    lNextToken,
    lMaxResults,

    -- * Destructuring the response
    ListConnectorDefinitionsResponse (..),
    mkListConnectorDefinitionsResponse,

    -- ** Response lenses
    lrsNextToken,
    lrsDefinitions,
    lrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListConnectorDefinitions' smart constructor.
data ListConnectorDefinitions = ListConnectorDefinitions'
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

-- | Creates a value of 'ListConnectorDefinitions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListConnectorDefinitions ::
  ListConnectorDefinitions
mkListConnectorDefinitions =
  ListConnectorDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListConnectorDefinitions (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListConnectorDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConnectorDefinitions)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListConnectorDefinitions (Lude.Maybe Lude.Text)
lMaxResults = Lens.lens (maxResults :: ListConnectorDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListConnectorDefinitions)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListConnectorDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListConnectorDefinitions where
  type Rs ListConnectorDefinitions = ListConnectorDefinitionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListConnectorDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Definitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListConnectorDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListConnectorDefinitions where
  toPath = Lude.const "/greengrass/definition/connectors"

instance Lude.ToQuery ListConnectorDefinitions where
  toQuery ListConnectorDefinitions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListConnectorDefinitionsResponse' smart constructor.
data ListConnectorDefinitionsResponse = ListConnectorDefinitionsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    definitions ::
      Lude.Maybe
        [DefinitionInformation],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListConnectorDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'definitions' - Information about a definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListConnectorDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListConnectorDefinitionsResponse
mkListConnectorDefinitionsResponse pResponseStatus_ =
  ListConnectorDefinitionsResponse'
    { nextToken = Lude.Nothing,
      definitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListConnectorDefinitionsResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListConnectorDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListConnectorDefinitionsResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsDefinitions :: Lens.Lens' ListConnectorDefinitionsResponse (Lude.Maybe [DefinitionInformation])
lrsDefinitions = Lens.lens (definitions :: ListConnectorDefinitionsResponse -> Lude.Maybe [DefinitionInformation]) (\s a -> s {definitions = a} :: ListConnectorDefinitionsResponse)
{-# DEPRECATED lrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListConnectorDefinitionsResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListConnectorDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListConnectorDefinitionsResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
