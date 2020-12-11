{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListFunctionDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of Lambda function definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListFunctionDefinitions
  ( -- * Creating a request
    ListFunctionDefinitions (..),
    mkListFunctionDefinitions,

    -- ** Request lenses
    lfdNextToken,
    lfdMaxResults,

    -- * Destructuring the response
    ListFunctionDefinitionsResponse (..),
    mkListFunctionDefinitionsResponse,

    -- ** Response lenses
    lfdrsNextToken,
    lfdrsDefinitions,
    lfdrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFunctionDefinitions' smart constructor.
data ListFunctionDefinitions = ListFunctionDefinitions'
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

-- | Creates a value of 'ListFunctionDefinitions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListFunctionDefinitions ::
  ListFunctionDefinitions
mkListFunctionDefinitions =
  ListFunctionDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdNextToken :: Lens.Lens' ListFunctionDefinitions (Lude.Maybe Lude.Text)
lfdNextToken = Lens.lens (nextToken :: ListFunctionDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFunctionDefinitions)
{-# DEPRECATED lfdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdMaxResults :: Lens.Lens' ListFunctionDefinitions (Lude.Maybe Lude.Text)
lfdMaxResults = Lens.lens (maxResults :: ListFunctionDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListFunctionDefinitions)
{-# DEPRECATED lfdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListFunctionDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lfdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfdrsDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfdNextToken Lens..~ rs Lens.^. lfdrsNextToken

instance Lude.AWSRequest ListFunctionDefinitions where
  type Rs ListFunctionDefinitions = ListFunctionDefinitionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFunctionDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Definitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFunctionDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListFunctionDefinitions where
  toPath = Lude.const "/greengrass/definition/functions"

instance Lude.ToQuery ListFunctionDefinitions where
  toQuery ListFunctionDefinitions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListFunctionDefinitionsResponse' smart constructor.
data ListFunctionDefinitionsResponse = ListFunctionDefinitionsResponse'
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

-- | Creates a value of 'ListFunctionDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'definitions' - Information about a definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListFunctionDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFunctionDefinitionsResponse
mkListFunctionDefinitionsResponse pResponseStatus_ =
  ListFunctionDefinitionsResponse'
    { nextToken = Lude.Nothing,
      definitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrsNextToken :: Lens.Lens' ListFunctionDefinitionsResponse (Lude.Maybe Lude.Text)
lfdrsNextToken = Lens.lens (nextToken :: ListFunctionDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFunctionDefinitionsResponse)
{-# DEPRECATED lfdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrsDefinitions :: Lens.Lens' ListFunctionDefinitionsResponse (Lude.Maybe [DefinitionInformation])
lfdrsDefinitions = Lens.lens (definitions :: ListFunctionDefinitionsResponse -> Lude.Maybe [DefinitionInformation]) (\s a -> s {definitions = a} :: ListFunctionDefinitionsResponse)
{-# DEPRECATED lfdrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfdrsResponseStatus :: Lens.Lens' ListFunctionDefinitionsResponse Lude.Int
lfdrsResponseStatus = Lens.lens (responseStatus :: ListFunctionDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFunctionDefinitionsResponse)
{-# DEPRECATED lfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
