{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListLoggerDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of logger definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListLoggerDefinitions
  ( -- * Creating a request
    ListLoggerDefinitions (..),
    mkListLoggerDefinitions,

    -- ** Request lenses
    lldNextToken,
    lldMaxResults,

    -- * Destructuring the response
    ListLoggerDefinitionsResponse (..),
    mkListLoggerDefinitionsResponse,

    -- ** Response lenses
    lldrsNextToken,
    lldrsDefinitions,
    lldrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLoggerDefinitions' smart constructor.
data ListLoggerDefinitions = ListLoggerDefinitions'
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

-- | Creates a value of 'ListLoggerDefinitions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to be returned per request.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
mkListLoggerDefinitions ::
  ListLoggerDefinitions
mkListLoggerDefinitions =
  ListLoggerDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldNextToken :: Lens.Lens' ListLoggerDefinitions (Lude.Maybe Lude.Text)
lldNextToken = Lens.lens (nextToken :: ListLoggerDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLoggerDefinitions)
{-# DEPRECATED lldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldMaxResults :: Lens.Lens' ListLoggerDefinitions (Lude.Maybe Lude.Text)
lldMaxResults = Lens.lens (maxResults :: ListLoggerDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListLoggerDefinitions)
{-# DEPRECATED lldMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListLoggerDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lldrsDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lldNextToken Lens..~ rs Lens.^. lldrsNextToken

instance Lude.AWSRequest ListLoggerDefinitions where
  type Rs ListLoggerDefinitions = ListLoggerDefinitionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLoggerDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Definitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLoggerDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListLoggerDefinitions where
  toPath = Lude.const "/greengrass/definition/loggers"

instance Lude.ToQuery ListLoggerDefinitions where
  toQuery ListLoggerDefinitions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListLoggerDefinitionsResponse' smart constructor.
data ListLoggerDefinitionsResponse = ListLoggerDefinitionsResponse'
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

-- | Creates a value of 'ListLoggerDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'definitions' - Information about a definition.
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'responseStatus' - The response status code.
mkListLoggerDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLoggerDefinitionsResponse
mkListLoggerDefinitionsResponse pResponseStatus_ =
  ListLoggerDefinitionsResponse'
    { nextToken = Lude.Nothing,
      definitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrsNextToken :: Lens.Lens' ListLoggerDefinitionsResponse (Lude.Maybe Lude.Text)
lldrsNextToken = Lens.lens (nextToken :: ListLoggerDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListLoggerDefinitionsResponse)
{-# DEPRECATED lldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrsDefinitions :: Lens.Lens' ListLoggerDefinitionsResponse (Lude.Maybe [DefinitionInformation])
lldrsDefinitions = Lens.lens (definitions :: ListLoggerDefinitionsResponse -> Lude.Maybe [DefinitionInformation]) (\s a -> s {definitions = a} :: ListLoggerDefinitionsResponse)
{-# DEPRECATED lldrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldrsResponseStatus :: Lens.Lens' ListLoggerDefinitionsResponse Lude.Int
lldrsResponseStatus = Lens.lens (responseStatus :: ListLoggerDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLoggerDefinitionsResponse)
{-# DEPRECATED lldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
