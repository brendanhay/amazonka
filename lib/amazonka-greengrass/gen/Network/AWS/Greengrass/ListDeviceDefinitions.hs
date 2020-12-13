{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of device definitions.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeviceDefinitions
  ( -- * Creating a request
    ListDeviceDefinitions (..),
    mkListDeviceDefinitions,

    -- ** Request lenses
    lddNextToken,
    lddMaxResults,

    -- * Destructuring the response
    ListDeviceDefinitionsResponse (..),
    mkListDeviceDefinitionsResponse,

    -- ** Response lenses
    lddrsNextToken,
    lddrsDefinitions,
    lddrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDeviceDefinitions' smart constructor.
data ListDeviceDefinitions = ListDeviceDefinitions'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceDefinitions' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'maxResults' - The maximum number of results to be returned per request.
mkListDeviceDefinitions ::
  ListDeviceDefinitions
mkListDeviceDefinitions =
  ListDeviceDefinitions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddNextToken :: Lens.Lens' ListDeviceDefinitions (Lude.Maybe Lude.Text)
lddNextToken = Lens.lens (nextToken :: ListDeviceDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceDefinitions)
{-# DEPRECATED lddNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddMaxResults :: Lens.Lens' ListDeviceDefinitions (Lude.Maybe Lude.Text)
lddMaxResults = Lens.lens (maxResults :: ListDeviceDefinitions -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListDeviceDefinitions)
{-# DEPRECATED lddMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDeviceDefinitions where
  page rq rs
    | Page.stop (rs Lens.^. lddrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lddrsDefinitions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lddNextToken Lens..~ rs Lens.^. lddrsNextToken

instance Lude.AWSRequest ListDeviceDefinitions where
  type Rs ListDeviceDefinitions = ListDeviceDefinitionsResponse
  request = Req.get greengrassService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeviceDefinitionsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Definitions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeviceDefinitions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListDeviceDefinitions where
  toPath = Lude.const "/greengrass/definition/devices"

instance Lude.ToQuery ListDeviceDefinitions where
  toQuery ListDeviceDefinitions' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListDeviceDefinitionsResponse' smart constructor.
data ListDeviceDefinitionsResponse = ListDeviceDefinitionsResponse'
  { -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about a definition.
    definitions :: Lude.Maybe [DefinitionInformation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceDefinitionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of results, or ''null'' if there are no additional results.
-- * 'definitions' - Information about a definition.
-- * 'responseStatus' - The response status code.
mkListDeviceDefinitionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeviceDefinitionsResponse
mkListDeviceDefinitionsResponse pResponseStatus_ =
  ListDeviceDefinitionsResponse'
    { nextToken = Lude.Nothing,
      definitions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddrsNextToken :: Lens.Lens' ListDeviceDefinitionsResponse (Lude.Maybe Lude.Text)
lddrsNextToken = Lens.lens (nextToken :: ListDeviceDefinitionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceDefinitionsResponse)
{-# DEPRECATED lddrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about a definition.
--
-- /Note:/ Consider using 'definitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddrsDefinitions :: Lens.Lens' ListDeviceDefinitionsResponse (Lude.Maybe [DefinitionInformation])
lddrsDefinitions = Lens.lens (definitions :: ListDeviceDefinitionsResponse -> Lude.Maybe [DefinitionInformation]) (\s a -> s {definitions = a} :: ListDeviceDefinitionsResponse)
{-# DEPRECATED lddrsDefinitions "Use generic-lens or generic-optics with 'definitions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddrsResponseStatus :: Lens.Lens' ListDeviceDefinitionsResponse Lude.Int
lddrsResponseStatus = Lens.lens (responseStatus :: ListDeviceDefinitionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeviceDefinitionsResponse)
{-# DEPRECATED lddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
