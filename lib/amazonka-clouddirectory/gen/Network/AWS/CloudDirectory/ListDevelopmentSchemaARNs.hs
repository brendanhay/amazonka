{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves each Amazon Resource Name (ARN) of schemas in the development state.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs
  ( -- * Creating a request
    ListDevelopmentSchemaARNs (..),
    mkListDevelopmentSchemaARNs,

    -- ** Request lenses
    ldsaNextToken,
    ldsaMaxResults,

    -- * Destructuring the response
    ListDevelopmentSchemaARNsResponse (..),
    mkListDevelopmentSchemaARNsResponse,

    -- ** Response lenses
    ldsarsSchemaARNs,
    ldsarsNextToken,
    ldsarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDevelopmentSchemaARNs' smart constructor.
data ListDevelopmentSchemaARNs = ListDevelopmentSchemaARNs'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevelopmentSchemaARNs' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'maxResults' - The maximum number of results to retrieve.
mkListDevelopmentSchemaARNs ::
  ListDevelopmentSchemaARNs
mkListDevelopmentSchemaARNs =
  ListDevelopmentSchemaARNs'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsaNextToken :: Lens.Lens' ListDevelopmentSchemaARNs (Lude.Maybe Lude.Text)
ldsaNextToken = Lens.lens (nextToken :: ListDevelopmentSchemaARNs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevelopmentSchemaARNs)
{-# DEPRECATED ldsaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsaMaxResults :: Lens.Lens' ListDevelopmentSchemaARNs (Lude.Maybe Lude.Natural)
ldsaMaxResults = Lens.lens (maxResults :: ListDevelopmentSchemaARNs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDevelopmentSchemaARNs)
{-# DEPRECATED ldsaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDevelopmentSchemaARNs where
  page rq rs
    | Page.stop (rs Lens.^. ldsarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldsarsSchemaARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldsaNextToken Lens..~ rs Lens.^. ldsarsNextToken

instance Lude.AWSRequest ListDevelopmentSchemaARNs where
  type
    Rs ListDevelopmentSchemaARNs =
      ListDevelopmentSchemaARNsResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDevelopmentSchemaARNsResponse'
            Lude.<$> (x Lude..?> "SchemaArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDevelopmentSchemaARNs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListDevelopmentSchemaARNs where
  toJSON ListDevelopmentSchemaARNs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDevelopmentSchemaARNs where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/schema/development"

instance Lude.ToQuery ListDevelopmentSchemaARNs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDevelopmentSchemaARNsResponse' smart constructor.
data ListDevelopmentSchemaARNsResponse = ListDevelopmentSchemaARNsResponse'
  { -- | The ARNs of retrieved development schemas.
    schemaARNs :: Lude.Maybe [Lude.Text],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevelopmentSchemaARNsResponse' with the minimum fields required to make a request.
--
-- * 'schemaARNs' - The ARNs of retrieved development schemas.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListDevelopmentSchemaARNsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDevelopmentSchemaARNsResponse
mkListDevelopmentSchemaARNsResponse pResponseStatus_ =
  ListDevelopmentSchemaARNsResponse'
    { schemaARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARNs of retrieved development schemas.
--
-- /Note:/ Consider using 'schemaARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsarsSchemaARNs :: Lens.Lens' ListDevelopmentSchemaARNsResponse (Lude.Maybe [Lude.Text])
ldsarsSchemaARNs = Lens.lens (schemaARNs :: ListDevelopmentSchemaARNsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {schemaARNs = a} :: ListDevelopmentSchemaARNsResponse)
{-# DEPRECATED ldsarsSchemaARNs "Use generic-lens or generic-optics with 'schemaARNs' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsarsNextToken :: Lens.Lens' ListDevelopmentSchemaARNsResponse (Lude.Maybe Lude.Text)
ldsarsNextToken = Lens.lens (nextToken :: ListDevelopmentSchemaARNsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevelopmentSchemaARNsResponse)
{-# DEPRECATED ldsarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldsarsResponseStatus :: Lens.Lens' ListDevelopmentSchemaARNsResponse Lude.Int
ldsarsResponseStatus = Lens.lens (responseStatus :: ListDevelopmentSchemaARNsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDevelopmentSchemaARNsResponse)
{-# DEPRECATED ldsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
