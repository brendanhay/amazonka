{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListPublishedSchemaARNs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the major version families of each published schema. If a major version ARN is provided as @SchemaArn@ , the minor version revisions in that family are listed instead.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPublishedSchemaARNs
  ( -- * Creating a request
    ListPublishedSchemaARNs (..),
    mkListPublishedSchemaARNs,

    -- ** Request lenses
    lpsaNextToken,
    lpsaSchemaARN,
    lpsaMaxResults,

    -- * Destructuring the response
    ListPublishedSchemaARNsResponse (..),
    mkListPublishedSchemaARNsResponse,

    -- ** Response lenses
    lpsarsSchemaARNs,
    lpsarsNextToken,
    lpsarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPublishedSchemaARNs' smart constructor.
data ListPublishedSchemaARNs = ListPublishedSchemaARNs'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response for @ListPublishedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
    schemaARN :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPublishedSchemaARNs' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'schemaARN' - The response for @ListPublishedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
-- * 'maxResults' - The maximum number of results to retrieve.
mkListPublishedSchemaARNs ::
  ListPublishedSchemaARNs
mkListPublishedSchemaARNs =
  ListPublishedSchemaARNs'
    { nextToken = Lude.Nothing,
      schemaARN = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsaNextToken :: Lens.Lens' ListPublishedSchemaARNs (Lude.Maybe Lude.Text)
lpsaNextToken = Lens.lens (nextToken :: ListPublishedSchemaARNs -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPublishedSchemaARNs)
{-# DEPRECATED lpsaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response for @ListPublishedSchemaArns@ when this parameter is used will list all minor version ARNs for a major version.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsaSchemaARN :: Lens.Lens' ListPublishedSchemaARNs (Lude.Maybe Lude.Text)
lpsaSchemaARN = Lens.lens (schemaARN :: ListPublishedSchemaARNs -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: ListPublishedSchemaARNs)
{-# DEPRECATED lpsaSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsaMaxResults :: Lens.Lens' ListPublishedSchemaARNs (Lude.Maybe Lude.Natural)
lpsaMaxResults = Lens.lens (maxResults :: ListPublishedSchemaARNs -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPublishedSchemaARNs)
{-# DEPRECATED lpsaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListPublishedSchemaARNs where
  page rq rs
    | Page.stop (rs Lens.^. lpsarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpsarsSchemaARNs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpsaNextToken Lens..~ rs Lens.^. lpsarsNextToken

instance Lude.AWSRequest ListPublishedSchemaARNs where
  type Rs ListPublishedSchemaARNs = ListPublishedSchemaARNsResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPublishedSchemaARNsResponse'
            Lude.<$> (x Lude..?> "SchemaArns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPublishedSchemaARNs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListPublishedSchemaARNs where
  toJSON ListPublishedSchemaARNs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SchemaArn" Lude..=) Lude.<$> schemaARN,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListPublishedSchemaARNs where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/schema/published"

instance Lude.ToQuery ListPublishedSchemaARNs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPublishedSchemaARNsResponse' smart constructor.
data ListPublishedSchemaARNsResponse = ListPublishedSchemaARNsResponse'
  { -- | The ARNs of published schemas.
    schemaARNs :: Lude.Maybe [Lude.Text],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPublishedSchemaARNsResponse' with the minimum fields required to make a request.
--
-- * 'schemaARNs' - The ARNs of published schemas.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListPublishedSchemaARNsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPublishedSchemaARNsResponse
mkListPublishedSchemaARNsResponse pResponseStatus_ =
  ListPublishedSchemaARNsResponse'
    { schemaARNs = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARNs of published schemas.
--
-- /Note:/ Consider using 'schemaARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsarsSchemaARNs :: Lens.Lens' ListPublishedSchemaARNsResponse (Lude.Maybe [Lude.Text])
lpsarsSchemaARNs = Lens.lens (schemaARNs :: ListPublishedSchemaARNsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {schemaARNs = a} :: ListPublishedSchemaARNsResponse)
{-# DEPRECATED lpsarsSchemaARNs "Use generic-lens or generic-optics with 'schemaARNs' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsarsNextToken :: Lens.Lens' ListPublishedSchemaARNsResponse (Lude.Maybe Lude.Text)
lpsarsNextToken = Lens.lens (nextToken :: ListPublishedSchemaARNsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPublishedSchemaARNsResponse)
{-# DEPRECATED lpsarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsarsResponseStatus :: Lens.Lens' ListPublishedSchemaARNsResponse Lude.Int
lpsarsResponseStatus = Lens.lens (responseStatus :: ListPublishedSchemaARNsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPublishedSchemaARNsResponse)
{-# DEPRECATED lpsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
