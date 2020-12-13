{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListFacetNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of facets that exist in a schema.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetNames
  ( -- * Creating a request
    ListFacetNames (..),
    mkListFacetNames,

    -- ** Request lenses
    lfnNextToken,
    lfnSchemaARN,
    lfnMaxResults,

    -- * Destructuring the response
    ListFacetNamesResponse (..),
    mkListFacetNamesResponse,

    -- ** Response lenses
    lfnrsNextToken,
    lfnrsFacetNames,
    lfnrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFacetNames' smart constructor.
data ListFacetNames = ListFacetNames'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) to retrieve facet names from.
    schemaARN :: Lude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFacetNames' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'schemaARN' - The Amazon Resource Name (ARN) to retrieve facet names from.
-- * 'maxResults' - The maximum number of results to retrieve.
mkListFacetNames ::
  -- | 'schemaARN'
  Lude.Text ->
  ListFacetNames
mkListFacetNames pSchemaARN_ =
  ListFacetNames'
    { nextToken = Lude.Nothing,
      schemaARN = pSchemaARN_,
      maxResults = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnNextToken :: Lens.Lens' ListFacetNames (Lude.Maybe Lude.Text)
lfnNextToken = Lens.lens (nextToken :: ListFacetNames -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFacetNames)
{-# DEPRECATED lfnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) to retrieve facet names from.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnSchemaARN :: Lens.Lens' ListFacetNames Lude.Text
lfnSchemaARN = Lens.lens (schemaARN :: ListFacetNames -> Lude.Text) (\s a -> s {schemaARN = a} :: ListFacetNames)
{-# DEPRECATED lfnSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnMaxResults :: Lens.Lens' ListFacetNames (Lude.Maybe Lude.Natural)
lfnMaxResults = Lens.lens (maxResults :: ListFacetNames -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFacetNames)
{-# DEPRECATED lfnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListFacetNames where
  page rq rs
    | Page.stop (rs Lens.^. lfnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfnrsFacetNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfnNextToken Lens..~ rs Lens.^. lfnrsNextToken

instance Lude.AWSRequest ListFacetNames where
  type Rs ListFacetNames = ListFacetNamesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFacetNamesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "FacetNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFacetNames where
  toHeaders ListFacetNames' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON ListFacetNames where
  toJSON ListFacetNames' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListFacetNames where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/facet/list"

instance Lude.ToQuery ListFacetNames where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListFacetNamesResponse' smart constructor.
data ListFacetNamesResponse = ListFacetNamesResponse'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The names of facets that exist within the schema.
    facetNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFacetNamesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'facetNames' - The names of facets that exist within the schema.
-- * 'responseStatus' - The response status code.
mkListFacetNamesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFacetNamesResponse
mkListFacetNamesResponse pResponseStatus_ =
  ListFacetNamesResponse'
    { nextToken = Lude.Nothing,
      facetNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnrsNextToken :: Lens.Lens' ListFacetNamesResponse (Lude.Maybe Lude.Text)
lfnrsNextToken = Lens.lens (nextToken :: ListFacetNamesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFacetNamesResponse)
{-# DEPRECATED lfnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of facets that exist within the schema.
--
-- /Note:/ Consider using 'facetNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnrsFacetNames :: Lens.Lens' ListFacetNamesResponse (Lude.Maybe [Lude.Text])
lfnrsFacetNames = Lens.lens (facetNames :: ListFacetNamesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {facetNames = a} :: ListFacetNamesResponse)
{-# DEPRECATED lfnrsFacetNames "Use generic-lens or generic-optics with 'facetNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfnrsResponseStatus :: Lens.Lens' ListFacetNamesResponse Lude.Int
lfnrsResponseStatus = Lens.lens (responseStatus :: ListFacetNamesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFacetNamesResponse)
{-# DEPRECATED lfnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
