{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListTypedLinkFacetNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of @TypedLink@ facet names for a particular schema. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTypedLinkFacetNames
  ( -- * Creating a request
    ListTypedLinkFacetNames (..),
    mkListTypedLinkFacetNames,

    -- ** Request lenses
    ltlfnNextToken,
    ltlfnMaxResults,
    ltlfnSchemaARN,

    -- * Destructuring the response
    ListTypedLinkFacetNamesResponse (..),
    mkListTypedLinkFacetNamesResponse,

    -- ** Response lenses
    ltlfnrsNextToken,
    ltlfnrsFacetNames,
    ltlfnrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTypedLinkFacetNames' smart constructor.
data ListTypedLinkFacetNames = ListTypedLinkFacetNames'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    schemaARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTypedLinkFacetNames' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
mkListTypedLinkFacetNames ::
  -- | 'schemaARN'
  Lude.Text ->
  ListTypedLinkFacetNames
mkListTypedLinkFacetNames pSchemaARN_ =
  ListTypedLinkFacetNames'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      schemaARN = pSchemaARN_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnNextToken :: Lens.Lens' ListTypedLinkFacetNames (Lude.Maybe Lude.Text)
ltlfnNextToken = Lens.lens (nextToken :: ListTypedLinkFacetNames -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypedLinkFacetNames)
{-# DEPRECATED ltlfnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnMaxResults :: Lens.Lens' ListTypedLinkFacetNames (Lude.Maybe Lude.Natural)
ltlfnMaxResults = Lens.lens (maxResults :: ListTypedLinkFacetNames -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTypedLinkFacetNames)
{-# DEPRECATED ltlfnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnSchemaARN :: Lens.Lens' ListTypedLinkFacetNames Lude.Text
ltlfnSchemaARN = Lens.lens (schemaARN :: ListTypedLinkFacetNames -> Lude.Text) (\s a -> s {schemaARN = a} :: ListTypedLinkFacetNames)
{-# DEPRECATED ltlfnSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

instance Page.AWSPager ListTypedLinkFacetNames where
  page rq rs
    | Page.stop (rs Lens.^. ltlfnrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltlfnrsFacetNames) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltlfnNextToken Lens..~ rs Lens.^. ltlfnrsNextToken

instance Lude.AWSRequest ListTypedLinkFacetNames where
  type Rs ListTypedLinkFacetNames = ListTypedLinkFacetNamesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTypedLinkFacetNamesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "FacetNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTypedLinkFacetNames where
  toHeaders ListTypedLinkFacetNames' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON ListTypedLinkFacetNames where
  toJSON ListTypedLinkFacetNames' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTypedLinkFacetNames where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/list"

instance Lude.ToQuery ListTypedLinkFacetNames where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTypedLinkFacetNamesResponse' smart constructor.
data ListTypedLinkFacetNamesResponse = ListTypedLinkFacetNamesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    facetNames ::
      Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ListTypedLinkFacetNamesResponse' with the minimum fields required to make a request.
--
-- * 'facetNames' - The names of typed link facets that exist within the schema.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListTypedLinkFacetNamesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTypedLinkFacetNamesResponse
mkListTypedLinkFacetNamesResponse pResponseStatus_ =
  ListTypedLinkFacetNamesResponse'
    { nextToken = Lude.Nothing,
      facetNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnrsNextToken :: Lens.Lens' ListTypedLinkFacetNamesResponse (Lude.Maybe Lude.Text)
ltlfnrsNextToken = Lens.lens (nextToken :: ListTypedLinkFacetNamesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypedLinkFacetNamesResponse)
{-# DEPRECATED ltlfnrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The names of typed link facets that exist within the schema.
--
-- /Note:/ Consider using 'facetNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnrsFacetNames :: Lens.Lens' ListTypedLinkFacetNamesResponse (Lude.Maybe [Lude.Text])
ltlfnrsFacetNames = Lens.lens (facetNames :: ListTypedLinkFacetNamesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {facetNames = a} :: ListTypedLinkFacetNamesResponse)
{-# DEPRECATED ltlfnrsFacetNames "Use generic-lens or generic-optics with 'facetNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnrsResponseStatus :: Lens.Lens' ListTypedLinkFacetNamesResponse Lude.Int
ltlfnrsResponseStatus = Lens.lens (responseStatus :: ListTypedLinkFacetNamesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTypedLinkFacetNamesResponse)
{-# DEPRECATED ltlfnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
