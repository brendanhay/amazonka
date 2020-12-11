{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListFacetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes attached to the facet.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetAttributes
  ( -- * Creating a request
    ListFacetAttributes (..),
    mkListFacetAttributes,

    -- ** Request lenses
    lfaNextToken,
    lfaMaxResults,
    lfaSchemaARN,
    lfaName,

    -- * Destructuring the response
    ListFacetAttributesResponse (..),
    mkListFacetAttributesResponse,

    -- ** Response lenses
    lfarsNextToken,
    lfarsAttributes,
    lfarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListFacetAttributes' smart constructor.
data ListFacetAttributes = ListFacetAttributes'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    schemaARN :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListFacetAttributes' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'name' - The name of the facet whose attributes will be retrieved.
-- * 'nextToken' - The pagination token.
-- * 'schemaARN' - The ARN of the schema where the facet resides.
mkListFacetAttributes ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ListFacetAttributes
mkListFacetAttributes pSchemaARN_ pName_ =
  ListFacetAttributes'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      schemaARN = pSchemaARN_,
      name = pName_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaNextToken :: Lens.Lens' ListFacetAttributes (Lude.Maybe Lude.Text)
lfaNextToken = Lens.lens (nextToken :: ListFacetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFacetAttributes)
{-# DEPRECATED lfaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaMaxResults :: Lens.Lens' ListFacetAttributes (Lude.Maybe Lude.Natural)
lfaMaxResults = Lens.lens (maxResults :: ListFacetAttributes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListFacetAttributes)
{-# DEPRECATED lfaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ARN of the schema where the facet resides.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaSchemaARN :: Lens.Lens' ListFacetAttributes Lude.Text
lfaSchemaARN = Lens.lens (schemaARN :: ListFacetAttributes -> Lude.Text) (\s a -> s {schemaARN = a} :: ListFacetAttributes)
{-# DEPRECATED lfaSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The name of the facet whose attributes will be retrieved.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfaName :: Lens.Lens' ListFacetAttributes Lude.Text
lfaName = Lens.lens (name :: ListFacetAttributes -> Lude.Text) (\s a -> s {name = a} :: ListFacetAttributes)
{-# DEPRECATED lfaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Page.AWSPager ListFacetAttributes where
  page rq rs
    | Page.stop (rs Lens.^. lfarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lfarsAttributes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lfaNextToken Lens..~ rs Lens.^. lfarsNextToken

instance Lude.AWSRequest ListFacetAttributes where
  type Rs ListFacetAttributes = ListFacetAttributesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListFacetAttributesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListFacetAttributes where
  toHeaders ListFacetAttributes' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON ListFacetAttributes where
  toJSON ListFacetAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath ListFacetAttributes where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/facet/attributes"

instance Lude.ToQuery ListFacetAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListFacetAttributesResponse' smart constructor.
data ListFacetAttributesResponse = ListFacetAttributesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe [FacetAttribute],
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

-- | Creates a value of 'ListFacetAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes attached to the facet.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListFacetAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListFacetAttributesResponse
mkListFacetAttributesResponse pResponseStatus_ =
  ListFacetAttributesResponse'
    { nextToken = Lude.Nothing,
      attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfarsNextToken :: Lens.Lens' ListFacetAttributesResponse (Lude.Maybe Lude.Text)
lfarsNextToken = Lens.lens (nextToken :: ListFacetAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListFacetAttributesResponse)
{-# DEPRECATED lfarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The attributes attached to the facet.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfarsAttributes :: Lens.Lens' ListFacetAttributesResponse (Lude.Maybe [FacetAttribute])
lfarsAttributes = Lens.lens (attributes :: ListFacetAttributesResponse -> Lude.Maybe [FacetAttribute]) (\s a -> s {attributes = a} :: ListFacetAttributesResponse)
{-# DEPRECATED lfarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfarsResponseStatus :: Lens.Lens' ListFacetAttributesResponse Lude.Int
lfarsResponseStatus = Lens.lens (responseStatus :: ListFacetAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListFacetAttributesResponse)
{-# DEPRECATED lfarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
