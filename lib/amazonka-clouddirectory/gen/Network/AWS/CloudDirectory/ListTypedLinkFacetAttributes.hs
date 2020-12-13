{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute definitions for a particular 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
  ( -- * Creating a request
    ListTypedLinkFacetAttributes (..),
    mkListTypedLinkFacetAttributes,

    -- ** Request lenses
    ltlfaNextToken,
    ltlfaSchemaARN,
    ltlfaName,
    ltlfaMaxResults,

    -- * Destructuring the response
    ListTypedLinkFacetAttributesResponse (..),
    mkListTypedLinkFacetAttributesResponse,

    -- ** Response lenses
    ltlfarsNextToken,
    ltlfarsAttributes,
    ltlfarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTypedLinkFacetAttributes' smart constructor.
data ListTypedLinkFacetAttributes = ListTypedLinkFacetAttributes'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaARN :: Lude.Text,
    -- | The unique name of the typed link facet.
    name :: Lude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTypedLinkFacetAttributes' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
-- * 'name' - The unique name of the typed link facet.
-- * 'maxResults' - The maximum number of results to retrieve.
mkListTypedLinkFacetAttributes ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ListTypedLinkFacetAttributes
mkListTypedLinkFacetAttributes pSchemaARN_ pName_ =
  ListTypedLinkFacetAttributes'
    { nextToken = Lude.Nothing,
      schemaARN = pSchemaARN_,
      name = pName_,
      maxResults = Lude.Nothing
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaNextToken :: Lens.Lens' ListTypedLinkFacetAttributes (Lude.Maybe Lude.Text)
ltlfaNextToken = Lens.lens (nextToken :: ListTypedLinkFacetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypedLinkFacetAttributes)
{-# DEPRECATED ltlfaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaSchemaARN :: Lens.Lens' ListTypedLinkFacetAttributes Lude.Text
ltlfaSchemaARN = Lens.lens (schemaARN :: ListTypedLinkFacetAttributes -> Lude.Text) (\s a -> s {schemaARN = a} :: ListTypedLinkFacetAttributes)
{-# DEPRECATED ltlfaSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaName :: Lens.Lens' ListTypedLinkFacetAttributes Lude.Text
ltlfaName = Lens.lens (name :: ListTypedLinkFacetAttributes -> Lude.Text) (\s a -> s {name = a} :: ListTypedLinkFacetAttributes)
{-# DEPRECATED ltlfaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfaMaxResults :: Lens.Lens' ListTypedLinkFacetAttributes (Lude.Maybe Lude.Natural)
ltlfaMaxResults = Lens.lens (maxResults :: ListTypedLinkFacetAttributes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTypedLinkFacetAttributes)
{-# DEPRECATED ltlfaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTypedLinkFacetAttributes where
  page rq rs
    | Page.stop (rs Lens.^. ltlfarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltlfarsAttributes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltlfaNextToken Lens..~ rs Lens.^. ltlfarsNextToken

instance Lude.AWSRequest ListTypedLinkFacetAttributes where
  type
    Rs ListTypedLinkFacetAttributes =
      ListTypedLinkFacetAttributesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTypedLinkFacetAttributesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTypedLinkFacetAttributes where
  toHeaders ListTypedLinkFacetAttributes' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON ListTypedLinkFacetAttributes where
  toJSON ListTypedLinkFacetAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("Name" Lude..= name),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTypedLinkFacetAttributes where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/attributes"

instance Lude.ToQuery ListTypedLinkFacetAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTypedLinkFacetAttributesResponse' smart constructor.
data ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse'
  { -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An ordered set of attributes associate with the typed link.
    attributes :: Lude.Maybe [TypedLinkAttributeDefinition],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTypedLinkFacetAttributesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token.
-- * 'attributes' - An ordered set of attributes associate with the typed link.
-- * 'responseStatus' - The response status code.
mkListTypedLinkFacetAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTypedLinkFacetAttributesResponse
mkListTypedLinkFacetAttributesResponse pResponseStatus_ =
  ListTypedLinkFacetAttributesResponse'
    { nextToken = Lude.Nothing,
      attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfarsNextToken :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Lude.Maybe Lude.Text)
ltlfarsNextToken = Lens.lens (nextToken :: ListTypedLinkFacetAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTypedLinkFacetAttributesResponse)
{-# DEPRECATED ltlfarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An ordered set of attributes associate with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfarsAttributes :: Lens.Lens' ListTypedLinkFacetAttributesResponse (Lude.Maybe [TypedLinkAttributeDefinition])
ltlfarsAttributes = Lens.lens (attributes :: ListTypedLinkFacetAttributesResponse -> Lude.Maybe [TypedLinkAttributeDefinition]) (\s a -> s {attributes = a} :: ListTypedLinkFacetAttributesResponse)
{-# DEPRECATED ltlfarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfarsResponseStatus :: Lens.Lens' ListTypedLinkFacetAttributesResponse Lude.Int
ltlfarsResponseStatus = Lens.lens (responseStatus :: ListTypedLinkFacetAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTypedLinkFacetAttributesResponse)
{-# DEPRECATED ltlfarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
