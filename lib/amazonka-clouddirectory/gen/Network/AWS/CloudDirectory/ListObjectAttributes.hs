{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all attributes that are associated with an object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectAttributes
  ( -- * Creating a request
    ListObjectAttributes (..),
    mkListObjectAttributes,

    -- ** Request lenses
    loaFacetFilter,
    loaConsistencyLevel,
    loaNextToken,
    loaMaxResults,
    loaDirectoryARN,
    loaObjectReference,

    -- * Destructuring the response
    ListObjectAttributesResponse (..),
    mkListObjectAttributesResponse,

    -- ** Response lenses
    loarsNextToken,
    loarsAttributes,
    loarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListObjectAttributes' smart constructor.
data ListObjectAttributes = ListObjectAttributes'
  { facetFilter ::
      Lude.Maybe SchemaFacet,
    consistencyLevel :: Lude.Maybe ConsistencyLevel,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    directoryARN :: Lude.Text,
    objectReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListObjectAttributes' with the minimum fields required to make a request.
--
-- * 'consistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
-- * 'facetFilter' - Used to filter the list of object attributes that are associated with a certain facet.
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - The reference that identifies the object whose attributes will be listed.
mkListObjectAttributes ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListObjectAttributes
mkListObjectAttributes pDirectoryARN_ pObjectReference_ =
  ListObjectAttributes'
    { facetFilter = Lude.Nothing,
      consistencyLevel = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      objectReference = pObjectReference_
    }

-- | Used to filter the list of object attributes that are associated with a certain facet.
--
-- /Note:/ Consider using 'facetFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaFacetFilter :: Lens.Lens' ListObjectAttributes (Lude.Maybe SchemaFacet)
loaFacetFilter = Lens.lens (facetFilter :: ListObjectAttributes -> Lude.Maybe SchemaFacet) (\s a -> s {facetFilter = a} :: ListObjectAttributes)
{-# DEPRECATED loaFacetFilter "Use generic-lens or generic-optics with 'facetFilter' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaConsistencyLevel :: Lens.Lens' ListObjectAttributes (Lude.Maybe ConsistencyLevel)
loaConsistencyLevel = Lens.lens (consistencyLevel :: ListObjectAttributes -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListObjectAttributes)
{-# DEPRECATED loaConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaNextToken :: Lens.Lens' ListObjectAttributes (Lude.Maybe Lude.Text)
loaNextToken = Lens.lens (nextToken :: ListObjectAttributes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectAttributes)
{-# DEPRECATED loaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaMaxResults :: Lens.Lens' ListObjectAttributes (Lude.Maybe Lude.Natural)
loaMaxResults = Lens.lens (maxResults :: ListObjectAttributes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListObjectAttributes)
{-# DEPRECATED loaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaDirectoryARN :: Lens.Lens' ListObjectAttributes Lude.Text
loaDirectoryARN = Lens.lens (directoryARN :: ListObjectAttributes -> Lude.Text) (\s a -> s {directoryARN = a} :: ListObjectAttributes)
{-# DEPRECATED loaDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaObjectReference :: Lens.Lens' ListObjectAttributes ObjectReference
loaObjectReference = Lens.lens (objectReference :: ListObjectAttributes -> ObjectReference) (\s a -> s {objectReference = a} :: ListObjectAttributes)
{-# DEPRECATED loaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Page.AWSPager ListObjectAttributes where
  page rq rs
    | Page.stop (rs Lens.^. loarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. loarsAttributes) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& loaNextToken Lens..~ rs Lens.^. loarsNextToken

instance Lude.AWSRequest ListObjectAttributes where
  type Rs ListObjectAttributes = ListObjectAttributesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListObjectAttributesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListObjectAttributes where
  toHeaders ListObjectAttributes' {..} =
    Lude.mconcat
      [ "x-amz-consistency-level" Lude.=# consistencyLevel,
        "x-amz-data-partition" Lude.=# directoryARN
      ]

instance Lude.ToJSON ListObjectAttributes where
  toJSON ListObjectAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FacetFilter" Lude..=) Lude.<$> facetFilter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath ListObjectAttributes where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/attributes"

instance Lude.ToQuery ListObjectAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListObjectAttributesResponse' smart constructor.
data ListObjectAttributesResponse = ListObjectAttributesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe [AttributeKeyAndValue],
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

-- | Creates a value of 'ListObjectAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Attributes map that is associated with the object. @AttributeArn@ is the key, and attribute value is the value.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListObjectAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListObjectAttributesResponse
mkListObjectAttributesResponse pResponseStatus_ =
  ListObjectAttributesResponse'
    { nextToken = Lude.Nothing,
      attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loarsNextToken :: Lens.Lens' ListObjectAttributesResponse (Lude.Maybe Lude.Text)
loarsNextToken = Lens.lens (nextToken :: ListObjectAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListObjectAttributesResponse)
{-# DEPRECATED loarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Attributes map that is associated with the object. @AttributeArn@ is the key, and attribute value is the value.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loarsAttributes :: Lens.Lens' ListObjectAttributesResponse (Lude.Maybe [AttributeKeyAndValue])
loarsAttributes = Lens.lens (attributes :: ListObjectAttributesResponse -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {attributes = a} :: ListObjectAttributesResponse)
{-# DEPRECATED loarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loarsResponseStatus :: Lens.Lens' ListObjectAttributesResponse Lude.Int
loarsResponseStatus = Lens.lens (responseStatus :: ListObjectAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListObjectAttributesResponse)
{-# DEPRECATED loarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
