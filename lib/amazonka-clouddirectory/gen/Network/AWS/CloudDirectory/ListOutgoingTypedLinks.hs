{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListOutgoingTypedLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListOutgoingTypedLinks
  ( -- * Creating a request
    ListOutgoingTypedLinks (..),
    mkListOutgoingTypedLinks,

    -- ** Request lenses
    lotlDirectoryARN,
    lotlFilterAttributeRanges,
    lotlConsistencyLevel,
    lotlNextToken,
    lotlFilterTypedLink,
    lotlObjectReference,
    lotlMaxResults,

    -- * Destructuring the response
    ListOutgoingTypedLinksResponse (..),
    mkListOutgoingTypedLinksResponse,

    -- ** Response lenses
    lotlrsTypedLinkSpecifiers,
    lotlrsNextToken,
    lotlrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListOutgoingTypedLinks' smart constructor.
data ListOutgoingTypedLinks = ListOutgoingTypedLinks'
  { -- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
    directoryARN :: Lude.Text,
    -- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
    filterAttributeRanges :: Lude.Maybe [TypedLinkAttributeRange],
    -- | The consistency level to execute the request at.
    consistencyLevel :: Lude.Maybe ConsistencyLevel,
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
    filterTypedLink :: Lude.Maybe TypedLinkSchemaAndFacetName,
    -- | A reference that identifies the object whose attributes will be listed.
    objectReference :: ObjectReference,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOutgoingTypedLinks' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
-- * 'filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
-- * 'consistencyLevel' - The consistency level to execute the request at.
-- * 'nextToken' - The pagination token.
-- * 'filterTypedLink' - Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
-- * 'objectReference' - A reference that identifies the object whose attributes will be listed.
-- * 'maxResults' - The maximum number of results to retrieve.
mkListOutgoingTypedLinks ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListOutgoingTypedLinks
mkListOutgoingTypedLinks pDirectoryARN_ pObjectReference_ =
  ListOutgoingTypedLinks'
    { directoryARN = pDirectoryARN_,
      filterAttributeRanges = Lude.Nothing,
      consistencyLevel = Lude.Nothing,
      nextToken = Lude.Nothing,
      filterTypedLink = Lude.Nothing,
      objectReference = pObjectReference_,
      maxResults = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlDirectoryARN :: Lens.Lens' ListOutgoingTypedLinks Lude.Text
lotlDirectoryARN = Lens.lens (directoryARN :: ListOutgoingTypedLinks -> Lude.Text) (\s a -> s {directoryARN = a} :: ListOutgoingTypedLinks)
{-# DEPRECATED lotlDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- /Note:/ Consider using 'filterAttributeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlFilterAttributeRanges :: Lens.Lens' ListOutgoingTypedLinks (Lude.Maybe [TypedLinkAttributeRange])
lotlFilterAttributeRanges = Lens.lens (filterAttributeRanges :: ListOutgoingTypedLinks -> Lude.Maybe [TypedLinkAttributeRange]) (\s a -> s {filterAttributeRanges = a} :: ListOutgoingTypedLinks)
{-# DEPRECATED lotlFilterAttributeRanges "Use generic-lens or generic-optics with 'filterAttributeRanges' instead." #-}

-- | The consistency level to execute the request at.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlConsistencyLevel :: Lens.Lens' ListOutgoingTypedLinks (Lude.Maybe ConsistencyLevel)
lotlConsistencyLevel = Lens.lens (consistencyLevel :: ListOutgoingTypedLinks -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListOutgoingTypedLinks)
{-# DEPRECATED lotlConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlNextToken :: Lens.Lens' ListOutgoingTypedLinks (Lude.Maybe Lude.Text)
lotlNextToken = Lens.lens (nextToken :: ListOutgoingTypedLinks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOutgoingTypedLinks)
{-# DEPRECATED lotlNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
--
-- /Note:/ Consider using 'filterTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlFilterTypedLink :: Lens.Lens' ListOutgoingTypedLinks (Lude.Maybe TypedLinkSchemaAndFacetName)
lotlFilterTypedLink = Lens.lens (filterTypedLink :: ListOutgoingTypedLinks -> Lude.Maybe TypedLinkSchemaAndFacetName) (\s a -> s {filterTypedLink = a} :: ListOutgoingTypedLinks)
{-# DEPRECATED lotlFilterTypedLink "Use generic-lens or generic-optics with 'filterTypedLink' instead." #-}

-- | A reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlObjectReference :: Lens.Lens' ListOutgoingTypedLinks ObjectReference
lotlObjectReference = Lens.lens (objectReference :: ListOutgoingTypedLinks -> ObjectReference) (\s a -> s {objectReference = a} :: ListOutgoingTypedLinks)
{-# DEPRECATED lotlObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlMaxResults :: Lens.Lens' ListOutgoingTypedLinks (Lude.Maybe Lude.Natural)
lotlMaxResults = Lens.lens (maxResults :: ListOutgoingTypedLinks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListOutgoingTypedLinks)
{-# DEPRECATED lotlMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListOutgoingTypedLinks where
  page rq rs
    | Page.stop (rs Lens.^. lotlrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lotlrsTypedLinkSpecifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lotlNextToken Lens..~ rs Lens.^. lotlrsNextToken

instance Lude.AWSRequest ListOutgoingTypedLinks where
  type Rs ListOutgoingTypedLinks = ListOutgoingTypedLinksResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListOutgoingTypedLinksResponse'
            Lude.<$> (x Lude..?> "TypedLinkSpecifiers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListOutgoingTypedLinks where
  toHeaders ListOutgoingTypedLinks' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON ListOutgoingTypedLinks where
  toJSON ListOutgoingTypedLinks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterAttributeRanges" Lude..=) Lude.<$> filterAttributeRanges,
            ("ConsistencyLevel" Lude..=) Lude.<$> consistencyLevel,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("FilterTypedLink" Lude..=) Lude.<$> filterTypedLink,
            Lude.Just ("ObjectReference" Lude..= objectReference),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListOutgoingTypedLinks where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/typedlink/outgoing"

instance Lude.ToQuery ListOutgoingTypedLinks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListOutgoingTypedLinksResponse' smart constructor.
data ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse'
  { -- | Returns a typed link specifier as output.
    typedLinkSpecifiers :: Lude.Maybe [TypedLinkSpecifier],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListOutgoingTypedLinksResponse' with the minimum fields required to make a request.
--
-- * 'typedLinkSpecifiers' - Returns a typed link specifier as output.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListOutgoingTypedLinksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListOutgoingTypedLinksResponse
mkListOutgoingTypedLinksResponse pResponseStatus_ =
  ListOutgoingTypedLinksResponse'
    { typedLinkSpecifiers =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlrsTypedLinkSpecifiers :: Lens.Lens' ListOutgoingTypedLinksResponse (Lude.Maybe [TypedLinkSpecifier])
lotlrsTypedLinkSpecifiers = Lens.lens (typedLinkSpecifiers :: ListOutgoingTypedLinksResponse -> Lude.Maybe [TypedLinkSpecifier]) (\s a -> s {typedLinkSpecifiers = a} :: ListOutgoingTypedLinksResponse)
{-# DEPRECATED lotlrsTypedLinkSpecifiers "Use generic-lens or generic-optics with 'typedLinkSpecifiers' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlrsNextToken :: Lens.Lens' ListOutgoingTypedLinksResponse (Lude.Maybe Lude.Text)
lotlrsNextToken = Lens.lens (nextToken :: ListOutgoingTypedLinksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListOutgoingTypedLinksResponse)
{-# DEPRECATED lotlrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlrsResponseStatus :: Lens.Lens' ListOutgoingTypedLinksResponse Lude.Int
lotlrsResponseStatus = Lens.lens (responseStatus :: ListOutgoingTypedLinksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListOutgoingTypedLinksResponse)
{-# DEPRECATED lotlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
