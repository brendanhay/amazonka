{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListIncomingTypedLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListIncomingTypedLinks
  ( -- * Creating a request
    ListIncomingTypedLinks (..),
    mkListIncomingTypedLinks,

    -- ** Request lenses
    litlDirectoryARN,
    litlFilterAttributeRanges,
    litlConsistencyLevel,
    litlNextToken,
    litlFilterTypedLink,
    litlObjectReference,
    litlMaxResults,

    -- * Destructuring the response
    ListIncomingTypedLinksResponse (..),
    mkListIncomingTypedLinksResponse,

    -- ** Response lenses
    litlrsLinkSpecifiers,
    litlrsNextToken,
    litlrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListIncomingTypedLinks' smart constructor.
data ListIncomingTypedLinks = ListIncomingTypedLinks'
  { -- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
    directoryARN :: Lude.Text,
    -- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
    filterAttributeRanges :: Lude.Maybe [TypedLinkAttributeRange],
    -- | The consistency level to execute the request at.
    consistencyLevel :: Lude.Maybe ConsistencyLevel,
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
    filterTypedLink :: Lude.Maybe TypedLinkSchemaAndFacetName,
    -- | Reference that identifies the object whose attributes will be listed.
    objectReference :: ObjectReference,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIncomingTypedLinks' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
-- * 'filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
-- * 'consistencyLevel' - The consistency level to execute the request at.
-- * 'nextToken' - The pagination token.
-- * 'filterTypedLink' - Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
-- * 'objectReference' - Reference that identifies the object whose attributes will be listed.
-- * 'maxResults' - The maximum number of results to retrieve.
mkListIncomingTypedLinks ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  ListIncomingTypedLinks
mkListIncomingTypedLinks pDirectoryARN_ pObjectReference_ =
  ListIncomingTypedLinks'
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
litlDirectoryARN :: Lens.Lens' ListIncomingTypedLinks Lude.Text
litlDirectoryARN = Lens.lens (directoryARN :: ListIncomingTypedLinks -> Lude.Text) (\s a -> s {directoryARN = a} :: ListIncomingTypedLinks)
{-# DEPRECATED litlDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- /Note:/ Consider using 'filterAttributeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlFilterAttributeRanges :: Lens.Lens' ListIncomingTypedLinks (Lude.Maybe [TypedLinkAttributeRange])
litlFilterAttributeRanges = Lens.lens (filterAttributeRanges :: ListIncomingTypedLinks -> Lude.Maybe [TypedLinkAttributeRange]) (\s a -> s {filterAttributeRanges = a} :: ListIncomingTypedLinks)
{-# DEPRECATED litlFilterAttributeRanges "Use generic-lens or generic-optics with 'filterAttributeRanges' instead." #-}

-- | The consistency level to execute the request at.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlConsistencyLevel :: Lens.Lens' ListIncomingTypedLinks (Lude.Maybe ConsistencyLevel)
litlConsistencyLevel = Lens.lens (consistencyLevel :: ListIncomingTypedLinks -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: ListIncomingTypedLinks)
{-# DEPRECATED litlConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlNextToken :: Lens.Lens' ListIncomingTypedLinks (Lude.Maybe Lude.Text)
litlNextToken = Lens.lens (nextToken :: ListIncomingTypedLinks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIncomingTypedLinks)
{-# DEPRECATED litlNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
--
-- /Note:/ Consider using 'filterTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlFilterTypedLink :: Lens.Lens' ListIncomingTypedLinks (Lude.Maybe TypedLinkSchemaAndFacetName)
litlFilterTypedLink = Lens.lens (filterTypedLink :: ListIncomingTypedLinks -> Lude.Maybe TypedLinkSchemaAndFacetName) (\s a -> s {filterTypedLink = a} :: ListIncomingTypedLinks)
{-# DEPRECATED litlFilterTypedLink "Use generic-lens or generic-optics with 'filterTypedLink' instead." #-}

-- | Reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlObjectReference :: Lens.Lens' ListIncomingTypedLinks ObjectReference
litlObjectReference = Lens.lens (objectReference :: ListIncomingTypedLinks -> ObjectReference) (\s a -> s {objectReference = a} :: ListIncomingTypedLinks)
{-# DEPRECATED litlObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlMaxResults :: Lens.Lens' ListIncomingTypedLinks (Lude.Maybe Lude.Natural)
litlMaxResults = Lens.lens (maxResults :: ListIncomingTypedLinks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListIncomingTypedLinks)
{-# DEPRECATED litlMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListIncomingTypedLinks where
  page rq rs
    | Page.stop (rs Lens.^. litlrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. litlrsLinkSpecifiers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& litlNextToken Lens..~ rs Lens.^. litlrsNextToken

instance Lude.AWSRequest ListIncomingTypedLinks where
  type Rs ListIncomingTypedLinks = ListIncomingTypedLinksResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListIncomingTypedLinksResponse'
            Lude.<$> (x Lude..?> "LinkSpecifiers" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListIncomingTypedLinks where
  toHeaders ListIncomingTypedLinks' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON ListIncomingTypedLinks where
  toJSON ListIncomingTypedLinks' {..} =
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

instance Lude.ToPath ListIncomingTypedLinks where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/typedlink/incoming"

instance Lude.ToQuery ListIncomingTypedLinks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListIncomingTypedLinksResponse' smart constructor.
data ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse'
  { -- | Returns one or more typed link specifiers as output.
    linkSpecifiers :: Lude.Maybe [TypedLinkSpecifier],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIncomingTypedLinksResponse' with the minimum fields required to make a request.
--
-- * 'linkSpecifiers' - Returns one or more typed link specifiers as output.
-- * 'nextToken' - The pagination token.
-- * 'responseStatus' - The response status code.
mkListIncomingTypedLinksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIncomingTypedLinksResponse
mkListIncomingTypedLinksResponse pResponseStatus_ =
  ListIncomingTypedLinksResponse'
    { linkSpecifiers = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns one or more typed link specifiers as output.
--
-- /Note:/ Consider using 'linkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlrsLinkSpecifiers :: Lens.Lens' ListIncomingTypedLinksResponse (Lude.Maybe [TypedLinkSpecifier])
litlrsLinkSpecifiers = Lens.lens (linkSpecifiers :: ListIncomingTypedLinksResponse -> Lude.Maybe [TypedLinkSpecifier]) (\s a -> s {linkSpecifiers = a} :: ListIncomingTypedLinksResponse)
{-# DEPRECATED litlrsLinkSpecifiers "Use generic-lens or generic-optics with 'linkSpecifiers' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlrsNextToken :: Lens.Lens' ListIncomingTypedLinksResponse (Lude.Maybe Lude.Text)
litlrsNextToken = Lens.lens (nextToken :: ListIncomingTypedLinksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListIncomingTypedLinksResponse)
{-# DEPRECATED litlrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlrsResponseStatus :: Lens.Lens' ListIncomingTypedLinksResponse Lude.Int
litlrsResponseStatus = Lens.lens (responseStatus :: ListIncomingTypedLinksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIncomingTypedLinksResponse)
{-# DEPRECATED litlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
