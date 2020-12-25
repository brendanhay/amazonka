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
    lotlDirectoryArn,
    lotlObjectReference,
    lotlConsistencyLevel,
    lotlFilterAttributeRanges,
    lotlFilterTypedLink,
    lotlMaxResults,
    lotlNextToken,

    -- * Destructuring the response
    ListOutgoingTypedLinksResponse (..),
    mkListOutgoingTypedLinksResponse,

    -- ** Response lenses
    lotlrrsNextToken,
    lotlrrsTypedLinkSpecifiers,
    lotlrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOutgoingTypedLinks' smart constructor.
data ListOutgoingTypedLinks = ListOutgoingTypedLinks'
  { -- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
    directoryArn :: Types.Arn,
    -- | A reference that identifies the object whose attributes will be listed.
    objectReference :: Types.ObjectReference,
    -- | The consistency level to execute the request at.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel,
    -- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
    filterAttributeRanges :: Core.Maybe [Types.TypedLinkAttributeRange],
    -- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
    filterTypedLink :: Core.Maybe Types.TypedLinkSchemaAndFacetName,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOutgoingTypedLinks' value with any optional fields omitted.
mkListOutgoingTypedLinks ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'objectReference'
  Types.ObjectReference ->
  ListOutgoingTypedLinks
mkListOutgoingTypedLinks directoryArn objectReference =
  ListOutgoingTypedLinks'
    { directoryArn,
      objectReference,
      consistencyLevel = Core.Nothing,
      filterAttributeRanges = Core.Nothing,
      filterTypedLink = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlDirectoryArn :: Lens.Lens' ListOutgoingTypedLinks Types.Arn
lotlDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED lotlDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | A reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlObjectReference :: Lens.Lens' ListOutgoingTypedLinks Types.ObjectReference
lotlObjectReference = Lens.field @"objectReference"
{-# DEPRECATED lotlObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The consistency level to execute the request at.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlConsistencyLevel :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe Types.ConsistencyLevel)
lotlConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED lotlConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- /Note:/ Consider using 'filterAttributeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlFilterAttributeRanges :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe [Types.TypedLinkAttributeRange])
lotlFilterAttributeRanges = Lens.field @"filterAttributeRanges"
{-# DEPRECATED lotlFilterAttributeRanges "Use generic-lens or generic-optics with 'filterAttributeRanges' instead." #-}

-- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
--
-- /Note:/ Consider using 'filterTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlFilterTypedLink :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe Types.TypedLinkSchemaAndFacetName)
lotlFilterTypedLink = Lens.field @"filterTypedLink"
{-# DEPRECATED lotlFilterTypedLink "Use generic-lens or generic-optics with 'filterTypedLink' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlMaxResults :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe Core.Natural)
lotlMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lotlMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlNextToken :: Lens.Lens' ListOutgoingTypedLinks (Core.Maybe Types.NextToken)
lotlNextToken = Lens.field @"nextToken"
{-# DEPRECATED lotlNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListOutgoingTypedLinks where
  toJSON ListOutgoingTypedLinks {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("ConsistencyLevel" Core..=) Core.<$> consistencyLevel,
            ("FilterAttributeRanges" Core..=) Core.<$> filterAttributeRanges,
            ("FilterTypedLink" Core..=) Core.<$> filterTypedLink,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListOutgoingTypedLinks where
  type Rs ListOutgoingTypedLinks = ListOutgoingTypedLinksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/typedlink/outgoing",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutgoingTypedLinksResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TypedLinkSpecifiers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListOutgoingTypedLinks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"typedLinkSpecifiers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListOutgoingTypedLinksResponse' smart constructor.
data ListOutgoingTypedLinksResponse = ListOutgoingTypedLinksResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Returns a typed link specifier as output.
    typedLinkSpecifiers :: Core.Maybe [Types.TypedLinkSpecifier],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListOutgoingTypedLinksResponse' value with any optional fields omitted.
mkListOutgoingTypedLinksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOutgoingTypedLinksResponse
mkListOutgoingTypedLinksResponse responseStatus =
  ListOutgoingTypedLinksResponse'
    { nextToken = Core.Nothing,
      typedLinkSpecifiers = Core.Nothing,
      responseStatus
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlrrsNextToken :: Lens.Lens' ListOutgoingTypedLinksResponse (Core.Maybe Types.NextToken)
lotlrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lotlrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlrrsTypedLinkSpecifiers :: Lens.Lens' ListOutgoingTypedLinksResponse (Core.Maybe [Types.TypedLinkSpecifier])
lotlrrsTypedLinkSpecifiers = Lens.field @"typedLinkSpecifiers"
{-# DEPRECATED lotlrrsTypedLinkSpecifiers "Use generic-lens or generic-optics with 'typedLinkSpecifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotlrrsResponseStatus :: Lens.Lens' ListOutgoingTypedLinksResponse Core.Int
lotlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lotlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
