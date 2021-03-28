{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListIncomingTypedLinks (..)
    , mkListIncomingTypedLinks
    -- ** Request lenses
    , litlDirectoryArn
    , litlObjectReference
    , litlConsistencyLevel
    , litlFilterAttributeRanges
    , litlFilterTypedLink
    , litlMaxResults
    , litlNextToken

    -- * Destructuring the response
    , ListIncomingTypedLinksResponse (..)
    , mkListIncomingTypedLinksResponse
    -- ** Response lenses
    , litlrrsLinkSpecifiers
    , litlrrsNextToken
    , litlrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListIncomingTypedLinks' smart constructor.
data ListIncomingTypedLinks = ListIncomingTypedLinks'
  { directoryArn :: Types.DirectoryArn
    -- ^ The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
  , objectReference :: Types.ObjectReference
    -- ^ Reference that identifies the object whose attributes will be listed.
  , consistencyLevel :: Core.Maybe Types.ConsistencyLevel
    -- ^ The consistency level to execute the request at.
  , filterAttributeRanges :: Core.Maybe [Types.TypedLinkAttributeRange]
    -- ^ Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
  , filterTypedLink :: Core.Maybe Types.TypedLinkSchemaAndFacetName
    -- ^ Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListIncomingTypedLinks' value with any optional fields omitted.
mkListIncomingTypedLinks
    :: Types.DirectoryArn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> ListIncomingTypedLinks
mkListIncomingTypedLinks directoryArn objectReference
  = ListIncomingTypedLinks'{directoryArn, objectReference,
                            consistencyLevel = Core.Nothing,
                            filterAttributeRanges = Core.Nothing,
                            filterTypedLink = Core.Nothing, maxResults = Core.Nothing,
                            nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the directory where you want to list the typed links.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlDirectoryArn :: Lens.Lens' ListIncomingTypedLinks Types.DirectoryArn
litlDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE litlDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | Reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlObjectReference :: Lens.Lens' ListIncomingTypedLinks Types.ObjectReference
litlObjectReference = Lens.field @"objectReference"
{-# INLINEABLE litlObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | The consistency level to execute the request at.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlConsistencyLevel :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe Types.ConsistencyLevel)
litlConsistencyLevel = Lens.field @"consistencyLevel"
{-# INLINEABLE litlConsistencyLevel #-}
{-# DEPRECATED consistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead"  #-}

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- /Note:/ Consider using 'filterAttributeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlFilterAttributeRanges :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe [Types.TypedLinkAttributeRange])
litlFilterAttributeRanges = Lens.field @"filterAttributeRanges"
{-# INLINEABLE litlFilterAttributeRanges #-}
{-# DEPRECATED filterAttributeRanges "Use generic-lens or generic-optics with 'filterAttributeRanges' instead"  #-}

-- | Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
--
-- /Note:/ Consider using 'filterTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlFilterTypedLink :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe Types.TypedLinkSchemaAndFacetName)
litlFilterTypedLink = Lens.field @"filterTypedLink"
{-# INLINEABLE litlFilterTypedLink #-}
{-# DEPRECATED filterTypedLink "Use generic-lens or generic-optics with 'filterTypedLink' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlMaxResults :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe Core.Natural)
litlMaxResults = Lens.field @"maxResults"
{-# INLINEABLE litlMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlNextToken :: Lens.Lens' ListIncomingTypedLinks (Core.Maybe Types.NextToken)
litlNextToken = Lens.field @"nextToken"
{-# INLINEABLE litlNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListIncomingTypedLinks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListIncomingTypedLinks where
        toHeaders ListIncomingTypedLinks{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON ListIncomingTypedLinks where
        toJSON ListIncomingTypedLinks{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("ConsistencyLevel" Core..=) Core.<$> consistencyLevel,
                  ("FilterAttributeRanges" Core..=) Core.<$> filterAttributeRanges,
                  ("FilterTypedLink" Core..=) Core.<$> filterTypedLink,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListIncomingTypedLinks where
        type Rs ListIncomingTypedLinks = ListIncomingTypedLinksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/typedlink/incoming",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListIncomingTypedLinksResponse' Core.<$>
                   (x Core..:? "LinkSpecifiers") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListIncomingTypedLinks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"linkSpecifiers" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListIncomingTypedLinksResponse' smart constructor.
data ListIncomingTypedLinksResponse = ListIncomingTypedLinksResponse'
  { linkSpecifiers :: Core.Maybe [Types.TypedLinkSpecifier]
    -- ^ Returns one or more typed link specifiers as output.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListIncomingTypedLinksResponse' value with any optional fields omitted.
mkListIncomingTypedLinksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListIncomingTypedLinksResponse
mkListIncomingTypedLinksResponse responseStatus
  = ListIncomingTypedLinksResponse'{linkSpecifiers = Core.Nothing,
                                    nextToken = Core.Nothing, responseStatus}

-- | Returns one or more typed link specifiers as output.
--
-- /Note:/ Consider using 'linkSpecifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlrrsLinkSpecifiers :: Lens.Lens' ListIncomingTypedLinksResponse (Core.Maybe [Types.TypedLinkSpecifier])
litlrrsLinkSpecifiers = Lens.field @"linkSpecifiers"
{-# INLINEABLE litlrrsLinkSpecifiers #-}
{-# DEPRECATED linkSpecifiers "Use generic-lens or generic-optics with 'linkSpecifiers' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlrrsNextToken :: Lens.Lens' ListIncomingTypedLinksResponse (Core.Maybe Types.NextToken)
litlrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE litlrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
litlrrsResponseStatus :: Lens.Lens' ListIncomingTypedLinksResponse Core.Int
litlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE litlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
