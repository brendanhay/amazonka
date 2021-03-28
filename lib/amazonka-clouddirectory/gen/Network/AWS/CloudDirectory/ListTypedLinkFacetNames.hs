{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListTypedLinkFacetNames (..)
    , mkListTypedLinkFacetNames
    -- ** Request lenses
    , ltlfnSchemaArn
    , ltlfnMaxResults
    , ltlfnNextToken

    -- * Destructuring the response
    , ListTypedLinkFacetNamesResponse (..)
    , mkListTypedLinkFacetNamesResponse
    -- ** Response lenses
    , ltlfnrrsFacetNames
    , ltlfnrrsNextToken
    , ltlfnrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListTypedLinkFacetNames' smart constructor.
data ListTypedLinkFacetNames = ListTypedLinkFacetNames'
  { schemaArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypedLinkFacetNames' value with any optional fields omitted.
mkListTypedLinkFacetNames
    :: Types.Arn -- ^ 'schemaArn'
    -> ListTypedLinkFacetNames
mkListTypedLinkFacetNames schemaArn
  = ListTypedLinkFacetNames'{schemaArn, maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnSchemaArn :: Lens.Lens' ListTypedLinkFacetNames Types.Arn
ltlfnSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE ltlfnSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnMaxResults :: Lens.Lens' ListTypedLinkFacetNames (Core.Maybe Core.Natural)
ltlfnMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltlfnMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnNextToken :: Lens.Lens' ListTypedLinkFacetNames (Core.Maybe Types.NextToken)
ltlfnNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltlfnNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListTypedLinkFacetNames where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListTypedLinkFacetNames where
        toHeaders ListTypedLinkFacetNames{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON ListTypedLinkFacetNames where
        toJSON ListTypedLinkFacetNames{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListTypedLinkFacetNames where
        type Rs ListTypedLinkFacetNames = ListTypedLinkFacetNamesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/typedlink/facet/list",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListTypedLinkFacetNamesResponse' Core.<$>
                   (x Core..:? "FacetNames") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListTypedLinkFacetNames where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"facetNames" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListTypedLinkFacetNamesResponse' smart constructor.
data ListTypedLinkFacetNamesResponse = ListTypedLinkFacetNamesResponse'
  { facetNames :: Core.Maybe [Types.TypedLinkName]
    -- ^ The names of typed link facets that exist within the schema.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTypedLinkFacetNamesResponse' value with any optional fields omitted.
mkListTypedLinkFacetNamesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListTypedLinkFacetNamesResponse
mkListTypedLinkFacetNamesResponse responseStatus
  = ListTypedLinkFacetNamesResponse'{facetNames = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | The names of typed link facets that exist within the schema.
--
-- /Note:/ Consider using 'facetNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnrrsFacetNames :: Lens.Lens' ListTypedLinkFacetNamesResponse (Core.Maybe [Types.TypedLinkName])
ltlfnrrsFacetNames = Lens.field @"facetNames"
{-# INLINEABLE ltlfnrrsFacetNames #-}
{-# DEPRECATED facetNames "Use generic-lens or generic-optics with 'facetNames' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnrrsNextToken :: Lens.Lens' ListTypedLinkFacetNamesResponse (Core.Maybe Types.NextToken)
ltlfnrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltlfnrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltlfnrrsResponseStatus :: Lens.Lens' ListTypedLinkFacetNamesResponse Core.Int
ltlfnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltlfnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
