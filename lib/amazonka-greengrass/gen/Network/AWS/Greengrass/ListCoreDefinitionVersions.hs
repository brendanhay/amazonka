{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListCoreDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a core definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListCoreDefinitionVersions
    (
    -- * Creating a request
      ListCoreDefinitionVersions (..)
    , mkListCoreDefinitionVersions
    -- ** Request lenses
    , lcdvsCoreDefinitionId
    , lcdvsMaxResults
    , lcdvsNextToken

    -- * Destructuring the response
    , ListCoreDefinitionVersionsResponse (..)
    , mkListCoreDefinitionVersionsResponse
    -- ** Response lenses
    , lcdvrrsNextToken
    , lcdvrrsVersions
    , lcdvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCoreDefinitionVersions' smart constructor.
data ListCoreDefinitionVersions = ListCoreDefinitionVersions'
  { coreDefinitionId :: Core.Text
    -- ^ The ID of the core definition.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCoreDefinitionVersions' value with any optional fields omitted.
mkListCoreDefinitionVersions
    :: Core.Text -- ^ 'coreDefinitionId'
    -> ListCoreDefinitionVersions
mkListCoreDefinitionVersions coreDefinitionId
  = ListCoreDefinitionVersions'{coreDefinitionId,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsCoreDefinitionId :: Lens.Lens' ListCoreDefinitionVersions Core.Text
lcdvsCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# INLINEABLE lcdvsCoreDefinitionId #-}
{-# DEPRECATED coreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsMaxResults :: Lens.Lens' ListCoreDefinitionVersions (Core.Maybe Core.Text)
lcdvsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcdvsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvsNextToken :: Lens.Lens' ListCoreDefinitionVersions (Core.Maybe Core.Text)
lcdvsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcdvsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListCoreDefinitionVersions where
        toQuery ListCoreDefinitionVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListCoreDefinitionVersions where
        toHeaders ListCoreDefinitionVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListCoreDefinitionVersions where
        type Rs ListCoreDefinitionVersions =
             ListCoreDefinitionVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/cores/" Core.<>
                             Core.toText coreDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCoreDefinitionVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCoreDefinitionVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListCoreDefinitionVersionsResponse' smart constructor.
data ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , versions :: Core.Maybe [Types.VersionInformation]
    -- ^ Information about a version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCoreDefinitionVersionsResponse' value with any optional fields omitted.
mkListCoreDefinitionVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCoreDefinitionVersionsResponse
mkListCoreDefinitionVersionsResponse responseStatus
  = ListCoreDefinitionVersionsResponse'{nextToken = Core.Nothing,
                                        versions = Core.Nothing, responseStatus}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrrsNextToken :: Lens.Lens' ListCoreDefinitionVersionsResponse (Core.Maybe Core.Text)
lcdvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcdvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrrsVersions :: Lens.Lens' ListCoreDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lcdvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lcdvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdvrrsResponseStatus :: Lens.Lens' ListCoreDefinitionVersionsResponse Core.Int
lcdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
