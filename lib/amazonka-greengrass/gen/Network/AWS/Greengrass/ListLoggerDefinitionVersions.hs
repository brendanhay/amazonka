{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListLoggerDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a logger definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListLoggerDefinitionVersions
    (
    -- * Creating a request
      ListLoggerDefinitionVersions (..)
    , mkListLoggerDefinitionVersions
    -- ** Request lenses
    , lldvLoggerDefinitionId
    , lldvMaxResults
    , lldvNextToken

    -- * Destructuring the response
    , ListLoggerDefinitionVersionsResponse (..)
    , mkListLoggerDefinitionVersionsResponse
    -- ** Response lenses
    , lldvrrsNextToken
    , lldvrrsVersions
    , lldvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLoggerDefinitionVersions' smart constructor.
data ListLoggerDefinitionVersions = ListLoggerDefinitionVersions'
  { loggerDefinitionId :: Core.Text
    -- ^ The ID of the logger definition.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggerDefinitionVersions' value with any optional fields omitted.
mkListLoggerDefinitionVersions
    :: Core.Text -- ^ 'loggerDefinitionId'
    -> ListLoggerDefinitionVersions
mkListLoggerDefinitionVersions loggerDefinitionId
  = ListLoggerDefinitionVersions'{loggerDefinitionId,
                                  maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvLoggerDefinitionId :: Lens.Lens' ListLoggerDefinitionVersions Core.Text
lldvLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# INLINEABLE lldvLoggerDefinitionId #-}
{-# DEPRECATED loggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvMaxResults :: Lens.Lens' ListLoggerDefinitionVersions (Core.Maybe Core.Text)
lldvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lldvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvNextToken :: Lens.Lens' ListLoggerDefinitionVersions (Core.Maybe Core.Text)
lldvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lldvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListLoggerDefinitionVersions where
        toQuery ListLoggerDefinitionVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListLoggerDefinitionVersions where
        toHeaders ListLoggerDefinitionVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListLoggerDefinitionVersions where
        type Rs ListLoggerDefinitionVersions =
             ListLoggerDefinitionVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/loggers/" Core.<>
                             Core.toText loggerDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListLoggerDefinitionVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListLoggerDefinitionVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListLoggerDefinitionVersionsResponse' smart constructor.
data ListLoggerDefinitionVersionsResponse = ListLoggerDefinitionVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , versions :: Core.Maybe [Types.VersionInformation]
    -- ^ Information about a version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLoggerDefinitionVersionsResponse' value with any optional fields omitted.
mkListLoggerDefinitionVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListLoggerDefinitionVersionsResponse
mkListLoggerDefinitionVersionsResponse responseStatus
  = ListLoggerDefinitionVersionsResponse'{nextToken = Core.Nothing,
                                          versions = Core.Nothing, responseStatus}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrrsNextToken :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Core.Maybe Core.Text)
lldvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lldvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrrsVersions :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lldvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lldvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lldvrrsResponseStatus :: Lens.Lens' ListLoggerDefinitionVersionsResponse Core.Int
lldvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lldvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
