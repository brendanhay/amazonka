{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListDeviceDefinitionVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a device definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListDeviceDefinitionVersions
    (
    -- * Creating a request
      ListDeviceDefinitionVersions (..)
    , mkListDeviceDefinitionVersions
    -- ** Request lenses
    , lddvDeviceDefinitionId
    , lddvMaxResults
    , lddvNextToken

    -- * Destructuring the response
    , ListDeviceDefinitionVersionsResponse (..)
    , mkListDeviceDefinitionVersionsResponse
    -- ** Response lenses
    , lddvrrsNextToken
    , lddvrrsVersions
    , lddvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDeviceDefinitionVersions' smart constructor.
data ListDeviceDefinitionVersions = ListDeviceDefinitionVersions'
  { deviceDefinitionId :: Core.Text
    -- ^ The ID of the device definition.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceDefinitionVersions' value with any optional fields omitted.
mkListDeviceDefinitionVersions
    :: Core.Text -- ^ 'deviceDefinitionId'
    -> ListDeviceDefinitionVersions
mkListDeviceDefinitionVersions deviceDefinitionId
  = ListDeviceDefinitionVersions'{deviceDefinitionId,
                                  maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the device definition.
--
-- /Note:/ Consider using 'deviceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvDeviceDefinitionId :: Lens.Lens' ListDeviceDefinitionVersions Core.Text
lddvDeviceDefinitionId = Lens.field @"deviceDefinitionId"
{-# INLINEABLE lddvDeviceDefinitionId #-}
{-# DEPRECATED deviceDefinitionId "Use generic-lens or generic-optics with 'deviceDefinitionId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvMaxResults :: Lens.Lens' ListDeviceDefinitionVersions (Core.Maybe Core.Text)
lddvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lddvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvNextToken :: Lens.Lens' ListDeviceDefinitionVersions (Core.Maybe Core.Text)
lddvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lddvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDeviceDefinitionVersions where
        toQuery ListDeviceDefinitionVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListDeviceDefinitionVersions where
        toHeaders ListDeviceDefinitionVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListDeviceDefinitionVersions where
        type Rs ListDeviceDefinitionVersions =
             ListDeviceDefinitionVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/devices/" Core.<>
                             Core.toText deviceDefinitionId
                             Core.<> "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDeviceDefinitionVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDeviceDefinitionVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDeviceDefinitionVersionsResponse' smart constructor.
data ListDeviceDefinitionVersionsResponse = ListDeviceDefinitionVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , versions :: Core.Maybe [Types.VersionInformation]
    -- ^ Information about a version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDeviceDefinitionVersionsResponse' value with any optional fields omitted.
mkListDeviceDefinitionVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDeviceDefinitionVersionsResponse
mkListDeviceDefinitionVersionsResponse responseStatus
  = ListDeviceDefinitionVersionsResponse'{nextToken = Core.Nothing,
                                          versions = Core.Nothing, responseStatus}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrrsNextToken :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Core.Maybe Core.Text)
lddvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lddvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrrsVersions :: Lens.Lens' ListDeviceDefinitionVersionsResponse (Core.Maybe [Types.VersionInformation])
lddvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lddvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lddvrrsResponseStatus :: Lens.Lens' ListDeviceDefinitionVersionsResponse Core.Int
lddvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lddvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
