{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ListIpRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the address blocks that you have added to a directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListIpRoutes
    (
    -- * Creating a request
      ListIpRoutes (..)
    , mkListIpRoutes
    -- ** Request lenses
    , lirDirectoryId
    , lirLimit
    , lirNextToken

    -- * Destructuring the response
    , ListIpRoutesResponse (..)
    , mkListIpRoutesResponse
    -- ** Response lenses
    , lirrrsIpRoutesInfo
    , lirrrsNextToken
    , lirrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListIpRoutes' smart constructor.
data ListIpRoutes = ListIpRoutes'
  { directoryId :: Types.DirectoryId
    -- ^ Identifier (ID) of the directory for which you want to retrieve the IP addresses.
  , limit :: Core.Maybe Core.Natural
    -- ^ Maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The /ListIpRoutes.NextToken/ value from a previous call to 'ListIpRoutes' . Pass null if this is the first call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIpRoutes' value with any optional fields omitted.
mkListIpRoutes
    :: Types.DirectoryId -- ^ 'directoryId'
    -> ListIpRoutes
mkListIpRoutes directoryId
  = ListIpRoutes'{directoryId, limit = Core.Nothing,
                  nextToken = Core.Nothing}

-- | Identifier (ID) of the directory for which you want to retrieve the IP addresses.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirDirectoryId :: Lens.Lens' ListIpRoutes Types.DirectoryId
lirDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE lirDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | Maximum number of items to return. If this value is zero, the maximum number of items is specified by the limitations of the operation.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirLimit :: Lens.Lens' ListIpRoutes (Core.Maybe Core.Natural)
lirLimit = Lens.field @"limit"
{-# INLINEABLE lirLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The /ListIpRoutes.NextToken/ value from a previous call to 'ListIpRoutes' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirNextToken :: Lens.Lens' ListIpRoutes (Core.Maybe Types.NextToken)
lirNextToken = Lens.field @"nextToken"
{-# INLINEABLE lirNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListIpRoutes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListIpRoutes where
        toHeaders ListIpRoutes{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.ListIpRoutes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListIpRoutes where
        toJSON ListIpRoutes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListIpRoutes where
        type Rs ListIpRoutes = ListIpRoutesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListIpRoutesResponse' Core.<$>
                   (x Core..:? "IpRoutesInfo") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListIpRoutes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"ipRoutesInfo" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListIpRoutesResponse' smart constructor.
data ListIpRoutesResponse = ListIpRoutesResponse'
  { ipRoutesInfo :: Core.Maybe [Types.IpRouteInfo]
    -- ^ A list of 'IpRoute' s.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'ListIpRoutes' to retrieve the next set of items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListIpRoutesResponse' value with any optional fields omitted.
mkListIpRoutesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListIpRoutesResponse
mkListIpRoutesResponse responseStatus
  = ListIpRoutesResponse'{ipRoutesInfo = Core.Nothing,
                          nextToken = Core.Nothing, responseStatus}

-- | A list of 'IpRoute' s.
--
-- /Note:/ Consider using 'ipRoutesInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrrsIpRoutesInfo :: Lens.Lens' ListIpRoutesResponse (Core.Maybe [Types.IpRouteInfo])
lirrrsIpRoutesInfo = Lens.field @"ipRoutesInfo"
{-# INLINEABLE lirrrsIpRoutesInfo #-}
{-# DEPRECATED ipRoutesInfo "Use generic-lens or generic-optics with 'ipRoutesInfo' instead"  #-}

-- | If not null, more results are available. Pass this value for the /NextToken/ parameter in a subsequent call to 'ListIpRoutes' to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrrsNextToken :: Lens.Lens' ListIpRoutesResponse (Core.Maybe Types.NextToken)
lirrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lirrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrrsResponseStatus :: Lens.Lens' ListIpRoutesResponse Core.Int
lirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
