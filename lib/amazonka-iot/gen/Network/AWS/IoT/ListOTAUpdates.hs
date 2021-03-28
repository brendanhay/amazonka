{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListOTAUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists OTA updates.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListOTAUpdates
    (
    -- * Creating a request
      ListOTAUpdates (..)
    , mkListOTAUpdates
    -- ** Request lenses
    , lotauMaxResults
    , lotauNextToken
    , lotauOtaUpdateStatus

    -- * Destructuring the response
    , ListOTAUpdatesResponse (..)
    , mkListOTAUpdatesResponse
    -- ** Response lenses
    , lotaurrsNextToken
    , lotaurrsOtaUpdates
    , lotaurrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOTAUpdates' smart constructor.
data ListOTAUpdates = ListOTAUpdates'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token used to retrieve the next set of results.
  , otaUpdateStatus :: Core.Maybe Types.OTAUpdateStatus
    -- ^ The OTA update job status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOTAUpdates' value with any optional fields omitted.
mkListOTAUpdates
    :: ListOTAUpdates
mkListOTAUpdates
  = ListOTAUpdates'{maxResults = Core.Nothing,
                    nextToken = Core.Nothing, otaUpdateStatus = Core.Nothing}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauMaxResults :: Lens.Lens' ListOTAUpdates (Core.Maybe Core.Natural)
lotauMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lotauMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token used to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauNextToken :: Lens.Lens' ListOTAUpdates (Core.Maybe Types.NextToken)
lotauNextToken = Lens.field @"nextToken"
{-# INLINEABLE lotauNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The OTA update job status.
--
-- /Note:/ Consider using 'otaUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotauOtaUpdateStatus :: Lens.Lens' ListOTAUpdates (Core.Maybe Types.OTAUpdateStatus)
lotauOtaUpdateStatus = Lens.field @"otaUpdateStatus"
{-# INLINEABLE lotauOtaUpdateStatus #-}
{-# DEPRECATED otaUpdateStatus "Use generic-lens or generic-optics with 'otaUpdateStatus' instead"  #-}

instance Core.ToQuery ListOTAUpdates where
        toQuery ListOTAUpdates{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "otaUpdateStatus")
                otaUpdateStatus

instance Core.ToHeaders ListOTAUpdates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListOTAUpdates where
        type Rs ListOTAUpdates = ListOTAUpdatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/otaUpdates",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListOTAUpdatesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "otaUpdates" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListOTAUpdates where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"otaUpdates" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListOTAUpdatesResponse' smart constructor.
data ListOTAUpdatesResponse = ListOTAUpdatesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to use to get the next set of results.
  , otaUpdates :: Core.Maybe [Types.OTAUpdateSummary]
    -- ^ A list of OTA update jobs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListOTAUpdatesResponse' value with any optional fields omitted.
mkListOTAUpdatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOTAUpdatesResponse
mkListOTAUpdatesResponse responseStatus
  = ListOTAUpdatesResponse'{nextToken = Core.Nothing,
                            otaUpdates = Core.Nothing, responseStatus}

-- | A token to use to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaurrsNextToken :: Lens.Lens' ListOTAUpdatesResponse (Core.Maybe Types.NextToken)
lotaurrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lotaurrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of OTA update jobs.
--
-- /Note:/ Consider using 'otaUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaurrsOtaUpdates :: Lens.Lens' ListOTAUpdatesResponse (Core.Maybe [Types.OTAUpdateSummary])
lotaurrsOtaUpdates = Lens.field @"otaUpdates"
{-# INLINEABLE lotaurrsOtaUpdates #-}
{-# DEPRECATED otaUpdates "Use generic-lens or generic-optics with 'otaUpdates' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lotaurrsResponseStatus :: Lens.Lens' ListOTAUpdatesResponse Core.Int
lotaurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lotaurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
