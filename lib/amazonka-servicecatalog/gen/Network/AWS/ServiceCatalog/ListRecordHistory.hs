{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListRecordHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified requests or all performed requests.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListRecordHistory
    (
    -- * Creating a request
      ListRecordHistory (..)
    , mkListRecordHistory
    -- ** Request lenses
    , lrhAcceptLanguage
    , lrhAccessLevelFilter
    , lrhPageSize
    , lrhPageToken
    , lrhSearchFilter

    -- * Destructuring the response
    , ListRecordHistoryResponse (..)
    , mkListRecordHistoryResponse
    -- ** Response lenses
    , lrhrrsNextPageToken
    , lrhrrsRecordDetails
    , lrhrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListRecordHistory' smart constructor.
data ListRecordHistory = ListRecordHistory'
  { acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  , accessLevelFilter :: Core.Maybe Types.AccessLevelFilter
    -- ^ The access level to use to obtain results. The default is @User@ .
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  , searchFilter :: Core.Maybe Types.ListRecordHistorySearchFilter
    -- ^ The search filter to scope the results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRecordHistory' value with any optional fields omitted.
mkListRecordHistory
    :: ListRecordHistory
mkListRecordHistory
  = ListRecordHistory'{acceptLanguage = Core.Nothing,
                       accessLevelFilter = Core.Nothing, pageSize = Core.Nothing,
                       pageToken = Core.Nothing, searchFilter = Core.Nothing}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhAcceptLanguage :: Lens.Lens' ListRecordHistory (Core.Maybe Types.AcceptLanguage)
lrhAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lrhAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The access level to use to obtain results. The default is @User@ .
--
-- /Note:/ Consider using 'accessLevelFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhAccessLevelFilter :: Lens.Lens' ListRecordHistory (Core.Maybe Types.AccessLevelFilter)
lrhAccessLevelFilter = Lens.field @"accessLevelFilter"
{-# INLINEABLE lrhAccessLevelFilter #-}
{-# DEPRECATED accessLevelFilter "Use generic-lens or generic-optics with 'accessLevelFilter' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhPageSize :: Lens.Lens' ListRecordHistory (Core.Maybe Core.Natural)
lrhPageSize = Lens.field @"pageSize"
{-# INLINEABLE lrhPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhPageToken :: Lens.Lens' ListRecordHistory (Core.Maybe Types.PageToken)
lrhPageToken = Lens.field @"pageToken"
{-# INLINEABLE lrhPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

-- | The search filter to scope the results.
--
-- /Note:/ Consider using 'searchFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhSearchFilter :: Lens.Lens' ListRecordHistory (Core.Maybe Types.ListRecordHistorySearchFilter)
lrhSearchFilter = Lens.field @"searchFilter"
{-# INLINEABLE lrhSearchFilter #-}
{-# DEPRECATED searchFilter "Use generic-lens or generic-optics with 'searchFilter' instead"  #-}

instance Core.ToQuery ListRecordHistory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListRecordHistory where
        toHeaders ListRecordHistory{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.ListRecordHistory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListRecordHistory where
        toJSON ListRecordHistory{..}
          = Core.object
              (Core.catMaybes
                 [("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("AccessLevelFilter" Core..=) Core.<$> accessLevelFilter,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken,
                  ("SearchFilter" Core..=) Core.<$> searchFilter])

instance Core.AWSRequest ListRecordHistory where
        type Rs ListRecordHistory = ListRecordHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListRecordHistoryResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "RecordDetails"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListRecordHistory where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"recordDetails" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkListRecordHistoryResponse' smart constructor.
data ListRecordHistoryResponse = ListRecordHistoryResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , recordDetails :: Core.Maybe [Types.RecordDetail]
    -- ^ The records, in reverse chronological order.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListRecordHistoryResponse' value with any optional fields omitted.
mkListRecordHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListRecordHistoryResponse
mkListRecordHistoryResponse responseStatus
  = ListRecordHistoryResponse'{nextPageToken = Core.Nothing,
                               recordDetails = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhrrsNextPageToken :: Lens.Lens' ListRecordHistoryResponse (Core.Maybe Types.NextPageToken)
lrhrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lrhrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The records, in reverse chronological order.
--
-- /Note:/ Consider using 'recordDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhrrsRecordDetails :: Lens.Lens' ListRecordHistoryResponse (Core.Maybe [Types.RecordDetail])
lrhrrsRecordDetails = Lens.field @"recordDetails"
{-# INLINEABLE lrhrrsRecordDetails #-}
{-# DEPRECATED recordDetails "Use generic-lens or generic-optics with 'recordDetails' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrhrrsResponseStatus :: Lens.Lens' ListRecordHistoryResponse Core.Int
lrhrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrhrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
