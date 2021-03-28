{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches devices and lists the ones that meet a set of filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchDevices
    (
    -- * Creating a request
      SearchDevices (..)
    , mkSearchDevices
    -- ** Request lenses
    , sdFilters
    , sdMaxResults
    , sdNextToken
    , sdSortCriteria

    -- * Destructuring the response
    , SearchDevicesResponse (..)
    , mkSearchDevicesResponse
    -- ** Response lenses
    , sdrrsDevices
    , sdrrsNextToken
    , sdrrsTotalCount
    , sdrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSearchDevices' smart constructor.
data SearchDevices = SearchDevices'
  { filters :: Core.Maybe [Types.Filter]
    -- ^ The filters to use to list a specified set of devices. Supported filter keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName, DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
  , sortCriteria :: Core.Maybe [Types.Sort]
    -- ^ The sort order to use in listing the specified set of devices. Supported sort keys are DeviceName, DeviceStatus, RoomName, DeviceType, DeviceSerialNumber, ConnectionStatus, NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchDevices' value with any optional fields omitted.
mkSearchDevices
    :: SearchDevices
mkSearchDevices
  = SearchDevices'{filters = Core.Nothing, maxResults = Core.Nothing,
                   nextToken = Core.Nothing, sortCriteria = Core.Nothing}

-- | The filters to use to list a specified set of devices. Supported filter keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName, DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdFilters :: Lens.Lens' SearchDevices (Core.Maybe [Types.Filter])
sdFilters = Lens.field @"filters"
{-# INLINEABLE sdFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMaxResults :: Lens.Lens' SearchDevices (Core.Maybe Core.Natural)
sdMaxResults = Lens.field @"maxResults"
{-# INLINEABLE sdMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdNextToken :: Lens.Lens' SearchDevices (Core.Maybe Types.NextToken)
sdNextToken = Lens.field @"nextToken"
{-# INLINEABLE sdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The sort order to use in listing the specified set of devices. Supported sort keys are DeviceName, DeviceStatus, RoomName, DeviceType, DeviceSerialNumber, ConnectionStatus, NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSortCriteria :: Lens.Lens' SearchDevices (Core.Maybe [Types.Sort])
sdSortCriteria = Lens.field @"sortCriteria"
{-# INLINEABLE sdSortCriteria #-}
{-# DEPRECATED sortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead"  #-}

instance Core.ToQuery SearchDevices where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SearchDevices where
        toHeaders SearchDevices{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.SearchDevices")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SearchDevices where
        toJSON SearchDevices{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SortCriteria" Core..=) Core.<$> sortCriteria])

instance Core.AWSRequest SearchDevices where
        type Rs SearchDevices = SearchDevicesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SearchDevicesResponse' Core.<$>
                   (x Core..:? "Devices") Core.<*> x Core..:? "NextToken" Core.<*>
                     x Core..:? "TotalCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager SearchDevices where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"devices" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkSearchDevicesResponse' smart constructor.
data SearchDevicesResponse = SearchDevicesResponse'
  { devices :: Core.Maybe [Types.DeviceData]
    -- ^ The devices that meet the specified set of filter criteria, in sort order.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned to indicate that there is more data available.
  , totalCount :: Core.Maybe Core.Int
    -- ^ The total number of devices returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SearchDevicesResponse' value with any optional fields omitted.
mkSearchDevicesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SearchDevicesResponse
mkSearchDevicesResponse responseStatus
  = SearchDevicesResponse'{devices = Core.Nothing,
                           nextToken = Core.Nothing, totalCount = Core.Nothing,
                           responseStatus}

-- | The devices that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsDevices :: Lens.Lens' SearchDevicesResponse (Core.Maybe [Types.DeviceData])
sdrrsDevices = Lens.field @"devices"
{-# INLINEABLE sdrrsDevices #-}
{-# DEPRECATED devices "Use generic-lens or generic-optics with 'devices' instead"  #-}

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsNextToken :: Lens.Lens' SearchDevicesResponse (Core.Maybe Types.NextToken)
sdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE sdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The total number of devices returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsTotalCount :: Lens.Lens' SearchDevicesResponse (Core.Maybe Core.Int)
sdrrsTotalCount = Lens.field @"totalCount"
{-# INLINEABLE sdrrsTotalCount #-}
{-# DEPRECATED totalCount "Use generic-lens or generic-optics with 'totalCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrrsResponseStatus :: Lens.Lens' SearchDevicesResponse Core.Int
sdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
