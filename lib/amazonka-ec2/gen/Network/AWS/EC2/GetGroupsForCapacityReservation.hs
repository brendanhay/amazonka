{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetGroupsForCapacityReservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource groups to which a Capacity Reservation has been added.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetGroupsForCapacityReservation
    (
    -- * Creating a request
      GetGroupsForCapacityReservation (..)
    , mkGetGroupsForCapacityReservation
    -- ** Request lenses
    , ggfcrCapacityReservationId
    , ggfcrDryRun
    , ggfcrMaxResults
    , ggfcrNextToken

    -- * Destructuring the response
    , GetGroupsForCapacityReservationResponse (..)
    , mkGetGroupsForCapacityReservationResponse
    -- ** Response lenses
    , ggfcrrrsCapacityReservationGroups
    , ggfcrrrsNextToken
    , ggfcrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroupsForCapacityReservation' smart constructor.
data GetGroupsForCapacityReservation = GetGroupsForCapacityReservation'
  { capacityReservationId :: Types.CapacityReservationId
    -- ^ The ID of the Capacity Reservation.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupsForCapacityReservation' value with any optional fields omitted.
mkGetGroupsForCapacityReservation
    :: Types.CapacityReservationId -- ^ 'capacityReservationId'
    -> GetGroupsForCapacityReservation
mkGetGroupsForCapacityReservation capacityReservationId
  = GetGroupsForCapacityReservation'{capacityReservationId,
                                     dryRun = Core.Nothing, maxResults = Core.Nothing,
                                     nextToken = Core.Nothing}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrCapacityReservationId :: Lens.Lens' GetGroupsForCapacityReservation Types.CapacityReservationId
ggfcrCapacityReservationId = Lens.field @"capacityReservationId"
{-# INLINEABLE ggfcrCapacityReservationId #-}
{-# DEPRECATED capacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrDryRun :: Lens.Lens' GetGroupsForCapacityReservation (Core.Maybe Core.Bool)
ggfcrDryRun = Lens.field @"dryRun"
{-# INLINEABLE ggfcrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrMaxResults :: Lens.Lens' GetGroupsForCapacityReservation (Core.Maybe Core.Natural)
ggfcrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ggfcrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrNextToken :: Lens.Lens' GetGroupsForCapacityReservation (Core.Maybe Core.Text)
ggfcrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ggfcrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetGroupsForCapacityReservation where
        toQuery GetGroupsForCapacityReservation{..}
          = Core.toQueryPair "Action"
              ("GetGroupsForCapacityReservation" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "CapacityReservationId" capacityReservationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders GetGroupsForCapacityReservation where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetGroupsForCapacityReservation where
        type Rs GetGroupsForCapacityReservation =
             GetGroupsForCapacityReservationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetGroupsForCapacityReservationResponse' Core.<$>
                   (x Core..@? "capacityReservationGroupSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetGroupsForCapacityReservation where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"capacityReservationGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetGroupsForCapacityReservationResponse' smart constructor.
data GetGroupsForCapacityReservationResponse = GetGroupsForCapacityReservationResponse'
  { capacityReservationGroups :: Core.Maybe [Types.CapacityReservationGroup]
    -- ^ Information about the resource groups to which the Capacity Reservation has been added.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupsForCapacityReservationResponse' value with any optional fields omitted.
mkGetGroupsForCapacityReservationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetGroupsForCapacityReservationResponse
mkGetGroupsForCapacityReservationResponse responseStatus
  = GetGroupsForCapacityReservationResponse'{capacityReservationGroups
                                               = Core.Nothing,
                                             nextToken = Core.Nothing, responseStatus}

-- | Information about the resource groups to which the Capacity Reservation has been added.
--
-- /Note:/ Consider using 'capacityReservationGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrrrsCapacityReservationGroups :: Lens.Lens' GetGroupsForCapacityReservationResponse (Core.Maybe [Types.CapacityReservationGroup])
ggfcrrrsCapacityReservationGroups = Lens.field @"capacityReservationGroups"
{-# INLINEABLE ggfcrrrsCapacityReservationGroups #-}
{-# DEPRECATED capacityReservationGroups "Use generic-lens or generic-optics with 'capacityReservationGroups' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrrrsNextToken :: Lens.Lens' GetGroupsForCapacityReservationResponse (Core.Maybe Core.Text)
ggfcrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ggfcrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggfcrrrsResponseStatus :: Lens.Lens' GetGroupsForCapacityReservationResponse Core.Int
ggfcrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ggfcrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
