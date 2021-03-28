{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSmartHomeAppliances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the smart home appliances associated with a room.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSmartHomeAppliances
    (
    -- * Creating a request
      ListSmartHomeAppliances (..)
    , mkListSmartHomeAppliances
    -- ** Request lenses
    , lshaRoomArn
    , lshaMaxResults
    , lshaNextToken

    -- * Destructuring the response
    , ListSmartHomeAppliancesResponse (..)
    , mkListSmartHomeAppliancesResponse
    -- ** Response lenses
    , lsharrsNextToken
    , lsharrsSmartHomeAppliances
    , lsharrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSmartHomeAppliances' smart constructor.
data ListSmartHomeAppliances = ListSmartHomeAppliances'
  { roomArn :: Types.Arn
    -- ^ The room that the appliances are associated with.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of appliances to be returned, per paginated calls.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The tokens used for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSmartHomeAppliances' value with any optional fields omitted.
mkListSmartHomeAppliances
    :: Types.Arn -- ^ 'roomArn'
    -> ListSmartHomeAppliances
mkListSmartHomeAppliances roomArn
  = ListSmartHomeAppliances'{roomArn, maxResults = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The room that the appliances are associated with.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lshaRoomArn :: Lens.Lens' ListSmartHomeAppliances Types.Arn
lshaRoomArn = Lens.field @"roomArn"
{-# INLINEABLE lshaRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

-- | The maximum number of appliances to be returned, per paginated calls.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lshaMaxResults :: Lens.Lens' ListSmartHomeAppliances (Core.Maybe Core.Natural)
lshaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lshaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lshaNextToken :: Lens.Lens' ListSmartHomeAppliances (Core.Maybe Types.NextToken)
lshaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lshaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListSmartHomeAppliances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSmartHomeAppliances where
        toHeaders ListSmartHomeAppliances{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.ListSmartHomeAppliances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSmartHomeAppliances where
        toJSON ListSmartHomeAppliances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RoomArn" Core..= roomArn),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListSmartHomeAppliances where
        type Rs ListSmartHomeAppliances = ListSmartHomeAppliancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSmartHomeAppliancesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SmartHomeAppliances"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSmartHomeAppliances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"smartHomeAppliances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSmartHomeAppliancesResponse' smart constructor.
data ListSmartHomeAppliancesResponse = ListSmartHomeAppliancesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The tokens used for pagination.
  , smartHomeAppliances :: Core.Maybe [Types.SmartHomeAppliance]
    -- ^ The smart home appliances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSmartHomeAppliancesResponse' value with any optional fields omitted.
mkListSmartHomeAppliancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSmartHomeAppliancesResponse
mkListSmartHomeAppliancesResponse responseStatus
  = ListSmartHomeAppliancesResponse'{nextToken = Core.Nothing,
                                     smartHomeAppliances = Core.Nothing, responseStatus}

-- | The tokens used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsharrsNextToken :: Lens.Lens' ListSmartHomeAppliancesResponse (Core.Maybe Types.NextToken)
lsharrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsharrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The smart home appliances.
--
-- /Note:/ Consider using 'smartHomeAppliances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsharrsSmartHomeAppliances :: Lens.Lens' ListSmartHomeAppliancesResponse (Core.Maybe [Types.SmartHomeAppliance])
lsharrsSmartHomeAppliances = Lens.field @"smartHomeAppliances"
{-# INLINEABLE lsharrsSmartHomeAppliances #-}
{-# DEPRECATED smartHomeAppliances "Use generic-lens or generic-optics with 'smartHomeAppliances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsharrsResponseStatus :: Lens.Lens' ListSmartHomeAppliancesResponse Core.Int
lsharrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsharrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
