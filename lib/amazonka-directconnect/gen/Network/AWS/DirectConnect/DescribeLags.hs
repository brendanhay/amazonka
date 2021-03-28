{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all your link aggregation groups (LAG) or the specified LAG.
module Network.AWS.DirectConnect.DescribeLags
    (
    -- * Creating a request
      DescribeLags (..)
    , mkDescribeLags
    -- ** Request lenses
    , dlLagId

    -- * Destructuring the response
    , DescribeLagsResponse (..)
    , mkDescribeLagsResponse
    -- ** Response lenses
    , dlrfrsLags
    , dlrfrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLags' smart constructor.
newtype DescribeLags = DescribeLags'
  { lagId :: Core.Maybe Types.LagId
    -- ^ The ID of the LAG.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLags' value with any optional fields omitted.
mkDescribeLags
    :: DescribeLags
mkDescribeLags = DescribeLags'{lagId = Core.Nothing}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLagId :: Lens.Lens' DescribeLags (Core.Maybe Types.LagId)
dlLagId = Lens.field @"lagId"
{-# INLINEABLE dlLagId #-}
{-# DEPRECATED lagId "Use generic-lens or generic-optics with 'lagId' instead"  #-}

instance Core.ToQuery DescribeLags where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeLags where
        toHeaders DescribeLags{..}
          = Core.pure ("X-Amz-Target", "OvertureService.DescribeLags")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeLags where
        toJSON DescribeLags{..}
          = Core.object (Core.catMaybes [("lagId" Core..=) Core.<$> lagId])

instance Core.AWSRequest DescribeLags where
        type Rs DescribeLags = DescribeLagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeLagsResponse' Core.<$>
                   (x Core..:? "lags") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeLagsResponse' smart constructor.
data DescribeLagsResponse = DescribeLagsResponse'
  { lags :: Core.Maybe [Types.Lag]
    -- ^ The LAGs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeLagsResponse' value with any optional fields omitted.
mkDescribeLagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLagsResponse
mkDescribeLagsResponse responseStatus
  = DescribeLagsResponse'{lags = Core.Nothing, responseStatus}

-- | The LAGs.
--
-- /Note:/ Consider using 'lags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrfrsLags :: Lens.Lens' DescribeLagsResponse (Core.Maybe [Types.Lag])
dlrfrsLags = Lens.field @"lags"
{-# INLINEABLE dlrfrsLags #-}
{-# DEPRECATED lags "Use generic-lens or generic-optics with 'lags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrfrsResponseStatus :: Lens.Lens' DescribeLagsResponse Core.Int
dlrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
