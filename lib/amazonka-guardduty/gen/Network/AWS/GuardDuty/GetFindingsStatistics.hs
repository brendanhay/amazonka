{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFindingsStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings statistics for the specified detector ID.
module Network.AWS.GuardDuty.GetFindingsStatistics
    (
    -- * Creating a request
      GetFindingsStatistics (..)
    , mkGetFindingsStatistics
    -- ** Request lenses
    , gfsDetectorId
    , gfsFindingStatisticTypes
    , gfsFindingCriteria

    -- * Destructuring the response
    , GetFindingsStatisticsResponse (..)
    , mkGetFindingsStatisticsResponse
    -- ** Response lenses
    , gfsrrsFindingStatistics
    , gfsrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFindingsStatistics' smart constructor.
data GetFindingsStatistics = GetFindingsStatistics'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
  , findingStatisticTypes :: [Types.FindingStatisticType]
    -- ^ The types of finding statistics to retrieve.
  , findingCriteria :: Core.Maybe Types.FindingCriteria
    -- ^ Represents the criteria that is used for querying findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFindingsStatistics' value with any optional fields omitted.
mkGetFindingsStatistics
    :: Types.DetectorId -- ^ 'detectorId'
    -> GetFindingsStatistics
mkGetFindingsStatistics detectorId
  = GetFindingsStatistics'{detectorId,
                           findingStatisticTypes = Core.mempty,
                           findingCriteria = Core.Nothing}

-- | The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsDetectorId :: Lens.Lens' GetFindingsStatistics Types.DetectorId
gfsDetectorId = Lens.field @"detectorId"
{-# INLINEABLE gfsDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The types of finding statistics to retrieve.
--
-- /Note:/ Consider using 'findingStatisticTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsFindingStatisticTypes :: Lens.Lens' GetFindingsStatistics [Types.FindingStatisticType]
gfsFindingStatisticTypes = Lens.field @"findingStatisticTypes"
{-# INLINEABLE gfsFindingStatisticTypes #-}
{-# DEPRECATED findingStatisticTypes "Use generic-lens or generic-optics with 'findingStatisticTypes' instead"  #-}

-- | Represents the criteria that is used for querying findings.
--
-- /Note:/ Consider using 'findingCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsFindingCriteria :: Lens.Lens' GetFindingsStatistics (Core.Maybe Types.FindingCriteria)
gfsFindingCriteria = Lens.field @"findingCriteria"
{-# INLINEABLE gfsFindingCriteria #-}
{-# DEPRECATED findingCriteria "Use generic-lens or generic-optics with 'findingCriteria' instead"  #-}

instance Core.ToQuery GetFindingsStatistics where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFindingsStatistics where
        toHeaders GetFindingsStatistics{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetFindingsStatistics where
        toJSON GetFindingsStatistics{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("findingStatisticTypes" Core..= findingStatisticTypes),
                  ("findingCriteria" Core..=) Core.<$> findingCriteria])

instance Core.AWSRequest GetFindingsStatistics where
        type Rs GetFindingsStatistics = GetFindingsStatisticsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/findings/statistics",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFindingsStatisticsResponse' Core.<$>
                   (x Core..: "findingStatistics") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFindingsStatisticsResponse' smart constructor.
data GetFindingsStatisticsResponse = GetFindingsStatisticsResponse'
  { findingStatistics :: Types.FindingStatistics
    -- ^ The finding statistics object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFindingsStatisticsResponse' value with any optional fields omitted.
mkGetFindingsStatisticsResponse
    :: Types.FindingStatistics -- ^ 'findingStatistics'
    -> Core.Int -- ^ 'responseStatus'
    -> GetFindingsStatisticsResponse
mkGetFindingsStatisticsResponse findingStatistics responseStatus
  = GetFindingsStatisticsResponse'{findingStatistics, responseStatus}

-- | The finding statistics object.
--
-- /Note:/ Consider using 'findingStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsFindingStatistics :: Lens.Lens' GetFindingsStatisticsResponse Types.FindingStatistics
gfsrrsFindingStatistics = Lens.field @"findingStatistics"
{-# INLINEABLE gfsrrsFindingStatistics #-}
{-# DEPRECATED findingStatistics "Use generic-lens or generic-optics with 'findingStatistics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfsrrsResponseStatus :: Lens.Lens' GetFindingsStatisticsResponse Core.Int
gfsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
