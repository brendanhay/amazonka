{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon GuardDuty findings specified by finding IDs.
module Network.AWS.GuardDuty.GetFindings
    (
    -- * Creating a request
      GetFindings (..)
    , mkGetFindings
    -- ** Request lenses
    , gfDetectorId
    , gfFindingIds
    , gfSortCriteria

    -- * Destructuring the response
    , GetFindingsResponse (..)
    , mkGetFindingsResponse
    -- ** Response lenses
    , grsFindings
    , grsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFindings' smart constructor.
data GetFindings = GetFindings'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
  , findingIds :: [Types.FindingId]
    -- ^ The IDs of the findings that you want to retrieve.
  , sortCriteria :: Core.Maybe Types.SortCriteria
    -- ^ Represents the criteria used for sorting findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFindings' value with any optional fields omitted.
mkGetFindings
    :: Types.DetectorId -- ^ 'detectorId'
    -> GetFindings
mkGetFindings detectorId
  = GetFindings'{detectorId, findingIds = Core.mempty,
                 sortCriteria = Core.Nothing}

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfDetectorId :: Lens.Lens' GetFindings Types.DetectorId
gfDetectorId = Lens.field @"detectorId"
{-# INLINEABLE gfDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The IDs of the findings that you want to retrieve.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFindingIds :: Lens.Lens' GetFindings [Types.FindingId]
gfFindingIds = Lens.field @"findingIds"
{-# INLINEABLE gfFindingIds #-}
{-# DEPRECATED findingIds "Use generic-lens or generic-optics with 'findingIds' instead"  #-}

-- | Represents the criteria used for sorting findings.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfSortCriteria :: Lens.Lens' GetFindings (Core.Maybe Types.SortCriteria)
gfSortCriteria = Lens.field @"sortCriteria"
{-# INLINEABLE gfSortCriteria #-}
{-# DEPRECATED sortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead"  #-}

instance Core.ToQuery GetFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFindings where
        toHeaders GetFindings{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetFindings where
        toJSON GetFindings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("findingIds" Core..= findingIds),
                  ("sortCriteria" Core..=) Core.<$> sortCriteria])

instance Core.AWSRequest GetFindings where
        type Rs GetFindings = GetFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/findings/get",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFindingsResponse' Core.<$>
                   (x Core..:? "findings" Core..!= Core.mempty) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFindingsResponse' smart constructor.
data GetFindingsResponse = GetFindingsResponse'
  { findings :: [Types.Finding]
    -- ^ A list of findings.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetFindingsResponse' value with any optional fields omitted.
mkGetFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFindingsResponse
mkGetFindingsResponse responseStatus
  = GetFindingsResponse'{findings = Core.mempty, responseStatus}

-- | A list of findings.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsFindings :: Lens.Lens' GetFindingsResponse [Types.Finding]
grsFindings = Lens.field @"findings"
{-# INLINEABLE grsFindings #-}
{-# DEPRECATED findings "Use generic-lens or generic-optics with 'findings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetFindingsResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
