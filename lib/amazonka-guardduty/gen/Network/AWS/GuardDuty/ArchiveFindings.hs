{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ArchiveFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Archives GuardDuty findings that are specified by the list of finding IDs.
module Network.AWS.GuardDuty.ArchiveFindings
    (
    -- * Creating a request
      ArchiveFindings (..)
    , mkArchiveFindings
    -- ** Request lenses
    , afDetectorId
    , afFindingIds

    -- * Destructuring the response
    , ArchiveFindingsResponse (..)
    , mkArchiveFindingsResponse
    -- ** Response lenses
    , afrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkArchiveFindings' smart constructor.
data ArchiveFindings = ArchiveFindings'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector that specifies the GuardDuty service whose findings you want to archive.
  , findingIds :: [Types.FindingId]
    -- ^ The IDs of the findings that you want to archive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ArchiveFindings' value with any optional fields omitted.
mkArchiveFindings
    :: Types.DetectorId -- ^ 'detectorId'
    -> ArchiveFindings
mkArchiveFindings detectorId
  = ArchiveFindings'{detectorId, findingIds = Core.mempty}

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to archive.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afDetectorId :: Lens.Lens' ArchiveFindings Types.DetectorId
afDetectorId = Lens.field @"detectorId"
{-# INLINEABLE afDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The IDs of the findings that you want to archive.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFindingIds :: Lens.Lens' ArchiveFindings [Types.FindingId]
afFindingIds = Lens.field @"findingIds"
{-# INLINEABLE afFindingIds #-}
{-# DEPRECATED findingIds "Use generic-lens or generic-optics with 'findingIds' instead"  #-}

instance Core.ToQuery ArchiveFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ArchiveFindings where
        toHeaders ArchiveFindings{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ArchiveFindings where
        toJSON ArchiveFindings{..}
          = Core.object
              (Core.catMaybes [Core.Just ("findingIds" Core..= findingIds)])

instance Core.AWSRequest ArchiveFindings where
        type Rs ArchiveFindings = ArchiveFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/findings/archive",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ArchiveFindingsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkArchiveFindingsResponse' smart constructor.
newtype ArchiveFindingsResponse = ArchiveFindingsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ArchiveFindingsResponse' value with any optional fields omitted.
mkArchiveFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ArchiveFindingsResponse
mkArchiveFindingsResponse responseStatus
  = ArchiveFindingsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afrrsResponseStatus :: Lens.Lens' ArchiveFindingsResponse Core.Int
afrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE afrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
