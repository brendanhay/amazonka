{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UnarchiveFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unarchives GuardDuty findings specified by the @findingIds@ .
module Network.AWS.GuardDuty.UnarchiveFindings
    (
    -- * Creating a request
      UnarchiveFindings (..)
    , mkUnarchiveFindings
    -- ** Request lenses
    , uDetectorId
    , uFindingIds

    -- * Destructuring the response
    , UnarchiveFindingsResponse (..)
    , mkUnarchiveFindingsResponse
    -- ** Response lenses
    , ursResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnarchiveFindings' smart constructor.
data UnarchiveFindings = UnarchiveFindings'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector associated with the findings to unarchive.
  , findingIds :: [Types.FindingId]
    -- ^ The IDs of the findings to unarchive.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnarchiveFindings' value with any optional fields omitted.
mkUnarchiveFindings
    :: Types.DetectorId -- ^ 'detectorId'
    -> UnarchiveFindings
mkUnarchiveFindings detectorId
  = UnarchiveFindings'{detectorId, findingIds = Core.mempty}

-- | The ID of the detector associated with the findings to unarchive.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDetectorId :: Lens.Lens' UnarchiveFindings Types.DetectorId
uDetectorId = Lens.field @"detectorId"
{-# INLINEABLE uDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The IDs of the findings to unarchive.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uFindingIds :: Lens.Lens' UnarchiveFindings [Types.FindingId]
uFindingIds = Lens.field @"findingIds"
{-# INLINEABLE uFindingIds #-}
{-# DEPRECATED findingIds "Use generic-lens or generic-optics with 'findingIds' instead"  #-}

instance Core.ToQuery UnarchiveFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UnarchiveFindings where
        toHeaders UnarchiveFindings{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UnarchiveFindings where
        toJSON UnarchiveFindings{..}
          = Core.object
              (Core.catMaybes [Core.Just ("findingIds" Core..= findingIds)])

instance Core.AWSRequest UnarchiveFindings where
        type Rs UnarchiveFindings = UnarchiveFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/findings/unarchive",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UnarchiveFindingsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUnarchiveFindingsResponse' smart constructor.
newtype UnarchiveFindingsResponse = UnarchiveFindingsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UnarchiveFindingsResponse' value with any optional fields omitted.
mkUnarchiveFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UnarchiveFindingsResponse
mkUnarchiveFindingsResponse responseStatus
  = UnarchiveFindingsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UnarchiveFindingsResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ursResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
