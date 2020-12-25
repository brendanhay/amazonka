{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UnarchiveFindings (..),
    mkUnarchiveFindings,

    -- ** Request lenses
    uDetectorId,
    uFindingIds,

    -- * Destructuring the response
    UnarchiveFindingsResponse (..),
    mkUnarchiveFindingsResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnarchiveFindings' smart constructor.
data UnarchiveFindings = UnarchiveFindings'
  { -- | The ID of the detector associated with the findings to unarchive.
    detectorId :: Types.DetectorId,
    -- | The IDs of the findings to unarchive.
    findingIds :: [Types.FindingId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnarchiveFindings' value with any optional fields omitted.
mkUnarchiveFindings ::
  -- | 'detectorId'
  Types.DetectorId ->
  UnarchiveFindings
mkUnarchiveFindings detectorId =
  UnarchiveFindings' {detectorId, findingIds = Core.mempty}

-- | The ID of the detector associated with the findings to unarchive.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDetectorId :: Lens.Lens' UnarchiveFindings Types.DetectorId
uDetectorId = Lens.field @"detectorId"
{-# DEPRECATED uDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The IDs of the findings to unarchive.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uFindingIds :: Lens.Lens' UnarchiveFindings [Types.FindingId]
uFindingIds = Lens.field @"findingIds"
{-# DEPRECATED uFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

instance Core.FromJSON UnarchiveFindings where
  toJSON UnarchiveFindings {..} =
    Core.object
      (Core.catMaybes [Core.Just ("findingIds" Core..= findingIds)])

instance Core.AWSRequest UnarchiveFindings where
  type Rs UnarchiveFindings = UnarchiveFindingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/findings/unarchive")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UnarchiveFindingsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUnarchiveFindingsResponse' smart constructor.
newtype UnarchiveFindingsResponse = UnarchiveFindingsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UnarchiveFindingsResponse' value with any optional fields omitted.
mkUnarchiveFindingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UnarchiveFindingsResponse
mkUnarchiveFindingsResponse responseStatus =
  UnarchiveFindingsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UnarchiveFindingsResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
