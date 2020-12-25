{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetFindings (..),
    mkGetFindings,

    -- ** Request lenses
    gfDetectorId,
    gfFindingIds,
    gfSortCriteria,

    -- * Destructuring the response
    GetFindingsResponse (..),
    mkGetFindingsResponse,

    -- ** Response lenses
    grsFindings,
    grsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFindings' smart constructor.
data GetFindings = GetFindings'
  { -- | The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
    detectorId :: Types.DetectorId,
    -- | The IDs of the findings that you want to retrieve.
    findingIds :: [Types.FindingId],
    -- | Represents the criteria used for sorting findings.
    sortCriteria :: Core.Maybe Types.SortCriteria
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFindings' value with any optional fields omitted.
mkGetFindings ::
  -- | 'detectorId'
  Types.DetectorId ->
  GetFindings
mkGetFindings detectorId =
  GetFindings'
    { detectorId,
      findingIds = Core.mempty,
      sortCriteria = Core.Nothing
    }

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfDetectorId :: Lens.Lens' GetFindings Types.DetectorId
gfDetectorId = Lens.field @"detectorId"
{-# DEPRECATED gfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The IDs of the findings that you want to retrieve.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFindingIds :: Lens.Lens' GetFindings [Types.FindingId]
gfFindingIds = Lens.field @"findingIds"
{-# DEPRECATED gfFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

-- | Represents the criteria used for sorting findings.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfSortCriteria :: Lens.Lens' GetFindings (Core.Maybe Types.SortCriteria)
gfSortCriteria = Lens.field @"sortCriteria"
{-# DEPRECATED gfSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

instance Core.FromJSON GetFindings where
  toJSON GetFindings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("findingIds" Core..= findingIds),
            ("sortCriteria" Core..=) Core.<$> sortCriteria
          ]
      )

instance Core.AWSRequest GetFindings where
  type Rs GetFindings = GetFindingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/findings/get")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingsResponse'
            Core.<$> (x Core..:? "findings" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFindingsResponse' smart constructor.
data GetFindingsResponse = GetFindingsResponse'
  { -- | A list of findings.
    findings :: [Types.Finding],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetFindingsResponse' value with any optional fields omitted.
mkGetFindingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFindingsResponse
mkGetFindingsResponse responseStatus =
  GetFindingsResponse' {findings = Core.mempty, responseStatus}

-- | A list of findings.
--
-- /Note:/ Consider using 'findings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsFindings :: Lens.Lens' GetFindingsResponse [Types.Finding]
grsFindings = Lens.field @"findings"
{-# DEPRECATED grsFindings "Use generic-lens or generic-optics with 'findings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetFindingsResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
