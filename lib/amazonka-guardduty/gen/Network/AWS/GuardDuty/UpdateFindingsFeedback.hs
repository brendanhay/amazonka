{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateFindingsFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks the specified GuardDuty findings as useful or not useful.
module Network.AWS.GuardDuty.UpdateFindingsFeedback
  ( -- * Creating a request
    UpdateFindingsFeedback (..),
    mkUpdateFindingsFeedback,

    -- ** Request lenses
    uffDetectorId,
    uffFindingIds,
    uffFeedback,
    uffComments,

    -- * Destructuring the response
    UpdateFindingsFeedbackResponse (..),
    mkUpdateFindingsFeedbackResponse,

    -- ** Response lenses
    uffrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFindingsFeedback' smart constructor.
data UpdateFindingsFeedback = UpdateFindingsFeedback'
  { -- | The ID of the detector associated with the findings to update feedback for.
    detectorId :: Types.DetectorId,
    -- | The IDs of the findings that you want to mark as useful or not useful.
    findingIds :: [Types.FindingId],
    -- | The feedback for the finding.
    feedback :: Types.Feedback,
    -- | Additional feedback about the GuardDuty findings.
    comments :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFindingsFeedback' value with any optional fields omitted.
mkUpdateFindingsFeedback ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'feedback'
  Types.Feedback ->
  UpdateFindingsFeedback
mkUpdateFindingsFeedback detectorId feedback =
  UpdateFindingsFeedback'
    { detectorId,
      findingIds = Core.mempty,
      feedback,
      comments = Core.Nothing
    }

-- | The ID of the detector associated with the findings to update feedback for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffDetectorId :: Lens.Lens' UpdateFindingsFeedback Types.DetectorId
uffDetectorId = Lens.field @"detectorId"
{-# DEPRECATED uffDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The IDs of the findings that you want to mark as useful or not useful.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffFindingIds :: Lens.Lens' UpdateFindingsFeedback [Types.FindingId]
uffFindingIds = Lens.field @"findingIds"
{-# DEPRECATED uffFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

-- | The feedback for the finding.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffFeedback :: Lens.Lens' UpdateFindingsFeedback Types.Feedback
uffFeedback = Lens.field @"feedback"
{-# DEPRECATED uffFeedback "Use generic-lens or generic-optics with 'feedback' instead." #-}

-- | Additional feedback about the GuardDuty findings.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffComments :: Lens.Lens' UpdateFindingsFeedback (Core.Maybe Types.String)
uffComments = Lens.field @"comments"
{-# DEPRECATED uffComments "Use generic-lens or generic-optics with 'comments' instead." #-}

instance Core.FromJSON UpdateFindingsFeedback where
  toJSON UpdateFindingsFeedback {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("findingIds" Core..= findingIds),
            Core.Just ("feedback" Core..= feedback),
            ("comments" Core..=) Core.<$> comments
          ]
      )

instance Core.AWSRequest UpdateFindingsFeedback where
  type Rs UpdateFindingsFeedback = UpdateFindingsFeedbackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/findings/feedback")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFindingsFeedbackResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateFindingsFeedbackResponse' smart constructor.
newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFindingsFeedbackResponse' value with any optional fields omitted.
mkUpdateFindingsFeedbackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateFindingsFeedbackResponse
mkUpdateFindingsFeedbackResponse responseStatus =
  UpdateFindingsFeedbackResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffrrsResponseStatus :: Lens.Lens' UpdateFindingsFeedbackResponse Core.Int
uffrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uffrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
