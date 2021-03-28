{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateFindingsFeedback (..)
    , mkUpdateFindingsFeedback
    -- ** Request lenses
    , uffDetectorId
    , uffFindingIds
    , uffFeedback
    , uffComments

    -- * Destructuring the response
    , UpdateFindingsFeedbackResponse (..)
    , mkUpdateFindingsFeedbackResponse
    -- ** Response lenses
    , uffrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFindingsFeedback' smart constructor.
data UpdateFindingsFeedback = UpdateFindingsFeedback'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector associated with the findings to update feedback for.
  , findingIds :: [Types.FindingId]
    -- ^ The IDs of the findings that you want to mark as useful or not useful.
  , feedback :: Types.Feedback
    -- ^ The feedback for the finding.
  , comments :: Core.Maybe Core.Text
    -- ^ Additional feedback about the GuardDuty findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFindingsFeedback' value with any optional fields omitted.
mkUpdateFindingsFeedback
    :: Types.DetectorId -- ^ 'detectorId'
    -> Types.Feedback -- ^ 'feedback'
    -> UpdateFindingsFeedback
mkUpdateFindingsFeedback detectorId feedback
  = UpdateFindingsFeedback'{detectorId, findingIds = Core.mempty,
                            feedback, comments = Core.Nothing}

-- | The ID of the detector associated with the findings to update feedback for.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffDetectorId :: Lens.Lens' UpdateFindingsFeedback Types.DetectorId
uffDetectorId = Lens.field @"detectorId"
{-# INLINEABLE uffDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The IDs of the findings that you want to mark as useful or not useful.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffFindingIds :: Lens.Lens' UpdateFindingsFeedback [Types.FindingId]
uffFindingIds = Lens.field @"findingIds"
{-# INLINEABLE uffFindingIds #-}
{-# DEPRECATED findingIds "Use generic-lens or generic-optics with 'findingIds' instead"  #-}

-- | The feedback for the finding.
--
-- /Note:/ Consider using 'feedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffFeedback :: Lens.Lens' UpdateFindingsFeedback Types.Feedback
uffFeedback = Lens.field @"feedback"
{-# INLINEABLE uffFeedback #-}
{-# DEPRECATED feedback "Use generic-lens or generic-optics with 'feedback' instead"  #-}

-- | Additional feedback about the GuardDuty findings.
--
-- /Note:/ Consider using 'comments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffComments :: Lens.Lens' UpdateFindingsFeedback (Core.Maybe Core.Text)
uffComments = Lens.field @"comments"
{-# INLINEABLE uffComments #-}
{-# DEPRECATED comments "Use generic-lens or generic-optics with 'comments' instead"  #-}

instance Core.ToQuery UpdateFindingsFeedback where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFindingsFeedback where
        toHeaders UpdateFindingsFeedback{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateFindingsFeedback where
        toJSON UpdateFindingsFeedback{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("findingIds" Core..= findingIds),
                  Core.Just ("feedback" Core..= feedback),
                  ("comments" Core..=) Core.<$> comments])

instance Core.AWSRequest UpdateFindingsFeedback where
        type Rs UpdateFindingsFeedback = UpdateFindingsFeedbackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/findings/feedback",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateFindingsFeedbackResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFindingsFeedbackResponse' smart constructor.
newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFindingsFeedbackResponse' value with any optional fields omitted.
mkUpdateFindingsFeedbackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateFindingsFeedbackResponse
mkUpdateFindingsFeedbackResponse responseStatus
  = UpdateFindingsFeedbackResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uffrrsResponseStatus :: Lens.Lens' UpdateFindingsFeedbackResponse Core.Int
uffrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uffrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
