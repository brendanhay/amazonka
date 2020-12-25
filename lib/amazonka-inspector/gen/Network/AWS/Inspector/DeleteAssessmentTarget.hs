{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DeleteAssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment target that is specified by the ARN of the assessment target.
module Network.AWS.Inspector.DeleteAssessmentTarget
  ( -- * Creating a request
    DeleteAssessmentTarget (..),
    mkDeleteAssessmentTarget,

    -- ** Request lenses
    datAssessmentTargetArn,

    -- * Destructuring the response
    DeleteAssessmentTargetResponse (..),
    mkDeleteAssessmentTargetResponse,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAssessmentTarget' smart constructor.
newtype DeleteAssessmentTarget = DeleteAssessmentTarget'
  { -- | The ARN that specifies the assessment target that you want to delete.
    assessmentTargetArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentTarget' value with any optional fields omitted.
mkDeleteAssessmentTarget ::
  -- | 'assessmentTargetArn'
  Types.Arn ->
  DeleteAssessmentTarget
mkDeleteAssessmentTarget assessmentTargetArn =
  DeleteAssessmentTarget' {assessmentTargetArn}

-- | The ARN that specifies the assessment target that you want to delete.
--
-- /Note:/ Consider using 'assessmentTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTargetArn :: Lens.Lens' DeleteAssessmentTarget Types.Arn
datAssessmentTargetArn = Lens.field @"assessmentTargetArn"
{-# DEPRECATED datAssessmentTargetArn "Use generic-lens or generic-optics with 'assessmentTargetArn' instead." #-}

instance Core.FromJSON DeleteAssessmentTarget where
  toJSON DeleteAssessmentTarget {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("assessmentTargetArn" Core..= assessmentTargetArn)]
      )

instance Core.AWSRequest DeleteAssessmentTarget where
  type Rs DeleteAssessmentTarget = DeleteAssessmentTargetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.DeleteAssessmentTarget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteAssessmentTargetResponse'

-- | /See:/ 'mkDeleteAssessmentTargetResponse' smart constructor.
data DeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentTargetResponse' value with any optional fields omitted.
mkDeleteAssessmentTargetResponse ::
  DeleteAssessmentTargetResponse
mkDeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
