{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.UpdateAssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the assessment target that is specified by the ARN of the assessment target.
--
-- If resourceGroupArn is not specified, all EC2 instances in the current AWS account and region are included in the assessment target.
module Network.AWS.Inspector.UpdateAssessmentTarget
  ( -- * Creating a request
    UpdateAssessmentTarget (..),
    mkUpdateAssessmentTarget,

    -- ** Request lenses
    uatAssessmentTargetArn,
    uatAssessmentTargetName,
    uatResourceGroupArn,

    -- * Destructuring the response
    UpdateAssessmentTargetResponse (..),
    mkUpdateAssessmentTargetResponse,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateAssessmentTarget' smart constructor.
data UpdateAssessmentTarget = UpdateAssessmentTarget'
  { -- | The ARN of the assessment target that you want to update.
    assessmentTargetArn :: Types.Arn,
    -- | The name of the assessment target that you want to update.
    assessmentTargetName :: Types.AssessmentTargetName,
    -- | The ARN of the resource group that is used to specify the new resource group to associate with the assessment target.
    resourceGroupArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAssessmentTarget' value with any optional fields omitted.
mkUpdateAssessmentTarget ::
  -- | 'assessmentTargetArn'
  Types.Arn ->
  -- | 'assessmentTargetName'
  Types.AssessmentTargetName ->
  UpdateAssessmentTarget
mkUpdateAssessmentTarget assessmentTargetArn assessmentTargetName =
  UpdateAssessmentTarget'
    { assessmentTargetArn,
      assessmentTargetName,
      resourceGroupArn = Core.Nothing
    }

-- | The ARN of the assessment target that you want to update.
--
-- /Note:/ Consider using 'assessmentTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatAssessmentTargetArn :: Lens.Lens' UpdateAssessmentTarget Types.Arn
uatAssessmentTargetArn = Lens.field @"assessmentTargetArn"
{-# DEPRECATED uatAssessmentTargetArn "Use generic-lens or generic-optics with 'assessmentTargetArn' instead." #-}

-- | The name of the assessment target that you want to update.
--
-- /Note:/ Consider using 'assessmentTargetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatAssessmentTargetName :: Lens.Lens' UpdateAssessmentTarget Types.AssessmentTargetName
uatAssessmentTargetName = Lens.field @"assessmentTargetName"
{-# DEPRECATED uatAssessmentTargetName "Use generic-lens or generic-optics with 'assessmentTargetName' instead." #-}

-- | The ARN of the resource group that is used to specify the new resource group to associate with the assessment target.
--
-- /Note:/ Consider using 'resourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uatResourceGroupArn :: Lens.Lens' UpdateAssessmentTarget (Core.Maybe Types.Arn)
uatResourceGroupArn = Lens.field @"resourceGroupArn"
{-# DEPRECATED uatResourceGroupArn "Use generic-lens or generic-optics with 'resourceGroupArn' instead." #-}

instance Core.FromJSON UpdateAssessmentTarget where
  toJSON UpdateAssessmentTarget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("assessmentTargetArn" Core..= assessmentTargetArn),
            Core.Just ("assessmentTargetName" Core..= assessmentTargetName),
            ("resourceGroupArn" Core..=) Core.<$> resourceGroupArn
          ]
      )

instance Core.AWSRequest UpdateAssessmentTarget where
  type Rs UpdateAssessmentTarget = UpdateAssessmentTargetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.UpdateAssessmentTarget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateAssessmentTargetResponse'

-- | /See:/ 'mkUpdateAssessmentTargetResponse' smart constructor.
data UpdateAssessmentTargetResponse = UpdateAssessmentTargetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAssessmentTargetResponse' value with any optional fields omitted.
mkUpdateAssessmentTargetResponse ::
  UpdateAssessmentTargetResponse
mkUpdateAssessmentTargetResponse = UpdateAssessmentTargetResponse'
