{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.StartAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the assessment run specified by the ARN of the assessment template. For this API to function properly, you must not exceed the limit of running up to 500 concurrent agents per AWS account.
module Network.AWS.Inspector.StartAssessmentRun
  ( -- * Creating a request
    StartAssessmentRun (..),
    mkStartAssessmentRun,

    -- ** Request lenses
    sarAssessmentTemplateArn,
    sarAssessmentRunName,

    -- * Destructuring the response
    StartAssessmentRunResponse (..),
    mkStartAssessmentRunResponse,

    -- ** Response lenses
    sarrrsAssessmentRunArn,
    sarrrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartAssessmentRun' smart constructor.
data StartAssessmentRun = StartAssessmentRun'
  { -- | The ARN of the assessment template of the assessment run that you want to start.
    assessmentTemplateArn :: Types.Arn,
    -- | You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
    assessmentRunName :: Core.Maybe Types.AssessmentRunName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAssessmentRun' value with any optional fields omitted.
mkStartAssessmentRun ::
  -- | 'assessmentTemplateArn'
  Types.Arn ->
  StartAssessmentRun
mkStartAssessmentRun assessmentTemplateArn =
  StartAssessmentRun'
    { assessmentTemplateArn,
      assessmentRunName = Core.Nothing
    }

-- | The ARN of the assessment template of the assessment run that you want to start.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentTemplateArn :: Lens.Lens' StartAssessmentRun Types.Arn
sarAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# DEPRECATED sarAssessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead." #-}

-- | You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
--
-- /Note:/ Consider using 'assessmentRunName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentRunName :: Lens.Lens' StartAssessmentRun (Core.Maybe Types.AssessmentRunName)
sarAssessmentRunName = Lens.field @"assessmentRunName"
{-# DEPRECATED sarAssessmentRunName "Use generic-lens or generic-optics with 'assessmentRunName' instead." #-}

instance Core.FromJSON StartAssessmentRun where
  toJSON StartAssessmentRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("assessmentTemplateArn" Core..= assessmentTemplateArn),
            ("assessmentRunName" Core..=) Core.<$> assessmentRunName
          ]
      )

instance Core.AWSRequest StartAssessmentRun where
  type Rs StartAssessmentRun = StartAssessmentRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.StartAssessmentRun")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAssessmentRunResponse'
            Core.<$> (x Core..: "assessmentRunArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartAssessmentRunResponse' smart constructor.
data StartAssessmentRunResponse = StartAssessmentRunResponse'
  { -- | The ARN of the assessment run that has been started.
    assessmentRunArn :: Types.AssessmentRunArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAssessmentRunResponse' value with any optional fields omitted.
mkStartAssessmentRunResponse ::
  -- | 'assessmentRunArn'
  Types.AssessmentRunArn ->
  -- | 'responseStatus'
  Core.Int ->
  StartAssessmentRunResponse
mkStartAssessmentRunResponse assessmentRunArn responseStatus =
  StartAssessmentRunResponse' {assessmentRunArn, responseStatus}

-- | The ARN of the assessment run that has been started.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrrsAssessmentRunArn :: Lens.Lens' StartAssessmentRunResponse Types.AssessmentRunArn
sarrrsAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# DEPRECATED sarrrsAssessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrrsResponseStatus :: Lens.Lens' StartAssessmentRunResponse Core.Int
sarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
