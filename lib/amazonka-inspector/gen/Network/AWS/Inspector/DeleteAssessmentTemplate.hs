{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DeleteAssessmentTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment template that is specified by the ARN of the assessment template.
module Network.AWS.Inspector.DeleteAssessmentTemplate
  ( -- * Creating a request
    DeleteAssessmentTemplate (..),
    mkDeleteAssessmentTemplate,

    -- ** Request lenses
    datAssessmentTemplateArn,

    -- * Destructuring the response
    DeleteAssessmentTemplateResponse (..),
    mkDeleteAssessmentTemplateResponse,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAssessmentTemplate' smart constructor.
newtype DeleteAssessmentTemplate = DeleteAssessmentTemplate'
  { -- | The ARN that specifies the assessment template that you want to delete.
    assessmentTemplateArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentTemplate' value with any optional fields omitted.
mkDeleteAssessmentTemplate ::
  -- | 'assessmentTemplateArn'
  Types.Arn ->
  DeleteAssessmentTemplate
mkDeleteAssessmentTemplate assessmentTemplateArn =
  DeleteAssessmentTemplate' {assessmentTemplateArn}

-- | The ARN that specifies the assessment template that you want to delete.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTemplateArn :: Lens.Lens' DeleteAssessmentTemplate Types.Arn
datAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# DEPRECATED datAssessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead." #-}

instance Core.FromJSON DeleteAssessmentTemplate where
  toJSON DeleteAssessmentTemplate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("assessmentTemplateArn" Core..= assessmentTemplateArn)
          ]
      )

instance Core.AWSRequest DeleteAssessmentTemplate where
  type Rs DeleteAssessmentTemplate = DeleteAssessmentTemplateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.DeleteAssessmentTemplate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteAssessmentTemplateResponse'

-- | /See:/ 'mkDeleteAssessmentTemplateResponse' smart constructor.
data DeleteAssessmentTemplateResponse = DeleteAssessmentTemplateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentTemplateResponse' value with any optional fields omitted.
mkDeleteAssessmentTemplateResponse ::
  DeleteAssessmentTemplateResponse
mkDeleteAssessmentTemplateResponse =
  DeleteAssessmentTemplateResponse'
