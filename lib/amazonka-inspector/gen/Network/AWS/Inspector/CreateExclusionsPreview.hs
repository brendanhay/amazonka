{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.CreateExclusionsPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the generation of an exclusions preview for the specified assessment template. The exclusions preview lists the potential exclusions (ExclusionPreview) that Inspector can detect before it runs the assessment.
module Network.AWS.Inspector.CreateExclusionsPreview
  ( -- * Creating a request
    CreateExclusionsPreview (..),
    mkCreateExclusionsPreview,

    -- ** Request lenses
    cepAssessmentTemplateArn,

    -- * Destructuring the response
    CreateExclusionsPreviewResponse (..),
    mkCreateExclusionsPreviewResponse,

    -- ** Response lenses
    ceprrsPreviewToken,
    ceprrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateExclusionsPreview' smart constructor.
newtype CreateExclusionsPreview = CreateExclusionsPreview'
  { -- | The ARN that specifies the assessment template for which you want to create an exclusions preview.
    assessmentTemplateArn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExclusionsPreview' value with any optional fields omitted.
mkCreateExclusionsPreview ::
  -- | 'assessmentTemplateArn'
  Types.Arn ->
  CreateExclusionsPreview
mkCreateExclusionsPreview assessmentTemplateArn =
  CreateExclusionsPreview' {assessmentTemplateArn}

-- | The ARN that specifies the assessment template for which you want to create an exclusions preview.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cepAssessmentTemplateArn :: Lens.Lens' CreateExclusionsPreview Types.Arn
cepAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# DEPRECATED cepAssessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead." #-}

instance Core.FromJSON CreateExclusionsPreview where
  toJSON CreateExclusionsPreview {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("assessmentTemplateArn" Core..= assessmentTemplateArn)
          ]
      )

instance Core.AWSRequest CreateExclusionsPreview where
  type Rs CreateExclusionsPreview = CreateExclusionsPreviewResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.CreateExclusionsPreview")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExclusionsPreviewResponse'
            Core.<$> (x Core..: "previewToken") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateExclusionsPreviewResponse' smart constructor.
data CreateExclusionsPreviewResponse = CreateExclusionsPreviewResponse'
  { -- | Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
    previewToken :: Types.PreviewToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExclusionsPreviewResponse' value with any optional fields omitted.
mkCreateExclusionsPreviewResponse ::
  -- | 'previewToken'
  Types.PreviewToken ->
  -- | 'responseStatus'
  Core.Int ->
  CreateExclusionsPreviewResponse
mkCreateExclusionsPreviewResponse previewToken responseStatus =
  CreateExclusionsPreviewResponse' {previewToken, responseStatus}

-- | Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
--
-- /Note:/ Consider using 'previewToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceprrsPreviewToken :: Lens.Lens' CreateExclusionsPreviewResponse Types.PreviewToken
ceprrsPreviewToken = Lens.field @"previewToken"
{-# DEPRECATED ceprrsPreviewToken "Use generic-lens or generic-optics with 'previewToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceprrsResponseStatus :: Lens.Lens' CreateExclusionsPreviewResponse Core.Int
ceprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ceprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
