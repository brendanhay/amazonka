{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateExclusionsPreview (..)
    , mkCreateExclusionsPreview
    -- ** Request lenses
    , cepAssessmentTemplateArn

    -- * Destructuring the response
    , CreateExclusionsPreviewResponse (..)
    , mkCreateExclusionsPreviewResponse
    -- ** Response lenses
    , ceprrsPreviewToken
    , ceprrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateExclusionsPreview' smart constructor.
newtype CreateExclusionsPreview = CreateExclusionsPreview'
  { assessmentTemplateArn :: Types.Arn
    -- ^ The ARN that specifies the assessment template for which you want to create an exclusions preview.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExclusionsPreview' value with any optional fields omitted.
mkCreateExclusionsPreview
    :: Types.Arn -- ^ 'assessmentTemplateArn'
    -> CreateExclusionsPreview
mkCreateExclusionsPreview assessmentTemplateArn
  = CreateExclusionsPreview'{assessmentTemplateArn}

-- | The ARN that specifies the assessment template for which you want to create an exclusions preview.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cepAssessmentTemplateArn :: Lens.Lens' CreateExclusionsPreview Types.Arn
cepAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# INLINEABLE cepAssessmentTemplateArn #-}
{-# DEPRECATED assessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead"  #-}

instance Core.ToQuery CreateExclusionsPreview where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateExclusionsPreview where
        toHeaders CreateExclusionsPreview{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.CreateExclusionsPreview")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateExclusionsPreview where
        toJSON CreateExclusionsPreview{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("assessmentTemplateArn" Core..= assessmentTemplateArn)])

instance Core.AWSRequest CreateExclusionsPreview where
        type Rs CreateExclusionsPreview = CreateExclusionsPreviewResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateExclusionsPreviewResponse' Core.<$>
                   (x Core..: "previewToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateExclusionsPreviewResponse' smart constructor.
data CreateExclusionsPreviewResponse = CreateExclusionsPreviewResponse'
  { previewToken :: Types.PreviewToken
    -- ^ Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExclusionsPreviewResponse' value with any optional fields omitted.
mkCreateExclusionsPreviewResponse
    :: Types.PreviewToken -- ^ 'previewToken'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateExclusionsPreviewResponse
mkCreateExclusionsPreviewResponse previewToken responseStatus
  = CreateExclusionsPreviewResponse'{previewToken, responseStatus}

-- | Specifies the unique identifier of the requested exclusions preview. You can use the unique identifier to retrieve the exclusions preview when running the GetExclusionsPreview API.
--
-- /Note:/ Consider using 'previewToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceprrsPreviewToken :: Lens.Lens' CreateExclusionsPreviewResponse Types.PreviewToken
ceprrsPreviewToken = Lens.field @"previewToken"
{-# INLINEABLE ceprrsPreviewToken #-}
{-# DEPRECATED previewToken "Use generic-lens or generic-optics with 'previewToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceprrsResponseStatus :: Lens.Lens' CreateExclusionsPreviewResponse Core.Int
ceprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ceprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
