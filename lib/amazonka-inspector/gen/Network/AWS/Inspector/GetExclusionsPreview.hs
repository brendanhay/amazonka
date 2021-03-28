{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.GetExclusionsPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the exclusions preview (a list of ExclusionPreview objects) specified by the preview token. You can obtain the preview token by running the CreateExclusionsPreview API.
module Network.AWS.Inspector.GetExclusionsPreview
    (
    -- * Creating a request
      GetExclusionsPreview (..)
    , mkGetExclusionsPreview
    -- ** Request lenses
    , gepAssessmentTemplateArn
    , gepPreviewToken
    , gepLocale
    , gepMaxResults
    , gepNextToken

    -- * Destructuring the response
    , GetExclusionsPreviewResponse (..)
    , mkGetExclusionsPreviewResponse
    -- ** Response lenses
    , geprrsPreviewStatus
    , geprrsExclusionPreviews
    , geprrsNextToken
    , geprrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetExclusionsPreview' smart constructor.
data GetExclusionsPreview = GetExclusionsPreview'
  { assessmentTemplateArn :: Types.AssessmentTemplateArn
    -- ^ The ARN that specifies the assessment template for which the exclusions preview was requested.
  , previewToken :: Types.PreviewToken
    -- ^ The unique identifier associated of the exclusions preview.
  , locale :: Core.Maybe Types.Locale
    -- ^ The locale into which you want to translate the exclusion's title, description, and recommendation.
  , maxResults :: Core.Maybe Core.Int
    -- ^ You can use this parameter to indicate the maximum number of items you want in the response. The default value is 100. The maximum value is 500.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the GetExclusionsPreviewRequest action. Subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExclusionsPreview' value with any optional fields omitted.
mkGetExclusionsPreview
    :: Types.AssessmentTemplateArn -- ^ 'assessmentTemplateArn'
    -> Types.PreviewToken -- ^ 'previewToken'
    -> GetExclusionsPreview
mkGetExclusionsPreview assessmentTemplateArn previewToken
  = GetExclusionsPreview'{assessmentTemplateArn, previewToken,
                          locale = Core.Nothing, maxResults = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The ARN that specifies the assessment template for which the exclusions preview was requested.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepAssessmentTemplateArn :: Lens.Lens' GetExclusionsPreview Types.AssessmentTemplateArn
gepAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# INLINEABLE gepAssessmentTemplateArn #-}
{-# DEPRECATED assessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead"  #-}

-- | The unique identifier associated of the exclusions preview.
--
-- /Note:/ Consider using 'previewToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepPreviewToken :: Lens.Lens' GetExclusionsPreview Types.PreviewToken
gepPreviewToken = Lens.field @"previewToken"
{-# INLINEABLE gepPreviewToken #-}
{-# DEPRECATED previewToken "Use generic-lens or generic-optics with 'previewToken' instead"  #-}

-- | The locale into which you want to translate the exclusion's title, description, and recommendation.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepLocale :: Lens.Lens' GetExclusionsPreview (Core.Maybe Types.Locale)
gepLocale = Lens.field @"locale"
{-# INLINEABLE gepLocale #-}
{-# DEPRECATED locale "Use generic-lens or generic-optics with 'locale' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 100. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepMaxResults :: Lens.Lens' GetExclusionsPreview (Core.Maybe Core.Int)
gepMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gepMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the GetExclusionsPreviewRequest action. Subsequent calls to the action fill nextToken in the request with the value of nextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepNextToken :: Lens.Lens' GetExclusionsPreview (Core.Maybe Types.PaginationToken)
gepNextToken = Lens.field @"nextToken"
{-# INLINEABLE gepNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetExclusionsPreview where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetExclusionsPreview where
        toHeaders GetExclusionsPreview{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.GetExclusionsPreview")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetExclusionsPreview where
        toJSON GetExclusionsPreview{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("assessmentTemplateArn" Core..= assessmentTemplateArn),
                  Core.Just ("previewToken" Core..= previewToken),
                  ("locale" Core..=) Core.<$> locale,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetExclusionsPreview where
        type Rs GetExclusionsPreview = GetExclusionsPreviewResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetExclusionsPreviewResponse' Core.<$>
                   (x Core..: "previewStatus") Core.<*> x Core..:? "exclusionPreviews"
                     Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetExclusionsPreviewResponse' smart constructor.
data GetExclusionsPreviewResponse = GetExclusionsPreviewResponse'
  { previewStatus :: Types.PreviewStatus
    -- ^ Specifies the status of the request to generate an exclusions preview.
  , exclusionPreviews :: Core.Maybe [Types.ExclusionPreview]
    -- ^ Information about the exclusions included in the preview.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ When a response is generated, if there is more data to be listed, this parameters is present in the response and contains the value to use for the nextToken parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExclusionsPreviewResponse' value with any optional fields omitted.
mkGetExclusionsPreviewResponse
    :: Types.PreviewStatus -- ^ 'previewStatus'
    -> Core.Int -- ^ 'responseStatus'
    -> GetExclusionsPreviewResponse
mkGetExclusionsPreviewResponse previewStatus responseStatus
  = GetExclusionsPreviewResponse'{previewStatus,
                                  exclusionPreviews = Core.Nothing, nextToken = Core.Nothing,
                                  responseStatus}

-- | Specifies the status of the request to generate an exclusions preview.
--
-- /Note:/ Consider using 'previewStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsPreviewStatus :: Lens.Lens' GetExclusionsPreviewResponse Types.PreviewStatus
geprrsPreviewStatus = Lens.field @"previewStatus"
{-# INLINEABLE geprrsPreviewStatus #-}
{-# DEPRECATED previewStatus "Use generic-lens or generic-optics with 'previewStatus' instead"  #-}

-- | Information about the exclusions included in the preview.
--
-- /Note:/ Consider using 'exclusionPreviews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsExclusionPreviews :: Lens.Lens' GetExclusionsPreviewResponse (Core.Maybe [Types.ExclusionPreview])
geprrsExclusionPreviews = Lens.field @"exclusionPreviews"
{-# INLINEABLE geprrsExclusionPreviews #-}
{-# DEPRECATED exclusionPreviews "Use generic-lens or generic-optics with 'exclusionPreviews' instead"  #-}

-- | When a response is generated, if there is more data to be listed, this parameters is present in the response and contains the value to use for the nextToken parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsNextToken :: Lens.Lens' GetExclusionsPreviewResponse (Core.Maybe Types.PaginationToken)
geprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE geprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsResponseStatus :: Lens.Lens' GetExclusionsPreviewResponse Core.Int
geprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE geprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
