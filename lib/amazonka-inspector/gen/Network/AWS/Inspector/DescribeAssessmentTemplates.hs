{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeAssessmentTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment templates that are specified by the ARNs of the assessment templates.
module Network.AWS.Inspector.DescribeAssessmentTemplates
    (
    -- * Creating a request
      DescribeAssessmentTemplates (..)
    , mkDescribeAssessmentTemplates
    -- ** Request lenses
    , datAssessmentTemplateArns

    -- * Destructuring the response
    , DescribeAssessmentTemplatesResponse (..)
    , mkDescribeAssessmentTemplatesResponse
    -- ** Response lenses
    , datrrsAssessmentTemplates
    , datrrsFailedItems
    , datrrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAssessmentTemplates' smart constructor.
newtype DescribeAssessmentTemplates = DescribeAssessmentTemplates'
  { assessmentTemplateArns :: Core.NonEmpty Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAssessmentTemplates' value with any optional fields omitted.
mkDescribeAssessmentTemplates
    :: Core.NonEmpty Types.Arn -- ^ 'assessmentTemplateArns'
    -> DescribeAssessmentTemplates
mkDescribeAssessmentTemplates assessmentTemplateArns
  = DescribeAssessmentTemplates'{assessmentTemplateArns}

-- | Undocumented field.
--
-- /Note:/ Consider using 'assessmentTemplateArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTemplateArns :: Lens.Lens' DescribeAssessmentTemplates (Core.NonEmpty Types.Arn)
datAssessmentTemplateArns = Lens.field @"assessmentTemplateArns"
{-# INLINEABLE datAssessmentTemplateArns #-}
{-# DEPRECATED assessmentTemplateArns "Use generic-lens or generic-optics with 'assessmentTemplateArns' instead"  #-}

instance Core.ToQuery DescribeAssessmentTemplates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAssessmentTemplates where
        toHeaders DescribeAssessmentTemplates{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.DescribeAssessmentTemplates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAssessmentTemplates where
        toJSON DescribeAssessmentTemplates{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("assessmentTemplateArns" Core..= assessmentTemplateArns)])

instance Core.AWSRequest DescribeAssessmentTemplates where
        type Rs DescribeAssessmentTemplates =
             DescribeAssessmentTemplatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAssessmentTemplatesResponse' Core.<$>
                   (x Core..:? "assessmentTemplates" Core..!= Core.mempty) Core.<*>
                     x Core..:? "failedItems" Core..!= Core.mempty
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAssessmentTemplatesResponse' smart constructor.
data DescribeAssessmentTemplatesResponse = DescribeAssessmentTemplatesResponse'
  { assessmentTemplates :: [Types.AssessmentTemplate]
    -- ^ Information about the assessment templates.
  , failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails
    -- ^ Assessment template details that cannot be described. An error code is provided for each failed item.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAssessmentTemplatesResponse' value with any optional fields omitted.
mkDescribeAssessmentTemplatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAssessmentTemplatesResponse
mkDescribeAssessmentTemplatesResponse responseStatus
  = DescribeAssessmentTemplatesResponse'{assessmentTemplates =
                                           Core.mempty,
                                         failedItems = Core.mempty, responseStatus}

-- | Information about the assessment templates.
--
-- /Note:/ Consider using 'assessmentTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsAssessmentTemplates :: Lens.Lens' DescribeAssessmentTemplatesResponse [Types.AssessmentTemplate]
datrrsAssessmentTemplates = Lens.field @"assessmentTemplates"
{-# INLINEABLE datrrsAssessmentTemplates #-}
{-# DEPRECATED assessmentTemplates "Use generic-lens or generic-optics with 'assessmentTemplates' instead"  #-}

-- | Assessment template details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsFailedItems :: Lens.Lens' DescribeAssessmentTemplatesResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
datrrsFailedItems = Lens.field @"failedItems"
{-# INLINEABLE datrrsFailedItems #-}
{-# DEPRECATED failedItems "Use generic-lens or generic-optics with 'failedItems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrrsResponseStatus :: Lens.Lens' DescribeAssessmentTemplatesResponse Core.Int
datrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE datrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
