{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartAssessmentRun (..)
    , mkStartAssessmentRun
    -- ** Request lenses
    , sarAssessmentTemplateArn
    , sarAssessmentRunName

    -- * Destructuring the response
    , StartAssessmentRunResponse (..)
    , mkStartAssessmentRunResponse
    -- ** Response lenses
    , sarrrsAssessmentRunArn
    , sarrrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartAssessmentRun' smart constructor.
data StartAssessmentRun = StartAssessmentRun'
  { assessmentTemplateArn :: Types.Arn
    -- ^ The ARN of the assessment template of the assessment run that you want to start.
  , assessmentRunName :: Core.Maybe Types.AssessmentRunName
    -- ^ You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAssessmentRun' value with any optional fields omitted.
mkStartAssessmentRun
    :: Types.Arn -- ^ 'assessmentTemplateArn'
    -> StartAssessmentRun
mkStartAssessmentRun assessmentTemplateArn
  = StartAssessmentRun'{assessmentTemplateArn,
                        assessmentRunName = Core.Nothing}

-- | The ARN of the assessment template of the assessment run that you want to start.
--
-- /Note:/ Consider using 'assessmentTemplateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentTemplateArn :: Lens.Lens' StartAssessmentRun Types.Arn
sarAssessmentTemplateArn = Lens.field @"assessmentTemplateArn"
{-# INLINEABLE sarAssessmentTemplateArn #-}
{-# DEPRECATED assessmentTemplateArn "Use generic-lens or generic-optics with 'assessmentTemplateArn' instead"  #-}

-- | You can specify the name for the assessment run. The name must be unique for the assessment template whose ARN is used to start the assessment run.
--
-- /Note:/ Consider using 'assessmentRunName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentRunName :: Lens.Lens' StartAssessmentRun (Core.Maybe Types.AssessmentRunName)
sarAssessmentRunName = Lens.field @"assessmentRunName"
{-# INLINEABLE sarAssessmentRunName #-}
{-# DEPRECATED assessmentRunName "Use generic-lens or generic-optics with 'assessmentRunName' instead"  #-}

instance Core.ToQuery StartAssessmentRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartAssessmentRun where
        toHeaders StartAssessmentRun{..}
          = Core.pure ("X-Amz-Target", "InspectorService.StartAssessmentRun")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartAssessmentRun where
        toJSON StartAssessmentRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("assessmentTemplateArn" Core..= assessmentTemplateArn),
                  ("assessmentRunName" Core..=) Core.<$> assessmentRunName])

instance Core.AWSRequest StartAssessmentRun where
        type Rs StartAssessmentRun = StartAssessmentRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartAssessmentRunResponse' Core.<$>
                   (x Core..: "assessmentRunArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartAssessmentRunResponse' smart constructor.
data StartAssessmentRunResponse = StartAssessmentRunResponse'
  { assessmentRunArn :: Types.AssessmentRunArn
    -- ^ The ARN of the assessment run that has been started.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartAssessmentRunResponse' value with any optional fields omitted.
mkStartAssessmentRunResponse
    :: Types.AssessmentRunArn -- ^ 'assessmentRunArn'
    -> Core.Int -- ^ 'responseStatus'
    -> StartAssessmentRunResponse
mkStartAssessmentRunResponse assessmentRunArn responseStatus
  = StartAssessmentRunResponse'{assessmentRunArn, responseStatus}

-- | The ARN of the assessment run that has been started.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrrsAssessmentRunArn :: Lens.Lens' StartAssessmentRunResponse Types.AssessmentRunArn
sarrrsAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# INLINEABLE sarrrsAssessmentRunArn #-}
{-# DEPRECATED assessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarrrsResponseStatus :: Lens.Lens' StartAssessmentRunResponse Core.Int
sarrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sarrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
