{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.StopAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the assessment run that is specified by the ARN of the assessment run.
module Network.AWS.Inspector.StopAssessmentRun
    (
    -- * Creating a request
      StopAssessmentRun (..)
    , mkStopAssessmentRun
    -- ** Request lenses
    , sarAssessmentRunArn
    , sarStopAction

    -- * Destructuring the response
    , StopAssessmentRunResponse (..)
    , mkStopAssessmentRunResponse
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopAssessmentRun' smart constructor.
data StopAssessmentRun = StopAssessmentRun'
  { assessmentRunArn :: Types.Arn
    -- ^ The ARN of the assessment run that you want to stop.
  , stopAction :: Core.Maybe Types.StopAction
    -- ^ An input option that can be set to either START_EVALUATION or SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS agent from collecting data and begins the results evaluation and the findings generation process. SKIP_EVALUATION cancels the assessment run immediately, after which no findings are generated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopAssessmentRun' value with any optional fields omitted.
mkStopAssessmentRun
    :: Types.Arn -- ^ 'assessmentRunArn'
    -> StopAssessmentRun
mkStopAssessmentRun assessmentRunArn
  = StopAssessmentRun'{assessmentRunArn, stopAction = Core.Nothing}

-- | The ARN of the assessment run that you want to stop.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarAssessmentRunArn :: Lens.Lens' StopAssessmentRun Types.Arn
sarAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# INLINEABLE sarAssessmentRunArn #-}
{-# DEPRECATED assessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead"  #-}

-- | An input option that can be set to either START_EVALUATION or SKIP_EVALUATION. START_EVALUATION (the default value), stops the AWS agent from collecting data and begins the results evaluation and the findings generation process. SKIP_EVALUATION cancels the assessment run immediately, after which no findings are generated.
--
-- /Note:/ Consider using 'stopAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sarStopAction :: Lens.Lens' StopAssessmentRun (Core.Maybe Types.StopAction)
sarStopAction = Lens.field @"stopAction"
{-# INLINEABLE sarStopAction #-}
{-# DEPRECATED stopAction "Use generic-lens or generic-optics with 'stopAction' instead"  #-}

instance Core.ToQuery StopAssessmentRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopAssessmentRun where
        toHeaders StopAssessmentRun{..}
          = Core.pure ("X-Amz-Target", "InspectorService.StopAssessmentRun")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopAssessmentRun where
        toJSON StopAssessmentRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("assessmentRunArn" Core..= assessmentRunArn),
                  ("stopAction" Core..=) Core.<$> stopAction])

instance Core.AWSRequest StopAssessmentRun where
        type Rs StopAssessmentRun = StopAssessmentRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StopAssessmentRunResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopAssessmentRunResponse' smart constructor.
data StopAssessmentRunResponse = StopAssessmentRunResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopAssessmentRunResponse' value with any optional fields omitted.
mkStopAssessmentRunResponse
    :: StopAssessmentRunResponse
mkStopAssessmentRunResponse = StopAssessmentRunResponse'
