{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DeleteAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment run that is specified by the ARN of the assessment run.
module Network.AWS.Inspector.DeleteAssessmentRun
    (
    -- * Creating a request
      DeleteAssessmentRun (..)
    , mkDeleteAssessmentRun
    -- ** Request lenses
    , darAssessmentRunArn

    -- * Destructuring the response
    , DeleteAssessmentRunResponse (..)
    , mkDeleteAssessmentRunResponse
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAssessmentRun' smart constructor.
newtype DeleteAssessmentRun = DeleteAssessmentRun'
  { assessmentRunArn :: Types.Arn
    -- ^ The ARN that specifies the assessment run that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentRun' value with any optional fields omitted.
mkDeleteAssessmentRun
    :: Types.Arn -- ^ 'assessmentRunArn'
    -> DeleteAssessmentRun
mkDeleteAssessmentRun assessmentRunArn
  = DeleteAssessmentRun'{assessmentRunArn}

-- | The ARN that specifies the assessment run that you want to delete.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darAssessmentRunArn :: Lens.Lens' DeleteAssessmentRun Types.Arn
darAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# INLINEABLE darAssessmentRunArn #-}
{-# DEPRECATED assessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead"  #-}

instance Core.ToQuery DeleteAssessmentRun where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAssessmentRun where
        toHeaders DeleteAssessmentRun{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.DeleteAssessmentRun")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAssessmentRun where
        toJSON DeleteAssessmentRun{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("assessmentRunArn" Core..= assessmentRunArn)])

instance Core.AWSRequest DeleteAssessmentRun where
        type Rs DeleteAssessmentRun = DeleteAssessmentRunResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAssessmentRunResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAssessmentRunResponse' smart constructor.
data DeleteAssessmentRunResponse = DeleteAssessmentRunResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentRunResponse' value with any optional fields omitted.
mkDeleteAssessmentRunResponse
    :: DeleteAssessmentRunResponse
mkDeleteAssessmentRunResponse = DeleteAssessmentRunResponse'
