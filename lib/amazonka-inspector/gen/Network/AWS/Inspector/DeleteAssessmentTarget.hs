{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DeleteAssessmentTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the assessment target that is specified by the ARN of the assessment target.
module Network.AWS.Inspector.DeleteAssessmentTarget
    (
    -- * Creating a request
      DeleteAssessmentTarget (..)
    , mkDeleteAssessmentTarget
    -- ** Request lenses
    , datAssessmentTargetArn

    -- * Destructuring the response
    , DeleteAssessmentTargetResponse (..)
    , mkDeleteAssessmentTargetResponse
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAssessmentTarget' smart constructor.
newtype DeleteAssessmentTarget = DeleteAssessmentTarget'
  { assessmentTargetArn :: Types.Arn
    -- ^ The ARN that specifies the assessment target that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentTarget' value with any optional fields omitted.
mkDeleteAssessmentTarget
    :: Types.Arn -- ^ 'assessmentTargetArn'
    -> DeleteAssessmentTarget
mkDeleteAssessmentTarget assessmentTargetArn
  = DeleteAssessmentTarget'{assessmentTargetArn}

-- | The ARN that specifies the assessment target that you want to delete.
--
-- /Note:/ Consider using 'assessmentTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datAssessmentTargetArn :: Lens.Lens' DeleteAssessmentTarget Types.Arn
datAssessmentTargetArn = Lens.field @"assessmentTargetArn"
{-# INLINEABLE datAssessmentTargetArn #-}
{-# DEPRECATED assessmentTargetArn "Use generic-lens or generic-optics with 'assessmentTargetArn' instead"  #-}

instance Core.ToQuery DeleteAssessmentTarget where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAssessmentTarget where
        toHeaders DeleteAssessmentTarget{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.DeleteAssessmentTarget")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAssessmentTarget where
        toJSON DeleteAssessmentTarget{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("assessmentTargetArn" Core..= assessmentTargetArn)])

instance Core.AWSRequest DeleteAssessmentTarget where
        type Rs DeleteAssessmentTarget = DeleteAssessmentTargetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteAssessmentTargetResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAssessmentTargetResponse' smart constructor.
data DeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAssessmentTargetResponse' value with any optional fields omitted.
mkDeleteAssessmentTargetResponse
    :: DeleteAssessmentTargetResponse
mkDeleteAssessmentTargetResponse = DeleteAssessmentTargetResponse'
