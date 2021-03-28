{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteEvaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the @DELETED@ status to an @Evaluation@ , rendering it unusable.
--
-- After invoking the @DeleteEvaluation@ operation, you can use the @GetEvaluation@ operation to verify that the status of the @Evaluation@ changed to @DELETED@ .
-- ____Caution__ 
-- The results of the @DeleteEvaluation@ operation are irreversible.
-- __ 
module Network.AWS.MachineLearning.DeleteEvaluation
    (
    -- * Creating a request
      DeleteEvaluation (..)
    , mkDeleteEvaluation
    -- ** Request lenses
    , deEvaluationId

    -- * Destructuring the response
    , DeleteEvaluationResponse (..)
    , mkDeleteEvaluationResponse
    -- ** Response lenses
    , derrsEvaluationId
    , derrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEvaluation' smart constructor.
newtype DeleteEvaluation = DeleteEvaluation'
  { evaluationId :: Types.EntityId
    -- ^ A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluation' value with any optional fields omitted.
mkDeleteEvaluation
    :: Types.EntityId -- ^ 'evaluationId'
    -> DeleteEvaluation
mkDeleteEvaluation evaluationId = DeleteEvaluation'{evaluationId}

-- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEvaluationId :: Lens.Lens' DeleteEvaluation Types.EntityId
deEvaluationId = Lens.field @"evaluationId"
{-# INLINEABLE deEvaluationId #-}
{-# DEPRECATED evaluationId "Use generic-lens or generic-optics with 'evaluationId' instead"  #-}

instance Core.ToQuery DeleteEvaluation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteEvaluation where
        toHeaders DeleteEvaluation{..}
          = Core.pure ("X-Amz-Target", "AmazonML_20141212.DeleteEvaluation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteEvaluation where
        toJSON DeleteEvaluation{..}
          = Core.object
              (Core.catMaybes [Core.Just ("EvaluationId" Core..= evaluationId)])

instance Core.AWSRequest DeleteEvaluation where
        type Rs DeleteEvaluation = DeleteEvaluationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteEvaluationResponse' Core.<$>
                   (x Core..:? "EvaluationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @DeleteEvaluation@ operation. The output indicates that Amazon Machine Learning (Amazon ML) received the request.
--
-- You can use the @GetEvaluation@ operation and check the value of the @Status@ parameter to see whether an @Evaluation@ is marked as @DELETED@ .
--
-- /See:/ 'mkDeleteEvaluationResponse' smart constructor.
data DeleteEvaluationResponse = DeleteEvaluationResponse'
  { evaluationId :: Core.Maybe Types.EvaluationId
    -- ^ A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluationResponse' value with any optional fields omitted.
mkDeleteEvaluationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteEvaluationResponse
mkDeleteEvaluationResponse responseStatus
  = DeleteEvaluationResponse'{evaluationId = Core.Nothing,
                              responseStatus}

-- | A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEvaluationId :: Lens.Lens' DeleteEvaluationResponse (Core.Maybe Types.EvaluationId)
derrsEvaluationId = Lens.field @"evaluationId"
{-# INLINEABLE derrsEvaluationId #-}
{-# DEPRECATED evaluationId "Use generic-lens or generic-optics with 'evaluationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeleteEvaluationResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
