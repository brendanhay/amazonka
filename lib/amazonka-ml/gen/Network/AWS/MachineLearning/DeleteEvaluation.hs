{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteEvaluation (..),
    mkDeleteEvaluation,

    -- ** Request lenses
    deEvaluationId,

    -- * Destructuring the response
    DeleteEvaluationResponse (..),
    mkDeleteEvaluationResponse,

    -- ** Response lenses
    derrsEvaluationId,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEvaluation' smart constructor.
newtype DeleteEvaluation = DeleteEvaluation'
  { -- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
    evaluationId :: Types.EntityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluation' value with any optional fields omitted.
mkDeleteEvaluation ::
  -- | 'evaluationId'
  Types.EntityId ->
  DeleteEvaluation
mkDeleteEvaluation evaluationId = DeleteEvaluation' {evaluationId}

-- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEvaluationId :: Lens.Lens' DeleteEvaluation Types.EntityId
deEvaluationId = Lens.field @"evaluationId"
{-# DEPRECATED deEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

instance Core.FromJSON DeleteEvaluation where
  toJSON DeleteEvaluation {..} =
    Core.object
      (Core.catMaybes [Core.Just ("EvaluationId" Core..= evaluationId)])

instance Core.AWSRequest DeleteEvaluation where
  type Rs DeleteEvaluation = DeleteEvaluationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.DeleteEvaluation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEvaluationResponse'
            Core.<$> (x Core..:? "EvaluationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @DeleteEvaluation@ operation. The output indicates that Amazon Machine Learning (Amazon ML) received the request.
--
-- You can use the @GetEvaluation@ operation and check the value of the @Status@ parameter to see whether an @Evaluation@ is marked as @DELETED@ .
--
-- /See:/ 'mkDeleteEvaluationResponse' smart constructor.
data DeleteEvaluationResponse = DeleteEvaluationResponse'
  { -- | A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
    evaluationId :: Core.Maybe Types.EvaluationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluationResponse' value with any optional fields omitted.
mkDeleteEvaluationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEvaluationResponse
mkDeleteEvaluationResponse responseStatus =
  DeleteEvaluationResponse'
    { evaluationId = Core.Nothing,
      responseStatus
    }

-- | A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEvaluationId :: Lens.Lens' DeleteEvaluationResponse (Core.Maybe Types.EvaluationId)
derrsEvaluationId = Lens.field @"evaluationId"
{-# DEPRECATED derrsEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeleteEvaluationResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
