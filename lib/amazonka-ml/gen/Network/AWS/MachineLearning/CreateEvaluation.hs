{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateEvaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new @Evaluation@ of an @MLModel@ . An @MLModel@ is evaluated on a set of observations associated to a @DataSource@ . Like a @DataSource@ for an @MLModel@ , the @DataSource@ for an @Evaluation@ contains values for the @Target Variable@ . The @Evaluation@ compares the predicted result for each observation to the actual outcome and provides a summary so that you know how effective the @MLModel@ functions on the test data. Evaluation generates a relevant performance metric, such as BinaryAUC, RegressionRMSE or MulticlassAvgFScore based on the corresponding @MLModelType@ : @BINARY@ , @REGRESSION@ or @MULTICLASS@ .
--
-- @CreateEvaluation@ is an asynchronous operation. In response to @CreateEvaluation@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the evaluation status to @PENDING@ . After the @Evaluation@ is created and ready for use, Amazon ML sets the status to @COMPLETED@ .
-- You can use the @GetEvaluation@ operation to check progress of the evaluation during the creation operation.
module Network.AWS.MachineLearning.CreateEvaluation
  ( -- * Creating a request
    CreateEvaluation (..),
    mkCreateEvaluation,

    -- ** Request lenses
    ceEvaluationId,
    ceMLModelId,
    ceEvaluationDataSourceId,
    ceEvaluationName,

    -- * Destructuring the response
    CreateEvaluationResponse (..),
    mkCreateEvaluationResponse,

    -- ** Response lenses
    cerrsEvaluationId,
    cerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEvaluation' smart constructor.
data CreateEvaluation = CreateEvaluation'
  { -- | A user-supplied ID that uniquely identifies the @Evaluation@ .
    evaluationId :: Types.EvaluationId,
    -- | The ID of the @MLModel@ to evaluate.
    --
    -- The schema used in creating the @MLModel@ must match the schema of the @DataSource@ used in the @Evaluation@ .
    mLModelId :: Types.MLModelId,
    -- | The ID of the @DataSource@ for the evaluation. The schema of the @DataSource@ must match the schema used to create the @MLModel@ .
    evaluationDataSourceId :: Types.EvaluationDataSourceId,
    -- | A user-supplied name or description of the @Evaluation@ .
    evaluationName :: Core.Maybe Types.EvaluationName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEvaluation' value with any optional fields omitted.
mkCreateEvaluation ::
  -- | 'evaluationId'
  Types.EvaluationId ->
  -- | 'mLModelId'
  Types.MLModelId ->
  -- | 'evaluationDataSourceId'
  Types.EvaluationDataSourceId ->
  CreateEvaluation
mkCreateEvaluation evaluationId mLModelId evaluationDataSourceId =
  CreateEvaluation'
    { evaluationId,
      mLModelId,
      evaluationDataSourceId,
      evaluationName = Core.Nothing
    }

-- | A user-supplied ID that uniquely identifies the @Evaluation@ .
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEvaluationId :: Lens.Lens' CreateEvaluation Types.EvaluationId
ceEvaluationId = Lens.field @"evaluationId"
{-# DEPRECATED ceEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The ID of the @MLModel@ to evaluate.
--
-- The schema used in creating the @MLModel@ must match the schema of the @DataSource@ used in the @Evaluation@ .
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMLModelId :: Lens.Lens' CreateEvaluation Types.MLModelId
ceMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED ceMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The ID of the @DataSource@ for the evaluation. The schema of the @DataSource@ must match the schema used to create the @MLModel@ .
--
-- /Note:/ Consider using 'evaluationDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEvaluationDataSourceId :: Lens.Lens' CreateEvaluation Types.EvaluationDataSourceId
ceEvaluationDataSourceId = Lens.field @"evaluationDataSourceId"
{-# DEPRECATED ceEvaluationDataSourceId "Use generic-lens or generic-optics with 'evaluationDataSourceId' instead." #-}

-- | A user-supplied name or description of the @Evaluation@ .
--
-- /Note:/ Consider using 'evaluationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEvaluationName :: Lens.Lens' CreateEvaluation (Core.Maybe Types.EvaluationName)
ceEvaluationName = Lens.field @"evaluationName"
{-# DEPRECATED ceEvaluationName "Use generic-lens or generic-optics with 'evaluationName' instead." #-}

instance Core.FromJSON CreateEvaluation where
  toJSON CreateEvaluation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EvaluationId" Core..= evaluationId),
            Core.Just ("MLModelId" Core..= mLModelId),
            Core.Just
              ("EvaluationDataSourceId" Core..= evaluationDataSourceId),
            ("EvaluationName" Core..=) Core.<$> evaluationName
          ]
      )

instance Core.AWSRequest CreateEvaluation where
  type Rs CreateEvaluation = CreateEvaluationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.CreateEvaluation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEvaluationResponse'
            Core.<$> (x Core..:? "EvaluationId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @CreateEvaluation@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- @CreateEvaluation@ operation is asynchronous. You can poll for status updates by using the @GetEvcaluation@ operation and checking the @Status@ parameter.
--
-- /See:/ 'mkCreateEvaluationResponse' smart constructor.
data CreateEvaluationResponse = CreateEvaluationResponse'
  { -- | The user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
    evaluationId :: Core.Maybe Types.EvaluationId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEvaluationResponse' value with any optional fields omitted.
mkCreateEvaluationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateEvaluationResponse
mkCreateEvaluationResponse responseStatus =
  CreateEvaluationResponse'
    { evaluationId = Core.Nothing,
      responseStatus
    }

-- | The user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsEvaluationId :: Lens.Lens' CreateEvaluationResponse (Core.Maybe Types.EvaluationId)
cerrsEvaluationId = Lens.field @"evaluationId"
{-# DEPRECATED cerrsEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsResponseStatus :: Lens.Lens' CreateEvaluationResponse Core.Int
cerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
