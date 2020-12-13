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
    ceMLModelId,
    ceEvaluationId,
    ceEvaluationDataSourceId,
    ceEvaluationName,

    -- * Destructuring the response
    CreateEvaluationResponse (..),
    mkCreateEvaluationResponse,

    -- ** Response lenses
    cersEvaluationId,
    cersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEvaluation' smart constructor.
data CreateEvaluation = CreateEvaluation'
  { -- | The ID of the @MLModel@ to evaluate.
    --
    -- The schema used in creating the @MLModel@ must match the schema of the @DataSource@ used in the @Evaluation@ .
    mLModelId :: Lude.Text,
    -- | A user-supplied ID that uniquely identifies the @Evaluation@ .
    evaluationId :: Lude.Text,
    -- | The ID of the @DataSource@ for the evaluation. The schema of the @DataSource@ must match the schema used to create the @MLModel@ .
    evaluationDataSourceId :: Lude.Text,
    -- | A user-supplied name or description of the @Evaluation@ .
    evaluationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEvaluation' with the minimum fields required to make a request.
--
-- * 'mLModelId' - The ID of the @MLModel@ to evaluate.
--
-- The schema used in creating the @MLModel@ must match the schema of the @DataSource@ used in the @Evaluation@ .
-- * 'evaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@ .
-- * 'evaluationDataSourceId' - The ID of the @DataSource@ for the evaluation. The schema of the @DataSource@ must match the schema used to create the @MLModel@ .
-- * 'evaluationName' - A user-supplied name or description of the @Evaluation@ .
mkCreateEvaluation ::
  -- | 'mLModelId'
  Lude.Text ->
  -- | 'evaluationId'
  Lude.Text ->
  -- | 'evaluationDataSourceId'
  Lude.Text ->
  CreateEvaluation
mkCreateEvaluation
  pMLModelId_
  pEvaluationId_
  pEvaluationDataSourceId_ =
    CreateEvaluation'
      { mLModelId = pMLModelId_,
        evaluationId = pEvaluationId_,
        evaluationDataSourceId = pEvaluationDataSourceId_,
        evaluationName = Lude.Nothing
      }

-- | The ID of the @MLModel@ to evaluate.
--
-- The schema used in creating the @MLModel@ must match the schema of the @DataSource@ used in the @Evaluation@ .
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceMLModelId :: Lens.Lens' CreateEvaluation Lude.Text
ceMLModelId = Lens.lens (mLModelId :: CreateEvaluation -> Lude.Text) (\s a -> s {mLModelId = a} :: CreateEvaluation)
{-# DEPRECATED ceMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | A user-supplied ID that uniquely identifies the @Evaluation@ .
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEvaluationId :: Lens.Lens' CreateEvaluation Lude.Text
ceEvaluationId = Lens.lens (evaluationId :: CreateEvaluation -> Lude.Text) (\s a -> s {evaluationId = a} :: CreateEvaluation)
{-# DEPRECATED ceEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The ID of the @DataSource@ for the evaluation. The schema of the @DataSource@ must match the schema used to create the @MLModel@ .
--
-- /Note:/ Consider using 'evaluationDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEvaluationDataSourceId :: Lens.Lens' CreateEvaluation Lude.Text
ceEvaluationDataSourceId = Lens.lens (evaluationDataSourceId :: CreateEvaluation -> Lude.Text) (\s a -> s {evaluationDataSourceId = a} :: CreateEvaluation)
{-# DEPRECATED ceEvaluationDataSourceId "Use generic-lens or generic-optics with 'evaluationDataSourceId' instead." #-}

-- | A user-supplied name or description of the @Evaluation@ .
--
-- /Note:/ Consider using 'evaluationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceEvaluationName :: Lens.Lens' CreateEvaluation (Lude.Maybe Lude.Text)
ceEvaluationName = Lens.lens (evaluationName :: CreateEvaluation -> Lude.Maybe Lude.Text) (\s a -> s {evaluationName = a} :: CreateEvaluation)
{-# DEPRECATED ceEvaluationName "Use generic-lens or generic-optics with 'evaluationName' instead." #-}

instance Lude.AWSRequest CreateEvaluation where
  type Rs CreateEvaluation = CreateEvaluationResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEvaluationResponse'
            Lude.<$> (x Lude..?> "EvaluationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEvaluation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.CreateEvaluation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEvaluation where
  toJSON CreateEvaluation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MLModelId" Lude..= mLModelId),
            Lude.Just ("EvaluationId" Lude..= evaluationId),
            Lude.Just
              ("EvaluationDataSourceId" Lude..= evaluationDataSourceId),
            ("EvaluationName" Lude..=) Lude.<$> evaluationName
          ]
      )

instance Lude.ToPath CreateEvaluation where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEvaluation where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateEvaluation@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- @CreateEvaluation@ operation is asynchronous. You can poll for status updates by using the @GetEvcaluation@ operation and checking the @Status@ parameter.
--
-- /See:/ 'mkCreateEvaluationResponse' smart constructor.
data CreateEvaluationResponse = CreateEvaluationResponse'
  { -- | The user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
    evaluationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEvaluationResponse' with the minimum fields required to make a request.
--
-- * 'evaluationId' - The user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
-- * 'responseStatus' - The response status code.
mkCreateEvaluationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEvaluationResponse
mkCreateEvaluationResponse pResponseStatus_ =
  CreateEvaluationResponse'
    { evaluationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cersEvaluationId :: Lens.Lens' CreateEvaluationResponse (Lude.Maybe Lude.Text)
cersEvaluationId = Lens.lens (evaluationId :: CreateEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {evaluationId = a} :: CreateEvaluationResponse)
{-# DEPRECATED cersEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cersResponseStatus :: Lens.Lens' CreateEvaluationResponse Lude.Int
cersResponseStatus = Lens.lens (responseStatus :: CreateEvaluationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEvaluationResponse)
{-# DEPRECATED cersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
