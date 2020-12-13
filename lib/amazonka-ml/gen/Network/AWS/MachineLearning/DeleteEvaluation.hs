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
    drsEvaluationId,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEvaluation' smart constructor.
newtype DeleteEvaluation = DeleteEvaluation'
  { -- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
    evaluationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEvaluation' with the minimum fields required to make a request.
--
-- * 'evaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
mkDeleteEvaluation ::
  -- | 'evaluationId'
  Lude.Text ->
  DeleteEvaluation
mkDeleteEvaluation pEvaluationId_ =
  DeleteEvaluation' {evaluationId = pEvaluationId_}

-- | A user-supplied ID that uniquely identifies the @Evaluation@ to delete.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEvaluationId :: Lens.Lens' DeleteEvaluation Lude.Text
deEvaluationId = Lens.lens (evaluationId :: DeleteEvaluation -> Lude.Text) (\s a -> s {evaluationId = a} :: DeleteEvaluation)
{-# DEPRECATED deEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

instance Lude.AWSRequest DeleteEvaluation where
  type Rs DeleteEvaluation = DeleteEvaluationResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteEvaluationResponse'
            Lude.<$> (x Lude..?> "EvaluationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteEvaluation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DeleteEvaluation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEvaluation where
  toJSON DeleteEvaluation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EvaluationId" Lude..= evaluationId)])

instance Lude.ToPath DeleteEvaluation where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEvaluation where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteEvaluation@ operation. The output indicates that Amazon Machine Learning (Amazon ML) received the request.
--
-- You can use the @GetEvaluation@ operation and check the value of the @Status@ parameter to see whether an @Evaluation@ is marked as @DELETED@ .
--
-- /See:/ 'mkDeleteEvaluationResponse' smart constructor.
data DeleteEvaluationResponse = DeleteEvaluationResponse'
  { -- | A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
    evaluationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEvaluationResponse' with the minimum fields required to make a request.
--
-- * 'evaluationId' - A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
-- * 'responseStatus' - The response status code.
mkDeleteEvaluationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteEvaluationResponse
mkDeleteEvaluationResponse pResponseStatus_ =
  DeleteEvaluationResponse'
    { evaluationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the @Evaluation@ . This value should be identical to the value of the @EvaluationId@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEvaluationId :: Lens.Lens' DeleteEvaluationResponse (Lude.Maybe Lude.Text)
drsEvaluationId = Lens.lens (evaluationId :: DeleteEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {evaluationId = a} :: DeleteEvaluationResponse)
{-# DEPRECATED drsEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteEvaluationResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteEvaluationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteEvaluationResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
