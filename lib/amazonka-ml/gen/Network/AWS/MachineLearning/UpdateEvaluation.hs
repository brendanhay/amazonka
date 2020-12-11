{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateEvaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @EvaluationName@ of an @Evaluation@ .
--
-- You can use the @GetEvaluation@ operation to view the contents of the updated data element.
module Network.AWS.MachineLearning.UpdateEvaluation
  ( -- * Creating a request
    UpdateEvaluation (..),
    mkUpdateEvaluation,

    -- ** Request lenses
    ueEvaluationId,
    ueEvaluationName,

    -- * Destructuring the response
    UpdateEvaluationResponse (..),
    mkUpdateEvaluationResponse,

    -- ** Response lenses
    uersEvaluationId,
    uersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEvaluation' smart constructor.
data UpdateEvaluation = UpdateEvaluation'
  { evaluationId ::
      Lude.Text,
    evaluationName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEvaluation' with the minimum fields required to make a request.
--
-- * 'evaluationId' - The ID assigned to the @Evaluation@ during creation.
-- * 'evaluationName' - A new user-supplied name or description of the @Evaluation@ that will replace the current content.
mkUpdateEvaluation ::
  -- | 'evaluationId'
  Lude.Text ->
  -- | 'evaluationName'
  Lude.Text ->
  UpdateEvaluation
mkUpdateEvaluation pEvaluationId_ pEvaluationName_ =
  UpdateEvaluation'
    { evaluationId = pEvaluationId_,
      evaluationName = pEvaluationName_
    }

-- | The ID assigned to the @Evaluation@ during creation.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEvaluationId :: Lens.Lens' UpdateEvaluation Lude.Text
ueEvaluationId = Lens.lens (evaluationId :: UpdateEvaluation -> Lude.Text) (\s a -> s {evaluationId = a} :: UpdateEvaluation)
{-# DEPRECATED ueEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | A new user-supplied name or description of the @Evaluation@ that will replace the current content.
--
-- /Note:/ Consider using 'evaluationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEvaluationName :: Lens.Lens' UpdateEvaluation Lude.Text
ueEvaluationName = Lens.lens (evaluationName :: UpdateEvaluation -> Lude.Text) (\s a -> s {evaluationName = a} :: UpdateEvaluation)
{-# DEPRECATED ueEvaluationName "Use generic-lens or generic-optics with 'evaluationName' instead." #-}

instance Lude.AWSRequest UpdateEvaluation where
  type Rs UpdateEvaluation = UpdateEvaluationResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEvaluationResponse'
            Lude.<$> (x Lude..?> "EvaluationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEvaluation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.UpdateEvaluation" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEvaluation where
  toJSON UpdateEvaluation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EvaluationId" Lude..= evaluationId),
            Lude.Just ("EvaluationName" Lude..= evaluationName)
          ]
      )

instance Lude.ToPath UpdateEvaluation where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEvaluation where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @UpdateEvaluation@ operation.
--
-- You can see the updated content by using the @GetEvaluation@ operation.
--
-- /See:/ 'mkUpdateEvaluationResponse' smart constructor.
data UpdateEvaluationResponse = UpdateEvaluationResponse'
  { evaluationId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEvaluationResponse' with the minimum fields required to make a request.
--
-- * 'evaluationId' - The ID assigned to the @Evaluation@ during creation. This value should be identical to the value of the @Evaluation@ in the request.
-- * 'responseStatus' - The response status code.
mkUpdateEvaluationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEvaluationResponse
mkUpdateEvaluationResponse pResponseStatus_ =
  UpdateEvaluationResponse'
    { evaluationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID assigned to the @Evaluation@ during creation. This value should be identical to the value of the @Evaluation@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersEvaluationId :: Lens.Lens' UpdateEvaluationResponse (Lude.Maybe Lude.Text)
uersEvaluationId = Lens.lens (evaluationId :: UpdateEvaluationResponse -> Lude.Maybe Lude.Text) (\s a -> s {evaluationId = a} :: UpdateEvaluationResponse)
{-# DEPRECATED uersEvaluationId "Use generic-lens or generic-optics with 'evaluationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uersResponseStatus :: Lens.Lens' UpdateEvaluationResponse Lude.Int
uersResponseStatus = Lens.lens (responseStatus :: UpdateEvaluationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEvaluationResponse)
{-# DEPRECATED uersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
