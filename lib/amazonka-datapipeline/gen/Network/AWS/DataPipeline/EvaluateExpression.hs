{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.EvaluateExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @EvaluateExpression@ to evaluate a string in the context of the specified object. For example, a task runner can evaluate SQL queries stored in Amazon S3.
module Network.AWS.DataPipeline.EvaluateExpression
  ( -- * Creating a request
    EvaluateExpression (..),
    mkEvaluateExpression,

    -- ** Request lenses
    eePipelineId,
    eeObjectId,
    eeExpression,

    -- * Destructuring the response
    EvaluateExpressionResponse (..),
    mkEvaluateExpressionResponse,

    -- ** Response lenses
    eersResponseStatus,
    eersEvaluatedExpression,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for EvaluateExpression.
--
-- /See:/ 'mkEvaluateExpression' smart constructor.
data EvaluateExpression = EvaluateExpression'
  { pipelineId ::
      Lude.Text,
    objectId :: Lude.Text,
    expression :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluateExpression' with the minimum fields required to make a request.
--
-- * 'expression' - The expression to evaluate.
-- * 'objectId' - The ID of the object.
-- * 'pipelineId' - The ID of the pipeline.
mkEvaluateExpression ::
  -- | 'pipelineId'
  Lude.Text ->
  -- | 'objectId'
  Lude.Text ->
  -- | 'expression'
  Lude.Text ->
  EvaluateExpression
mkEvaluateExpression pPipelineId_ pObjectId_ pExpression_ =
  EvaluateExpression'
    { pipelineId = pPipelineId_,
      objectId = pObjectId_,
      expression = pExpression_
    }

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eePipelineId :: Lens.Lens' EvaluateExpression Lude.Text
eePipelineId = Lens.lens (pipelineId :: EvaluateExpression -> Lude.Text) (\s a -> s {pipelineId = a} :: EvaluateExpression)
{-# DEPRECATED eePipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The ID of the object.
--
-- /Note:/ Consider using 'objectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeObjectId :: Lens.Lens' EvaluateExpression Lude.Text
eeObjectId = Lens.lens (objectId :: EvaluateExpression -> Lude.Text) (\s a -> s {objectId = a} :: EvaluateExpression)
{-# DEPRECATED eeObjectId "Use generic-lens or generic-optics with 'objectId' instead." #-}

-- | The expression to evaluate.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeExpression :: Lens.Lens' EvaluateExpression Lude.Text
eeExpression = Lens.lens (expression :: EvaluateExpression -> Lude.Text) (\s a -> s {expression = a} :: EvaluateExpression)
{-# DEPRECATED eeExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Lude.AWSRequest EvaluateExpression where
  type Rs EvaluateExpression = EvaluateExpressionResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          EvaluateExpressionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "evaluatedExpression")
      )

instance Lude.ToHeaders EvaluateExpression where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.EvaluateExpression" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EvaluateExpression where
  toJSON EvaluateExpression' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("pipelineId" Lude..= pipelineId),
            Lude.Just ("objectId" Lude..= objectId),
            Lude.Just ("expression" Lude..= expression)
          ]
      )

instance Lude.ToPath EvaluateExpression where
  toPath = Lude.const "/"

instance Lude.ToQuery EvaluateExpression where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of EvaluateExpression.
--
-- /See:/ 'mkEvaluateExpressionResponse' smart constructor.
data EvaluateExpressionResponse = EvaluateExpressionResponse'
  { responseStatus ::
      Lude.Int,
    evaluatedExpression :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluateExpressionResponse' with the minimum fields required to make a request.
--
-- * 'evaluatedExpression' - The evaluated expression.
-- * 'responseStatus' - The response status code.
mkEvaluateExpressionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'evaluatedExpression'
  Lude.Text ->
  EvaluateExpressionResponse
mkEvaluateExpressionResponse pResponseStatus_ pEvaluatedExpression_ =
  EvaluateExpressionResponse'
    { responseStatus = pResponseStatus_,
      evaluatedExpression = pEvaluatedExpression_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eersResponseStatus :: Lens.Lens' EvaluateExpressionResponse Lude.Int
eersResponseStatus = Lens.lens (responseStatus :: EvaluateExpressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EvaluateExpressionResponse)
{-# DEPRECATED eersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The evaluated expression.
--
-- /Note:/ Consider using 'evaluatedExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eersEvaluatedExpression :: Lens.Lens' EvaluateExpressionResponse Lude.Text
eersEvaluatedExpression = Lens.lens (evaluatedExpression :: EvaluateExpressionResponse -> Lude.Text) (\s a -> s {evaluatedExpression = a} :: EvaluateExpressionResponse)
{-# DEPRECATED eersEvaluatedExpression "Use generic-lens or generic-optics with 'evaluatedExpression' instead." #-}
