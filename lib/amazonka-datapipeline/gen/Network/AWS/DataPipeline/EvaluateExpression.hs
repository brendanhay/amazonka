{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    eerrsEvaluatedExpression,
    eerrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for EvaluateExpression.
--
-- /See:/ 'mkEvaluateExpression' smart constructor.
data EvaluateExpression = EvaluateExpression'
  { -- | The ID of the pipeline.
    pipelineId :: Types.PipelineId,
    -- | The ID of the object.
    objectId :: Types.ObjectId,
    -- | The expression to evaluate.
    expression :: Types.LongString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluateExpression' value with any optional fields omitted.
mkEvaluateExpression ::
  -- | 'pipelineId'
  Types.PipelineId ->
  -- | 'objectId'
  Types.ObjectId ->
  -- | 'expression'
  Types.LongString ->
  EvaluateExpression
mkEvaluateExpression pipelineId objectId expression =
  EvaluateExpression' {pipelineId, objectId, expression}

-- | The ID of the pipeline.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eePipelineId :: Lens.Lens' EvaluateExpression Types.PipelineId
eePipelineId = Lens.field @"pipelineId"
{-# DEPRECATED eePipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The ID of the object.
--
-- /Note:/ Consider using 'objectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeObjectId :: Lens.Lens' EvaluateExpression Types.ObjectId
eeObjectId = Lens.field @"objectId"
{-# DEPRECATED eeObjectId "Use generic-lens or generic-optics with 'objectId' instead." #-}

-- | The expression to evaluate.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeExpression :: Lens.Lens' EvaluateExpression Types.LongString
eeExpression = Lens.field @"expression"
{-# DEPRECATED eeExpression "Use generic-lens or generic-optics with 'expression' instead." #-}

instance Core.FromJSON EvaluateExpression where
  toJSON EvaluateExpression {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineId" Core..= pipelineId),
            Core.Just ("objectId" Core..= objectId),
            Core.Just ("expression" Core..= expression)
          ]
      )

instance Core.AWSRequest EvaluateExpression where
  type Rs EvaluateExpression = EvaluateExpressionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DataPipeline.EvaluateExpression")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          EvaluateExpressionResponse'
            Core.<$> (x Core..: "evaluatedExpression")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of EvaluateExpression.
--
-- /See:/ 'mkEvaluateExpressionResponse' smart constructor.
data EvaluateExpressionResponse = EvaluateExpressionResponse'
  { -- | The evaluated expression.
    evaluatedExpression :: Types.LongString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluateExpressionResponse' value with any optional fields omitted.
mkEvaluateExpressionResponse ::
  -- | 'evaluatedExpression'
  Types.LongString ->
  -- | 'responseStatus'
  Core.Int ->
  EvaluateExpressionResponse
mkEvaluateExpressionResponse evaluatedExpression responseStatus =
  EvaluateExpressionResponse' {evaluatedExpression, responseStatus}

-- | The evaluated expression.
--
-- /Note:/ Consider using 'evaluatedExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eerrsEvaluatedExpression :: Lens.Lens' EvaluateExpressionResponse Types.LongString
eerrsEvaluatedExpression = Lens.field @"evaluatedExpression"
{-# DEPRECATED eerrsEvaluatedExpression "Use generic-lens or generic-optics with 'evaluatedExpression' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eerrsResponseStatus :: Lens.Lens' EvaluateExpressionResponse Core.Int
eerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
