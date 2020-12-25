{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateMLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @MLModelName@ and the @ScoreThreshold@ of an @MLModel@ .
--
-- You can use the @GetMLModel@ operation to view the contents of the updated data element.
module Network.AWS.MachineLearning.UpdateMLModel
  ( -- * Creating a request
    UpdateMLModel (..),
    mkUpdateMLModel,

    -- ** Request lenses
    umlmMLModelId,
    umlmMLModelName,
    umlmScoreThreshold,

    -- * Destructuring the response
    UpdateMLModelResponse (..),
    mkUpdateMLModelResponse,

    -- ** Response lenses
    umlmrrsMLModelId,
    umlmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateMLModel' smart constructor.
data UpdateMLModel = UpdateMLModel'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Types.EntityId,
    -- | A user-supplied name or description of the @MLModel@ .
    mLModelName :: Core.Maybe Types.EntityName,
    -- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks the boundary between a positive prediction and a negative prediction.
    --
    -- Output values greater than or equal to the @ScoreThreshold@ receive a positive result from the @MLModel@ , such as @true@ . Output values less than the @ScoreThreshold@ receive a negative response from the @MLModel@ , such as @false@ .
    scoreThreshold :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMLModel' value with any optional fields omitted.
mkUpdateMLModel ::
  -- | 'mLModelId'
  Types.EntityId ->
  UpdateMLModel
mkUpdateMLModel mLModelId =
  UpdateMLModel'
    { mLModelId,
      mLModelName = Core.Nothing,
      scoreThreshold = Core.Nothing
    }

-- | The ID assigned to the @MLModel@ during creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmMLModelId :: Lens.Lens' UpdateMLModel Types.EntityId
umlmMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED umlmMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmMLModelName :: Lens.Lens' UpdateMLModel (Core.Maybe Types.EntityName)
umlmMLModelName = Lens.field @"mLModelName"
{-# DEPRECATED umlmMLModelName "Use generic-lens or generic-optics with 'mLModelName' instead." #-}

-- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a positive result from the @MLModel@ , such as @true@ . Output values less than the @ScoreThreshold@ receive a negative response from the @MLModel@ , such as @false@ .
--
-- /Note:/ Consider using 'scoreThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmScoreThreshold :: Lens.Lens' UpdateMLModel (Core.Maybe Core.Double)
umlmScoreThreshold = Lens.field @"scoreThreshold"
{-# DEPRECATED umlmScoreThreshold "Use generic-lens or generic-optics with 'scoreThreshold' instead." #-}

instance Core.FromJSON UpdateMLModel where
  toJSON UpdateMLModel {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MLModelId" Core..= mLModelId),
            ("MLModelName" Core..=) Core.<$> mLModelName,
            ("ScoreThreshold" Core..=) Core.<$> scoreThreshold
          ]
      )

instance Core.AWSRequest UpdateMLModel where
  type Rs UpdateMLModel = UpdateMLModelResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.UpdateMLModel")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMLModelResponse'
            Core.<$> (x Core..:? "MLModelId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of an @UpdateMLModel@ operation.
--
-- You can see the updated content by using the @GetMLModel@ operation.
--
-- /See:/ 'mkUpdateMLModelResponse' smart constructor.
data UpdateMLModelResponse = UpdateMLModelResponse'
  { -- | The ID assigned to the @MLModel@ during creation. This value should be identical to the value of the @MLModelID@ in the request.
    mLModelId :: Core.Maybe Types.MLModelId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMLModelResponse' value with any optional fields omitted.
mkUpdateMLModelResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMLModelResponse
mkUpdateMLModelResponse responseStatus =
  UpdateMLModelResponse' {mLModelId = Core.Nothing, responseStatus}

-- | The ID assigned to the @MLModel@ during creation. This value should be identical to the value of the @MLModelID@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmrrsMLModelId :: Lens.Lens' UpdateMLModelResponse (Core.Maybe Types.MLModelId)
umlmrrsMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED umlmrrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmrrsResponseStatus :: Lens.Lens' UpdateMLModelResponse Core.Int
umlmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umlmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
