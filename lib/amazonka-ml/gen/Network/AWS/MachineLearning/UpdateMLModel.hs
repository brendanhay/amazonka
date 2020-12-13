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
    umlmrsMLModelId,
    umlmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateMLModel' smart constructor.
data UpdateMLModel = UpdateMLModel'
  { -- | The ID assigned to the @MLModel@ during creation.
    mLModelId :: Lude.Text,
    -- | A user-supplied name or description of the @MLModel@ .
    mLModelName :: Lude.Maybe Lude.Text,
    -- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks the boundary between a positive prediction and a negative prediction.
    --
    -- Output values greater than or equal to the @ScoreThreshold@ receive a positive result from the @MLModel@ , such as @true@ . Output values less than the @ScoreThreshold@ receive a negative response from the @MLModel@ , such as @false@ .
    scoreThreshold :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMLModel' with the minimum fields required to make a request.
--
-- * 'mLModelId' - The ID assigned to the @MLModel@ during creation.
-- * 'mLModelName' - A user-supplied name or description of the @MLModel@ .
-- * 'scoreThreshold' - The @ScoreThreshold@ used in binary classification @MLModel@ that marks the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a positive result from the @MLModel@ , such as @true@ . Output values less than the @ScoreThreshold@ receive a negative response from the @MLModel@ , such as @false@ .
mkUpdateMLModel ::
  -- | 'mLModelId'
  Lude.Text ->
  UpdateMLModel
mkUpdateMLModel pMLModelId_ =
  UpdateMLModel'
    { mLModelId = pMLModelId_,
      mLModelName = Lude.Nothing,
      scoreThreshold = Lude.Nothing
    }

-- | The ID assigned to the @MLModel@ during creation.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmMLModelId :: Lens.Lens' UpdateMLModel Lude.Text
umlmMLModelId = Lens.lens (mLModelId :: UpdateMLModel -> Lude.Text) (\s a -> s {mLModelId = a} :: UpdateMLModel)
{-# DEPRECATED umlmMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | A user-supplied name or description of the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmMLModelName :: Lens.Lens' UpdateMLModel (Lude.Maybe Lude.Text)
umlmMLModelName = Lens.lens (mLModelName :: UpdateMLModel -> Lude.Maybe Lude.Text) (\s a -> s {mLModelName = a} :: UpdateMLModel)
{-# DEPRECATED umlmMLModelName "Use generic-lens or generic-optics with 'mLModelName' instead." #-}

-- | The @ScoreThreshold@ used in binary classification @MLModel@ that marks the boundary between a positive prediction and a negative prediction.
--
-- Output values greater than or equal to the @ScoreThreshold@ receive a positive result from the @MLModel@ , such as @true@ . Output values less than the @ScoreThreshold@ receive a negative response from the @MLModel@ , such as @false@ .
--
-- /Note:/ Consider using 'scoreThreshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmScoreThreshold :: Lens.Lens' UpdateMLModel (Lude.Maybe Lude.Double)
umlmScoreThreshold = Lens.lens (scoreThreshold :: UpdateMLModel -> Lude.Maybe Lude.Double) (\s a -> s {scoreThreshold = a} :: UpdateMLModel)
{-# DEPRECATED umlmScoreThreshold "Use generic-lens or generic-optics with 'scoreThreshold' instead." #-}

instance Lude.AWSRequest UpdateMLModel where
  type Rs UpdateMLModel = UpdateMLModelResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMLModelResponse'
            Lude.<$> (x Lude..?> "MLModelId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMLModel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.UpdateMLModel" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMLModel where
  toJSON UpdateMLModel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MLModelId" Lude..= mLModelId),
            ("MLModelName" Lude..=) Lude.<$> mLModelName,
            ("ScoreThreshold" Lude..=) Lude.<$> scoreThreshold
          ]
      )

instance Lude.ToPath UpdateMLModel where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMLModel where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @UpdateMLModel@ operation.
--
-- You can see the updated content by using the @GetMLModel@ operation.
--
-- /See:/ 'mkUpdateMLModelResponse' smart constructor.
data UpdateMLModelResponse = UpdateMLModelResponse'
  { -- | The ID assigned to the @MLModel@ during creation. This value should be identical to the value of the @MLModelID@ in the request.
    mLModelId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMLModelResponse' with the minimum fields required to make a request.
--
-- * 'mLModelId' - The ID assigned to the @MLModel@ during creation. This value should be identical to the value of the @MLModelID@ in the request.
-- * 'responseStatus' - The response status code.
mkUpdateMLModelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMLModelResponse
mkUpdateMLModelResponse pResponseStatus_ =
  UpdateMLModelResponse'
    { mLModelId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID assigned to the @MLModel@ during creation. This value should be identical to the value of the @MLModelID@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmrsMLModelId :: Lens.Lens' UpdateMLModelResponse (Lude.Maybe Lude.Text)
umlmrsMLModelId = Lens.lens (mLModelId :: UpdateMLModelResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: UpdateMLModelResponse)
{-# DEPRECATED umlmrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umlmrsResponseStatus :: Lens.Lens' UpdateMLModelResponse Lude.Int
umlmrsResponseStatus = Lens.lens (responseStatus :: UpdateMLModelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMLModelResponse)
{-# DEPRECATED umlmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
