{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Predict
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a prediction for the observation using the specified @ML Model@ .
module Network.AWS.MachineLearning.Predict
  ( -- * Creating a request
    Predict (..),
    mkPredict,

    -- ** Request lenses
    pMLModelId,
    pRecord,
    pPredictEndpoint,

    -- * Destructuring the response
    PredictResponse (..),
    mkPredictResponse,

    -- ** Response lenses
    prsPrediction,
    prsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPredict' smart constructor.
data Predict = Predict'
  { mLModelId :: Lude.Text,
    record :: Lude.HashMap Lude.Text (Lude.Text),
    predictEndpoint :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Predict' with the minimum fields required to make a request.
--
-- * 'mLModelId' - A unique identifier of the @MLModel@ .
-- * 'predictEndpoint' - Undocumented field.
-- * 'record' - Undocumented field.
mkPredict ::
  -- | 'mLModelId'
  Lude.Text ->
  -- | 'predictEndpoint'
  Lude.Text ->
  Predict
mkPredict pMLModelId_ pPredictEndpoint_ =
  Predict'
    { mLModelId = pMLModelId_,
      record = Lude.mempty,
      predictEndpoint = pPredictEndpoint_
    }

-- | A unique identifier of the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMLModelId :: Lens.Lens' Predict Lude.Text
pMLModelId = Lens.lens (mLModelId :: Predict -> Lude.Text) (\s a -> s {mLModelId = a} :: Predict)
{-# DEPRECATED pMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'record' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRecord :: Lens.Lens' Predict (Lude.HashMap Lude.Text (Lude.Text))
pRecord = Lens.lens (record :: Predict -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {record = a} :: Predict)
{-# DEPRECATED pRecord "Use generic-lens or generic-optics with 'record' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'predictEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPredictEndpoint :: Lens.Lens' Predict Lude.Text
pPredictEndpoint = Lens.lens (predictEndpoint :: Predict -> Lude.Text) (\s a -> s {predictEndpoint = a} :: Predict)
{-# DEPRECATED pPredictEndpoint "Use generic-lens or generic-optics with 'predictEndpoint' instead." #-}

instance Lude.AWSRequest Predict where
  type Rs Predict = PredictResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          PredictResponse'
            Lude.<$> (x Lude..?> "Prediction") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders Predict where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.Predict" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON Predict where
  toJSON Predict' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MLModelId" Lude..= mLModelId),
            Lude.Just ("Record" Lude..= record),
            Lude.Just ("PredictEndpoint" Lude..= predictEndpoint)
          ]
      )

instance Lude.ToPath Predict where
  toPath = Lude.const "/"

instance Lude.ToQuery Predict where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPredictResponse' smart constructor.
data PredictResponse = PredictResponse'
  { prediction ::
      Lude.Maybe Prediction,
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

-- | Creates a value of 'PredictResponse' with the minimum fields required to make a request.
--
-- * 'prediction' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkPredictResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PredictResponse
mkPredictResponse pResponseStatus_ =
  PredictResponse'
    { prediction = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'prediction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsPrediction :: Lens.Lens' PredictResponse (Lude.Maybe Prediction)
prsPrediction = Lens.lens (prediction :: PredictResponse -> Lude.Maybe Prediction) (\s a -> s {prediction = a} :: PredictResponse)
{-# DEPRECATED prsPrediction "Use generic-lens or generic-optics with 'prediction' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prsResponseStatus :: Lens.Lens' PredictResponse Lude.Int
prsResponseStatus = Lens.lens (responseStatus :: PredictResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PredictResponse)
{-# DEPRECATED prsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
