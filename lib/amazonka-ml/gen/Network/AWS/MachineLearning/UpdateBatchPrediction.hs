{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateBatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @BatchPredictionName@ of a @BatchPrediction@ .
--
-- You can use the @GetBatchPrediction@ operation to view the contents of the updated data element.
module Network.AWS.MachineLearning.UpdateBatchPrediction
  ( -- * Creating a request
    UpdateBatchPrediction (..),
    mkUpdateBatchPrediction,

    -- ** Request lenses
    ubpBatchPredictionName,
    ubpBatchPredictionId,

    -- * Destructuring the response
    UpdateBatchPredictionResponse (..),
    mkUpdateBatchPredictionResponse,

    -- ** Response lenses
    ubprsBatchPredictionId,
    ubprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateBatchPrediction' smart constructor.
data UpdateBatchPrediction = UpdateBatchPrediction'
  { -- | A new user-supplied name or description of the @BatchPrediction@ .
    batchPredictionName :: Lude.Text,
    -- | The ID assigned to the @BatchPrediction@ during creation.
    batchPredictionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBatchPrediction' with the minimum fields required to make a request.
--
-- * 'batchPredictionName' - A new user-supplied name or description of the @BatchPrediction@ .
-- * 'batchPredictionId' - The ID assigned to the @BatchPrediction@ during creation.
mkUpdateBatchPrediction ::
  -- | 'batchPredictionName'
  Lude.Text ->
  -- | 'batchPredictionId'
  Lude.Text ->
  UpdateBatchPrediction
mkUpdateBatchPrediction pBatchPredictionName_ pBatchPredictionId_ =
  UpdateBatchPrediction'
    { batchPredictionName =
        pBatchPredictionName_,
      batchPredictionId = pBatchPredictionId_
    }

-- | A new user-supplied name or description of the @BatchPrediction@ .
--
-- /Note:/ Consider using 'batchPredictionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpBatchPredictionName :: Lens.Lens' UpdateBatchPrediction Lude.Text
ubpBatchPredictionName = Lens.lens (batchPredictionName :: UpdateBatchPrediction -> Lude.Text) (\s a -> s {batchPredictionName = a} :: UpdateBatchPrediction)
{-# DEPRECATED ubpBatchPredictionName "Use generic-lens or generic-optics with 'batchPredictionName' instead." #-}

-- | The ID assigned to the @BatchPrediction@ during creation.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpBatchPredictionId :: Lens.Lens' UpdateBatchPrediction Lude.Text
ubpBatchPredictionId = Lens.lens (batchPredictionId :: UpdateBatchPrediction -> Lude.Text) (\s a -> s {batchPredictionId = a} :: UpdateBatchPrediction)
{-# DEPRECATED ubpBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

instance Lude.AWSRequest UpdateBatchPrediction where
  type Rs UpdateBatchPrediction = UpdateBatchPredictionResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateBatchPredictionResponse'
            Lude.<$> (x Lude..?> "BatchPredictionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBatchPrediction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.UpdateBatchPrediction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBatchPrediction where
  toJSON UpdateBatchPrediction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BatchPredictionName" Lude..= batchPredictionName),
            Lude.Just ("BatchPredictionId" Lude..= batchPredictionId)
          ]
      )

instance Lude.ToPath UpdateBatchPrediction where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateBatchPrediction where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an @UpdateBatchPrediction@ operation.
--
-- You can see the updated content by using the @GetBatchPrediction@ operation.
--
-- /See:/ 'mkUpdateBatchPredictionResponse' smart constructor.
data UpdateBatchPredictionResponse = UpdateBatchPredictionResponse'
  { -- | The ID assigned to the @BatchPrediction@ during creation. This value should be identical to the value of the @BatchPredictionId@ in the request.
    batchPredictionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBatchPredictionResponse' with the minimum fields required to make a request.
--
-- * 'batchPredictionId' - The ID assigned to the @BatchPrediction@ during creation. This value should be identical to the value of the @BatchPredictionId@ in the request.
-- * 'responseStatus' - The response status code.
mkUpdateBatchPredictionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBatchPredictionResponse
mkUpdateBatchPredictionResponse pResponseStatus_ =
  UpdateBatchPredictionResponse'
    { batchPredictionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID assigned to the @BatchPrediction@ during creation. This value should be identical to the value of the @BatchPredictionId@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubprsBatchPredictionId :: Lens.Lens' UpdateBatchPredictionResponse (Lude.Maybe Lude.Text)
ubprsBatchPredictionId = Lens.lens (batchPredictionId :: UpdateBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionId = a} :: UpdateBatchPredictionResponse)
{-# DEPRECATED ubprsBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubprsResponseStatus :: Lens.Lens' UpdateBatchPredictionResponse Lude.Int
ubprsResponseStatus = Lens.lens (responseStatus :: UpdateBatchPredictionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBatchPredictionResponse)
{-# DEPRECATED ubprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
