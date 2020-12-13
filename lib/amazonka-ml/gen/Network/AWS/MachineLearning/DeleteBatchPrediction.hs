{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteBatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @BatchPrediction@ , rendering it unusable.
--
-- After using the @DeleteBatchPrediction@ operation, you can use the 'GetBatchPrediction' operation to verify that the status of the @BatchPrediction@ changed to DELETED.
-- __Caution:__ The result of the @DeleteBatchPrediction@ operation is irreversible.
module Network.AWS.MachineLearning.DeleteBatchPrediction
  ( -- * Creating a request
    DeleteBatchPrediction (..),
    mkDeleteBatchPrediction,

    -- ** Request lenses
    dbpBatchPredictionId,

    -- * Destructuring the response
    DeleteBatchPredictionResponse (..),
    mkDeleteBatchPredictionResponse,

    -- ** Response lenses
    dbpfrsBatchPredictionId,
    dbpfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBatchPrediction' smart constructor.
newtype DeleteBatchPrediction = DeleteBatchPrediction'
  { -- | A user-supplied ID that uniquely identifies the @BatchPrediction@ .
    batchPredictionId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBatchPrediction' with the minimum fields required to make a request.
--
-- * 'batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ .
mkDeleteBatchPrediction ::
  -- | 'batchPredictionId'
  Lude.Text ->
  DeleteBatchPrediction
mkDeleteBatchPrediction pBatchPredictionId_ =
  DeleteBatchPrediction' {batchPredictionId = pBatchPredictionId_}

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ .
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpBatchPredictionId :: Lens.Lens' DeleteBatchPrediction Lude.Text
dbpBatchPredictionId = Lens.lens (batchPredictionId :: DeleteBatchPrediction -> Lude.Text) (\s a -> s {batchPredictionId = a} :: DeleteBatchPrediction)
{-# DEPRECATED dbpBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

instance Lude.AWSRequest DeleteBatchPrediction where
  type Rs DeleteBatchPrediction = DeleteBatchPredictionResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBatchPredictionResponse'
            Lude.<$> (x Lude..?> "BatchPredictionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBatchPrediction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.DeleteBatchPrediction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBatchPrediction where
  toJSON DeleteBatchPrediction' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("BatchPredictionId" Lude..= batchPredictionId)]
      )

instance Lude.ToPath DeleteBatchPrediction where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBatchPrediction where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DeleteBatchPrediction@ operation.
--
-- You can use the @GetBatchPrediction@ operation and check the value of the @Status@ parameter to see whether a @BatchPrediction@ is marked as @DELETED@ .
--
-- /See:/ 'mkDeleteBatchPredictionResponse' smart constructor.
data DeleteBatchPredictionResponse = DeleteBatchPredictionResponse'
  { -- | A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value should be identical to the value of the @BatchPredictionID@ in the request.
    batchPredictionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBatchPredictionResponse' with the minimum fields required to make a request.
--
-- * 'batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value should be identical to the value of the @BatchPredictionID@ in the request.
-- * 'responseStatus' - The response status code.
mkDeleteBatchPredictionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBatchPredictionResponse
mkDeleteBatchPredictionResponse pResponseStatus_ =
  DeleteBatchPredictionResponse'
    { batchPredictionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value should be identical to the value of the @BatchPredictionID@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpfrsBatchPredictionId :: Lens.Lens' DeleteBatchPredictionResponse (Lude.Maybe Lude.Text)
dbpfrsBatchPredictionId = Lens.lens (batchPredictionId :: DeleteBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionId = a} :: DeleteBatchPredictionResponse)
{-# DEPRECATED dbpfrsBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpfrsResponseStatus :: Lens.Lens' DeleteBatchPredictionResponse Lude.Int
dbpfrsResponseStatus = Lens.lens (responseStatus :: DeleteBatchPredictionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBatchPredictionResponse)
{-# DEPRECATED dbpfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
