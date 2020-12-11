{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.CreateBatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates predictions for a group of observations. The observations to process exist in one or more data files referenced by a @DataSource@ . This operation creates a new @BatchPrediction@ , and uses an @MLModel@ and the data files referenced by the @DataSource@ as information sources.
--
-- @CreateBatchPrediction@ is an asynchronous operation. In response to @CreateBatchPrediction@ , Amazon Machine Learning (Amazon ML) immediately returns and sets the @BatchPrediction@ status to @PENDING@ . After the @BatchPrediction@ completes, Amazon ML sets the status to @COMPLETED@ .
-- You can poll for status updates by using the 'GetBatchPrediction' operation and checking the @Status@ parameter of the result. After the @COMPLETED@ status appears, the results are available in the location specified by the @OutputUri@ parameter.
module Network.AWS.MachineLearning.CreateBatchPrediction
  ( -- * Creating a request
    CreateBatchPrediction (..),
    mkCreateBatchPrediction,

    -- ** Request lenses
    cbpBatchPredictionName,
    cbpBatchPredictionId,
    cbpMLModelId,
    cbpBatchPredictionDataSourceId,
    cbpOutputURI,

    -- * Destructuring the response
    CreateBatchPredictionResponse (..),
    mkCreateBatchPredictionResponse,

    -- ** Response lenses
    cbprsBatchPredictionId,
    cbprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBatchPrediction' smart constructor.
data CreateBatchPrediction = CreateBatchPrediction'
  { batchPredictionName ::
      Lude.Maybe Lude.Text,
    batchPredictionId :: Lude.Text,
    mLModelId :: Lude.Text,
    batchPredictionDataSourceId :: Lude.Text,
    outputURI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBatchPrediction' with the minimum fields required to make a request.
--
-- * 'batchPredictionDataSourceId' - The ID of the @DataSource@ that points to the group of observations to predict.
-- * 'batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ .
-- * 'batchPredictionName' - A user-supplied name or description of the @BatchPrediction@ . @BatchPredictionName@ can only use the UTF-8 character set.
-- * 'mLModelId' - The ID of the @MLModel@ that will generate predictions for the group of observations.
-- * 'outputURI' - The location of an Amazon Simple Storage Service (Amazon S3) bucket or directory to store the batch prediction results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'.
--
-- Amazon ML needs permissions to store and retrieve the logs on your behalf. For information about how to set permissions, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
mkCreateBatchPrediction ::
  -- | 'batchPredictionId'
  Lude.Text ->
  -- | 'mLModelId'
  Lude.Text ->
  -- | 'batchPredictionDataSourceId'
  Lude.Text ->
  -- | 'outputURI'
  Lude.Text ->
  CreateBatchPrediction
mkCreateBatchPrediction
  pBatchPredictionId_
  pMLModelId_
  pBatchPredictionDataSourceId_
  pOutputURI_ =
    CreateBatchPrediction'
      { batchPredictionName = Lude.Nothing,
        batchPredictionId = pBatchPredictionId_,
        mLModelId = pMLModelId_,
        batchPredictionDataSourceId = pBatchPredictionDataSourceId_,
        outputURI = pOutputURI_
      }

-- | A user-supplied name or description of the @BatchPrediction@ . @BatchPredictionName@ can only use the UTF-8 character set.
--
-- /Note:/ Consider using 'batchPredictionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpBatchPredictionName :: Lens.Lens' CreateBatchPrediction (Lude.Maybe Lude.Text)
cbpBatchPredictionName = Lens.lens (batchPredictionName :: CreateBatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionName = a} :: CreateBatchPrediction)
{-# DEPRECATED cbpBatchPredictionName "Use generic-lens or generic-optics with 'batchPredictionName' instead." #-}

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ .
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpBatchPredictionId :: Lens.Lens' CreateBatchPrediction Lude.Text
cbpBatchPredictionId = Lens.lens (batchPredictionId :: CreateBatchPrediction -> Lude.Text) (\s a -> s {batchPredictionId = a} :: CreateBatchPrediction)
{-# DEPRECATED cbpBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

-- | The ID of the @MLModel@ that will generate predictions for the group of observations.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpMLModelId :: Lens.Lens' CreateBatchPrediction Lude.Text
cbpMLModelId = Lens.lens (mLModelId :: CreateBatchPrediction -> Lude.Text) (\s a -> s {mLModelId = a} :: CreateBatchPrediction)
{-# DEPRECATED cbpMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The ID of the @DataSource@ that points to the group of observations to predict.
--
-- /Note:/ Consider using 'batchPredictionDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpBatchPredictionDataSourceId :: Lens.Lens' CreateBatchPrediction Lude.Text
cbpBatchPredictionDataSourceId = Lens.lens (batchPredictionDataSourceId :: CreateBatchPrediction -> Lude.Text) (\s a -> s {batchPredictionDataSourceId = a} :: CreateBatchPrediction)
{-# DEPRECATED cbpBatchPredictionDataSourceId "Use generic-lens or generic-optics with 'batchPredictionDataSourceId' instead." #-}

-- | The location of an Amazon Simple Storage Service (Amazon S3) bucket or directory to store the batch prediction results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'.
--
-- Amazon ML needs permissions to store and retrieve the logs on your behalf. For information about how to set permissions, see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
--
-- /Note:/ Consider using 'outputURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbpOutputURI :: Lens.Lens' CreateBatchPrediction Lude.Text
cbpOutputURI = Lens.lens (outputURI :: CreateBatchPrediction -> Lude.Text) (\s a -> s {outputURI = a} :: CreateBatchPrediction)
{-# DEPRECATED cbpOutputURI "Use generic-lens or generic-optics with 'outputURI' instead." #-}

instance Lude.AWSRequest CreateBatchPrediction where
  type Rs CreateBatchPrediction = CreateBatchPredictionResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBatchPredictionResponse'
            Lude.<$> (x Lude..?> "BatchPredictionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBatchPrediction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.CreateBatchPrediction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBatchPrediction where
  toJSON CreateBatchPrediction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BatchPredictionName" Lude..=) Lude.<$> batchPredictionName,
            Lude.Just ("BatchPredictionId" Lude..= batchPredictionId),
            Lude.Just ("MLModelId" Lude..= mLModelId),
            Lude.Just
              ( "BatchPredictionDataSourceId"
                  Lude..= batchPredictionDataSourceId
              ),
            Lude.Just ("OutputUri" Lude..= outputURI)
          ]
      )

instance Lude.ToPath CreateBatchPrediction where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBatchPrediction where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @CreateBatchPrediction@ operation, and is an acknowledgement that Amazon ML received the request.
--
-- The @CreateBatchPrediction@ operation is asynchronous. You can poll for status updates by using the @>GetBatchPrediction@ operation and checking the @Status@ parameter of the result.
--
-- /See:/ 'mkCreateBatchPredictionResponse' smart constructor.
data CreateBatchPredictionResponse = CreateBatchPredictionResponse'
  { batchPredictionId ::
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

-- | Creates a value of 'CreateBatchPredictionResponse' with the minimum fields required to make a request.
--
-- * 'batchPredictionId' - A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value is identical to the value of the @BatchPredictionId@ in the request.
-- * 'responseStatus' - The response status code.
mkCreateBatchPredictionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBatchPredictionResponse
mkCreateBatchPredictionResponse pResponseStatus_ =
  CreateBatchPredictionResponse'
    { batchPredictionId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value is identical to the value of the @BatchPredictionId@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbprsBatchPredictionId :: Lens.Lens' CreateBatchPredictionResponse (Lude.Maybe Lude.Text)
cbprsBatchPredictionId = Lens.lens (batchPredictionId :: CreateBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionId = a} :: CreateBatchPredictionResponse)
{-# DEPRECATED cbprsBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbprsResponseStatus :: Lens.Lens' CreateBatchPredictionResponse Lude.Int
cbprsResponseStatus = Lens.lens (responseStatus :: CreateBatchPredictionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBatchPredictionResponse)
{-# DEPRECATED cbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
