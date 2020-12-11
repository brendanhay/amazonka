{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetBatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @BatchPrediction@ that includes detailed metadata, status, and data file information for a @Batch Prediction@ request.
module Network.AWS.MachineLearning.GetBatchPrediction
  ( -- * Creating a request
    GetBatchPrediction (..),
    mkGetBatchPrediction,

    -- ** Request lenses
    gbpBatchPredictionId,

    -- * Destructuring the response
    GetBatchPredictionResponse (..),
    mkGetBatchPredictionResponse,

    -- ** Response lenses
    gbprsStatus,
    gbprsLastUpdatedAt,
    gbprsCreatedAt,
    gbprsComputeTime,
    gbprsInputDataLocationS3,
    gbprsMLModelId,
    gbprsBatchPredictionDataSourceId,
    gbprsTotalRecordCount,
    gbprsStartedAt,
    gbprsBatchPredictionId,
    gbprsFinishedAt,
    gbprsInvalidRecordCount,
    gbprsCreatedByIAMUser,
    gbprsName,
    gbprsLogURI,
    gbprsMessage,
    gbprsOutputURI,
    gbprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetBatchPrediction' smart constructor.
newtype GetBatchPrediction = GetBatchPrediction'
  { batchPredictionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBatchPrediction' with the minimum fields required to make a request.
--
-- * 'batchPredictionId' - An ID assigned to the @BatchPrediction@ at creation.
mkGetBatchPrediction ::
  -- | 'batchPredictionId'
  Lude.Text ->
  GetBatchPrediction
mkGetBatchPrediction pBatchPredictionId_ =
  GetBatchPrediction' {batchPredictionId = pBatchPredictionId_}

-- | An ID assigned to the @BatchPrediction@ at creation.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpBatchPredictionId :: Lens.Lens' GetBatchPrediction Lude.Text
gbpBatchPredictionId = Lens.lens (batchPredictionId :: GetBatchPrediction -> Lude.Text) (\s a -> s {batchPredictionId = a} :: GetBatchPrediction)
{-# DEPRECATED gbpBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

instance Lude.AWSRequest GetBatchPrediction where
  type Rs GetBatchPrediction = GetBatchPredictionResponse
  request = Req.postJSON machineLearningService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetBatchPredictionResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "LastUpdatedAt")
            Lude.<*> (x Lude..?> "CreatedAt")
            Lude.<*> (x Lude..?> "ComputeTime")
            Lude.<*> (x Lude..?> "InputDataLocationS3")
            Lude.<*> (x Lude..?> "MLModelId")
            Lude.<*> (x Lude..?> "BatchPredictionDataSourceId")
            Lude.<*> (x Lude..?> "TotalRecordCount")
            Lude.<*> (x Lude..?> "StartedAt")
            Lude.<*> (x Lude..?> "BatchPredictionId")
            Lude.<*> (x Lude..?> "FinishedAt")
            Lude.<*> (x Lude..?> "InvalidRecordCount")
            Lude.<*> (x Lude..?> "CreatedByIamUser")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "LogUri")
            Lude.<*> (x Lude..?> "Message")
            Lude.<*> (x Lude..?> "OutputUri")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBatchPrediction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonML_20141212.GetBatchPrediction" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetBatchPrediction where
  toJSON GetBatchPrediction' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("BatchPredictionId" Lude..= batchPredictionId)]
      )

instance Lude.ToPath GetBatchPrediction where
  toPath = Lude.const "/"

instance Lude.ToQuery GetBatchPrediction where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @GetBatchPrediction@ operation and describes a @BatchPrediction@ .
--
-- /See:/ 'mkGetBatchPredictionResponse' smart constructor.
data GetBatchPredictionResponse = GetBatchPredictionResponse'
  { status ::
      Lude.Maybe EntityStatus,
    lastUpdatedAt ::
      Lude.Maybe Lude.Timestamp,
    createdAt ::
      Lude.Maybe Lude.Timestamp,
    computeTime ::
      Lude.Maybe Lude.Integer,
    inputDataLocationS3 ::
      Lude.Maybe Lude.Text,
    mLModelId :: Lude.Maybe Lude.Text,
    batchPredictionDataSourceId ::
      Lude.Maybe Lude.Text,
    totalRecordCount ::
      Lude.Maybe Lude.Integer,
    startedAt ::
      Lude.Maybe Lude.Timestamp,
    batchPredictionId ::
      Lude.Maybe Lude.Text,
    finishedAt ::
      Lude.Maybe Lude.Timestamp,
    invalidRecordCount ::
      Lude.Maybe Lude.Integer,
    createdByIAMUser ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    logURI :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    outputURI :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetBatchPredictionResponse' with the minimum fields required to make a request.
--
-- * 'batchPredictionDataSourceId' - The ID of the @DataSource@ that was used to create the @BatchPrediction@ .
-- * 'batchPredictionId' - An ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request.
-- * 'computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @BatchPrediction@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @BatchPrediction@ is in the @COMPLETED@ state.
-- * 'createdAt' - The time when the @BatchPrediction@ was created. The time is expressed in epoch time.
-- * 'createdByIAMUser' - The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'finishedAt' - The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
-- * 'inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
-- * 'invalidRecordCount' - The number of invalid records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
-- * 'lastUpdatedAt' - The time of the most recent edit to @BatchPrediction@ . The time is expressed in epoch time.
-- * 'logURI' - A link to the file that contains logs of the @CreateBatchPrediction@ operation.
-- * 'mLModelId' - The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
-- * 'message' - A description of the most recent details about processing the batch prediction request.
-- * 'name' - A user-supplied name or description of the @BatchPrediction@ .
-- * 'outputURI' - The location of an Amazon S3 bucket or directory to receive the operation results.
-- * 'responseStatus' - The response status code.
-- * 'startedAt' - The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @INPROGRESS@ . @StartedAt@ isn't available if the @BatchPrediction@ is in the @PENDING@ state.
-- * 'status' - The status of the @BatchPrediction@ , which can be one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to generate batch predictions.
--
--     * @INPROGRESS@ - The batch predictions are in progress.
--
--     * @FAILED@ - The request to perform a batch prediction did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The batch prediction process completed successfully.
--
--     * @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not usable.
--
-- * 'totalRecordCount' - The number of total records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
mkGetBatchPredictionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBatchPredictionResponse
mkGetBatchPredictionResponse pResponseStatus_ =
  GetBatchPredictionResponse'
    { status = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      computeTime = Lude.Nothing,
      inputDataLocationS3 = Lude.Nothing,
      mLModelId = Lude.Nothing,
      batchPredictionDataSourceId = Lude.Nothing,
      totalRecordCount = Lude.Nothing,
      startedAt = Lude.Nothing,
      batchPredictionId = Lude.Nothing,
      finishedAt = Lude.Nothing,
      invalidRecordCount = Lude.Nothing,
      createdByIAMUser = Lude.Nothing,
      name = Lude.Nothing,
      logURI = Lude.Nothing,
      message = Lude.Nothing,
      outputURI = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the @BatchPrediction@ , which can be one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to generate batch predictions.
--
--     * @INPROGRESS@ - The batch predictions are in progress.
--
--     * @FAILED@ - The request to perform a batch prediction did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The batch prediction process completed successfully.
--
--     * @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsStatus :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe EntityStatus)
gbprsStatus = Lens.lens (status :: GetBatchPredictionResponse -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time of the most recent edit to @BatchPrediction@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsLastUpdatedAt :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Timestamp)
gbprsLastUpdatedAt = Lens.lens (lastUpdatedAt :: GetBatchPredictionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time when the @BatchPrediction@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsCreatedAt :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Timestamp)
gbprsCreatedAt = Lens.lens (createdAt :: GetBatchPredictionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @BatchPrediction@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @BatchPrediction@ is in the @COMPLETED@ state.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsComputeTime :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Integer)
gbprsComputeTime = Lens.lens (computeTime :: GetBatchPredictionResponse -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsInputDataLocationS3 :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsInputDataLocationS3 = Lens.lens (inputDataLocationS3 :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {inputDataLocationS3 = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsMLModelId :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsMLModelId = Lens.lens (mLModelId :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The ID of the @DataSource@ that was used to create the @BatchPrediction@ .
--
-- /Note:/ Consider using 'batchPredictionDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsBatchPredictionDataSourceId :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsBatchPredictionDataSourceId = Lens.lens (batchPredictionDataSourceId :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionDataSourceId = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsBatchPredictionDataSourceId "Use generic-lens or generic-optics with 'batchPredictionDataSourceId' instead." #-}

-- | The number of total records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
--
-- /Note:/ Consider using 'totalRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsTotalRecordCount :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Integer)
gbprsTotalRecordCount = Lens.lens (totalRecordCount :: GetBatchPredictionResponse -> Lude.Maybe Lude.Integer) (\s a -> s {totalRecordCount = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsTotalRecordCount "Use generic-lens or generic-optics with 'totalRecordCount' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @INPROGRESS@ . @StartedAt@ isn't available if the @BatchPrediction@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsStartedAt :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Timestamp)
gbprsStartedAt = Lens.lens (startedAt :: GetBatchPredictionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | An ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsBatchPredictionId :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsBatchPredictionId = Lens.lens (batchPredictionId :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionId = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsFinishedAt :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Timestamp)
gbprsFinishedAt = Lens.lens (finishedAt :: GetBatchPredictionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The number of invalid records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
--
-- /Note:/ Consider using 'invalidRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsInvalidRecordCount :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Integer)
gbprsInvalidRecordCount = Lens.lens (invalidRecordCount :: GetBatchPredictionResponse -> Lude.Maybe Lude.Integer) (\s a -> s {invalidRecordCount = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsInvalidRecordCount "Use generic-lens or generic-optics with 'invalidRecordCount' instead." #-}

-- | The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsCreatedByIAMUser :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsCreatedByIAMUser = Lens.lens (createdByIAMUser :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @BatchPrediction@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsName :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsName = Lens.lens (name :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A link to the file that contains logs of the @CreateBatchPrediction@ operation.
--
-- /Note:/ Consider using 'logURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsLogURI :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsLogURI = Lens.lens (logURI :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {logURI = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsLogURI "Use generic-lens or generic-optics with 'logURI' instead." #-}

-- | A description of the most recent details about processing the batch prediction request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsMessage :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsMessage = Lens.lens (message :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The location of an Amazon S3 bucket or directory to receive the operation results.
--
-- /Note:/ Consider using 'outputURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsOutputURI :: Lens.Lens' GetBatchPredictionResponse (Lude.Maybe Lude.Text)
gbprsOutputURI = Lens.lens (outputURI :: GetBatchPredictionResponse -> Lude.Maybe Lude.Text) (\s a -> s {outputURI = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsOutputURI "Use generic-lens or generic-optics with 'outputURI' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprsResponseStatus :: Lens.Lens' GetBatchPredictionResponse Lude.Int
gbprsResponseStatus = Lens.lens (responseStatus :: GetBatchPredictionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBatchPredictionResponse)
{-# DEPRECATED gbprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
