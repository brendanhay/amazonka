{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.BatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.BatchPrediction
  ( BatchPrediction (..),

    -- * Smart constructor
    mkBatchPrediction,

    -- * Lenses
    bpStatus,
    bpLastUpdatedAt,
    bpCreatedAt,
    bpComputeTime,
    bpInputDataLocationS3,
    bpMLModelId,
    bpBatchPredictionDataSourceId,
    bpTotalRecordCount,
    bpStartedAt,
    bpBatchPredictionId,
    bpFinishedAt,
    bpInvalidRecordCount,
    bpCreatedByIAMUser,
    bpName,
    bpMessage,
    bpOutputURI,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a @GetBatchPrediction@ operation.
--
-- The content consists of the detailed metadata, the status, and the data file information of a @Batch Prediction@ .
--
-- /See:/ 'mkBatchPrediction' smart constructor.
data BatchPrediction = BatchPrediction'
  { status ::
      Lude.Maybe EntityStatus,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    createdAt :: Lude.Maybe Lude.Timestamp,
    computeTime :: Lude.Maybe Lude.Integer,
    inputDataLocationS3 :: Lude.Maybe Lude.Text,
    mLModelId :: Lude.Maybe Lude.Text,
    batchPredictionDataSourceId :: Lude.Maybe Lude.Text,
    totalRecordCount :: Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Timestamp,
    batchPredictionId :: Lude.Maybe Lude.Text,
    finishedAt :: Lude.Maybe Lude.Timestamp,
    invalidRecordCount :: Lude.Maybe Lude.Integer,
    createdByIAMUser :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text,
    outputURI :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPrediction' with the minimum fields required to make a request.
--
-- * 'batchPredictionDataSourceId' - The ID of the @DataSource@ that points to the group of observations to predict.
-- * 'batchPredictionId' - The ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request.
-- * 'computeTime' - Undocumented field.
-- * 'createdAt' - The time that the @BatchPrediction@ was created. The time is expressed in epoch time.
-- * 'createdByIAMUser' - The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
-- * 'finishedAt' - Undocumented field.
-- * 'inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
-- * 'invalidRecordCount' - Undocumented field.
-- * 'lastUpdatedAt' - The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
-- * 'mLModelId' - The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
-- * 'message' - A description of the most recent details about processing the batch prediction request.
-- * 'name' - A user-supplied name or description of the @BatchPrediction@ .
-- * 'outputURI' - The location of an Amazon S3 bucket or directory to receive the operation results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'.
-- * 'startedAt' - Undocumented field.
-- * 'status' - The status of the @BatchPrediction@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to generate predictions for a batch of observations.
--
--     * @INPROGRESS@ - The process is underway.
--
--     * @FAILED@ - The request to perform a batch prediction did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The batch prediction process completed successfully.
--
--     * @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not usable.
--
-- * 'totalRecordCount' - Undocumented field.
mkBatchPrediction ::
  BatchPrediction
mkBatchPrediction =
  BatchPrediction'
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
      message = Lude.Nothing,
      outputURI = Lude.Nothing
    }

-- | The status of the @BatchPrediction@ . This element can have one of the following values:
--
--
--     * @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request to generate predictions for a batch of observations.
--
--     * @INPROGRESS@ - The process is underway.
--
--     * @FAILED@ - The request to perform a batch prediction did not run to completion. It is not usable.
--
--     * @COMPLETED@ - The batch prediction process completed successfully.
--
--     * @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not usable.
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpStatus :: Lens.Lens' BatchPrediction (Lude.Maybe EntityStatus)
bpStatus = Lens.lens (status :: BatchPrediction -> Lude.Maybe EntityStatus) (\s a -> s {status = a} :: BatchPrediction)
{-# DEPRECATED bpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpLastUpdatedAt :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Timestamp)
bpLastUpdatedAt = Lens.lens (lastUpdatedAt :: BatchPrediction -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: BatchPrediction)
{-# DEPRECATED bpLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time that the @BatchPrediction@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpCreatedAt :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Timestamp)
bpCreatedAt = Lens.lens (createdAt :: BatchPrediction -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: BatchPrediction)
{-# DEPRECATED bpCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpComputeTime :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Integer)
bpComputeTime = Lens.lens (computeTime :: BatchPrediction -> Lude.Maybe Lude.Integer) (\s a -> s {computeTime = a} :: BatchPrediction)
{-# DEPRECATED bpComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpInputDataLocationS3 :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpInputDataLocationS3 = Lens.lens (inputDataLocationS3 :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {inputDataLocationS3 = a} :: BatchPrediction)
{-# DEPRECATED bpInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpMLModelId :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpMLModelId = Lens.lens (mLModelId :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {mLModelId = a} :: BatchPrediction)
{-# DEPRECATED bpMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | The ID of the @DataSource@ that points to the group of observations to predict.
--
-- /Note:/ Consider using 'batchPredictionDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBatchPredictionDataSourceId :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpBatchPredictionDataSourceId = Lens.lens (batchPredictionDataSourceId :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionDataSourceId = a} :: BatchPrediction)
{-# DEPRECATED bpBatchPredictionDataSourceId "Use generic-lens or generic-optics with 'batchPredictionDataSourceId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'totalRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpTotalRecordCount :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Integer)
bpTotalRecordCount = Lens.lens (totalRecordCount :: BatchPrediction -> Lude.Maybe Lude.Integer) (\s a -> s {totalRecordCount = a} :: BatchPrediction)
{-# DEPRECATED bpTotalRecordCount "Use generic-lens or generic-optics with 'totalRecordCount' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpStartedAt :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Timestamp)
bpStartedAt = Lens.lens (startedAt :: BatchPrediction -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: BatchPrediction)
{-# DEPRECATED bpStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBatchPredictionId :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpBatchPredictionId = Lens.lens (batchPredictionId :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {batchPredictionId = a} :: BatchPrediction)
{-# DEPRECATED bpBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpFinishedAt :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Timestamp)
bpFinishedAt = Lens.lens (finishedAt :: BatchPrediction -> Lude.Maybe Lude.Timestamp) (\s a -> s {finishedAt = a} :: BatchPrediction)
{-# DEPRECATED bpFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'invalidRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpInvalidRecordCount :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Integer)
bpInvalidRecordCount = Lens.lens (invalidRecordCount :: BatchPrediction -> Lude.Maybe Lude.Integer) (\s a -> s {invalidRecordCount = a} :: BatchPrediction)
{-# DEPRECATED bpInvalidRecordCount "Use generic-lens or generic-optics with 'invalidRecordCount' instead." #-}

-- | The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIAMUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpCreatedByIAMUser :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpCreatedByIAMUser = Lens.lens (createdByIAMUser :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {createdByIAMUser = a} :: BatchPrediction)
{-# DEPRECATED bpCreatedByIAMUser "Use generic-lens or generic-optics with 'createdByIAMUser' instead." #-}

-- | A user-supplied name or description of the @BatchPrediction@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpName :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpName = Lens.lens (name :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: BatchPrediction)
{-# DEPRECATED bpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description of the most recent details about processing the batch prediction request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpMessage :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpMessage = Lens.lens (message :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: BatchPrediction)
{-# DEPRECATED bpMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The location of an Amazon S3 bucket or directory to receive the operation results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'.
--
-- /Note:/ Consider using 'outputURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpOutputURI :: Lens.Lens' BatchPrediction (Lude.Maybe Lude.Text)
bpOutputURI = Lens.lens (outputURI :: BatchPrediction -> Lude.Maybe Lude.Text) (\s a -> s {outputURI = a} :: BatchPrediction)
{-# DEPRECATED bpOutputURI "Use generic-lens or generic-optics with 'outputURI' instead." #-}

instance Lude.FromJSON BatchPrediction where
  parseJSON =
    Lude.withObject
      "BatchPrediction"
      ( \x ->
          BatchPrediction'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "LastUpdatedAt")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "ComputeTime")
            Lude.<*> (x Lude..:? "InputDataLocationS3")
            Lude.<*> (x Lude..:? "MLModelId")
            Lude.<*> (x Lude..:? "BatchPredictionDataSourceId")
            Lude.<*> (x Lude..:? "TotalRecordCount")
            Lude.<*> (x Lude..:? "StartedAt")
            Lude.<*> (x Lude..:? "BatchPredictionId")
            Lude.<*> (x Lude..:? "FinishedAt")
            Lude.<*> (x Lude..:? "InvalidRecordCount")
            Lude.<*> (x Lude..:? "CreatedByIamUser")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "OutputUri")
      )
