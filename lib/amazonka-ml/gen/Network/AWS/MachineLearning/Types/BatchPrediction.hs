{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.BatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.BatchPrediction
  ( BatchPrediction (..)
  -- * Smart constructor
  , mkBatchPrediction
  -- * Lenses
  , bpBatchPredictionDataSourceId
  , bpBatchPredictionId
  , bpComputeTime
  , bpCreatedAt
  , bpCreatedByIamUser
  , bpFinishedAt
  , bpInputDataLocationS3
  , bpInvalidRecordCount
  , bpLastUpdatedAt
  , bpMLModelId
  , bpMessage
  , bpName
  , bpOutputUri
  , bpStartedAt
  , bpStatus
  , bpTotalRecordCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types.AwsUserArn as Types
import qualified Network.AWS.MachineLearning.Types.BatchPredictionDataSourceId as Types
import qualified Network.AWS.MachineLearning.Types.BatchPredictionId as Types
import qualified Network.AWS.MachineLearning.Types.EntityName as Types
import qualified Network.AWS.MachineLearning.Types.EntityStatus as Types
import qualified Network.AWS.MachineLearning.Types.InputDataLocationS3 as Types
import qualified Network.AWS.MachineLearning.Types.MLModelId as Types
import qualified Network.AWS.MachineLearning.Types.Message as Types
import qualified Network.AWS.MachineLearning.Types.OutputUri as Types
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @GetBatchPrediction@ operation.
--
-- The content consists of the detailed metadata, the status, and the data file information of a @Batch Prediction@ .
--
-- /See:/ 'mkBatchPrediction' smart constructor.
data BatchPrediction = BatchPrediction'
  { batchPredictionDataSourceId :: Core.Maybe Types.BatchPredictionDataSourceId
    -- ^ The ID of the @DataSource@ that points to the group of observations to predict.
  , batchPredictionId :: Core.Maybe Types.BatchPredictionId
    -- ^ The ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request. 
  , computeTime :: Core.Maybe Core.Integer
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the @BatchPrediction@ was created. The time is expressed in epoch time.
  , createdByIamUser :: Core.Maybe Types.AwsUserArn
    -- ^ The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
  , finishedAt :: Core.Maybe Core.NominalDiffTime
  , inputDataLocationS3 :: Core.Maybe Types.InputDataLocationS3
    -- ^ The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
  , invalidRecordCount :: Core.Maybe Core.Integer
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
  , mLModelId :: Core.Maybe Types.MLModelId
    -- ^ The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
  , message :: Core.Maybe Types.Message
    -- ^ A description of the most recent details about processing the batch prediction request.
  , name :: Core.Maybe Types.EntityName
    -- ^ A user-supplied name or description of the @BatchPrediction@ .
  , outputUri :: Core.Maybe Types.OutputUri
    -- ^ The location of an Amazon S3 bucket or directory to receive the operation results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'.
  , startedAt :: Core.Maybe Core.NominalDiffTime
  , status :: Core.Maybe Types.EntityStatus
    -- ^ The status of the @BatchPrediction@ . This element can have one of the following values:
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
  , totalRecordCount :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchPrediction' value with any optional fields omitted.
mkBatchPrediction
    :: BatchPrediction
mkBatchPrediction
  = BatchPrediction'{batchPredictionDataSourceId = Core.Nothing,
                     batchPredictionId = Core.Nothing, computeTime = Core.Nothing,
                     createdAt = Core.Nothing, createdByIamUser = Core.Nothing,
                     finishedAt = Core.Nothing, inputDataLocationS3 = Core.Nothing,
                     invalidRecordCount = Core.Nothing, lastUpdatedAt = Core.Nothing,
                     mLModelId = Core.Nothing, message = Core.Nothing,
                     name = Core.Nothing, outputUri = Core.Nothing,
                     startedAt = Core.Nothing, status = Core.Nothing,
                     totalRecordCount = Core.Nothing}

-- | The ID of the @DataSource@ that points to the group of observations to predict.
--
-- /Note:/ Consider using 'batchPredictionDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBatchPredictionDataSourceId :: Lens.Lens' BatchPrediction (Core.Maybe Types.BatchPredictionDataSourceId)
bpBatchPredictionDataSourceId = Lens.field @"batchPredictionDataSourceId"
{-# INLINEABLE bpBatchPredictionDataSourceId #-}
{-# DEPRECATED batchPredictionDataSourceId "Use generic-lens or generic-optics with 'batchPredictionDataSourceId' instead"  #-}

-- | The ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request. 
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBatchPredictionId :: Lens.Lens' BatchPrediction (Core.Maybe Types.BatchPredictionId)
bpBatchPredictionId = Lens.field @"batchPredictionId"
{-# INLINEABLE bpBatchPredictionId #-}
{-# DEPRECATED batchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpComputeTime :: Lens.Lens' BatchPrediction (Core.Maybe Core.Integer)
bpComputeTime = Lens.field @"computeTime"
{-# INLINEABLE bpComputeTime #-}
{-# DEPRECATED computeTime "Use generic-lens or generic-optics with 'computeTime' instead"  #-}

-- | The time that the @BatchPrediction@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpCreatedAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.NominalDiffTime)
bpCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE bpCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpCreatedByIamUser :: Lens.Lens' BatchPrediction (Core.Maybe Types.AwsUserArn)
bpCreatedByIamUser = Lens.field @"createdByIamUser"
{-# INLINEABLE bpCreatedByIamUser #-}
{-# DEPRECATED createdByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpFinishedAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.NominalDiffTime)
bpFinishedAt = Lens.field @"finishedAt"
{-# INLINEABLE bpFinishedAt #-}
{-# DEPRECATED finishedAt "Use generic-lens or generic-optics with 'finishedAt' instead"  #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpInputDataLocationS3 :: Lens.Lens' BatchPrediction (Core.Maybe Types.InputDataLocationS3)
bpInputDataLocationS3 = Lens.field @"inputDataLocationS3"
{-# INLINEABLE bpInputDataLocationS3 #-}
{-# DEPRECATED inputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'invalidRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpInvalidRecordCount :: Lens.Lens' BatchPrediction (Core.Maybe Core.Integer)
bpInvalidRecordCount = Lens.field @"invalidRecordCount"
{-# INLINEABLE bpInvalidRecordCount #-}
{-# DEPRECATED invalidRecordCount "Use generic-lens or generic-optics with 'invalidRecordCount' instead"  #-}

-- | The time of the most recent edit to the @BatchPrediction@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpLastUpdatedAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.NominalDiffTime)
bpLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE bpLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpMLModelId :: Lens.Lens' BatchPrediction (Core.Maybe Types.MLModelId)
bpMLModelId = Lens.field @"mLModelId"
{-# INLINEABLE bpMLModelId #-}
{-# DEPRECATED mLModelId "Use generic-lens or generic-optics with 'mLModelId' instead"  #-}

-- | A description of the most recent details about processing the batch prediction request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpMessage :: Lens.Lens' BatchPrediction (Core.Maybe Types.Message)
bpMessage = Lens.field @"message"
{-# INLINEABLE bpMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | A user-supplied name or description of the @BatchPrediction@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpName :: Lens.Lens' BatchPrediction (Core.Maybe Types.EntityName)
bpName = Lens.field @"name"
{-# INLINEABLE bpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The location of an Amazon S3 bucket or directory to receive the operation results. The following substrings are not allowed in the @s3 key@ portion of the @outputURI@ field: ':', '//', '/./', '/../'.
--
-- /Note:/ Consider using 'outputUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpOutputUri :: Lens.Lens' BatchPrediction (Core.Maybe Types.OutputUri)
bpOutputUri = Lens.field @"outputUri"
{-# INLINEABLE bpOutputUri #-}
{-# DEPRECATED outputUri "Use generic-lens or generic-optics with 'outputUri' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpStartedAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.NominalDiffTime)
bpStartedAt = Lens.field @"startedAt"
{-# INLINEABLE bpStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

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
bpStatus :: Lens.Lens' BatchPrediction (Core.Maybe Types.EntityStatus)
bpStatus = Lens.field @"status"
{-# INLINEABLE bpStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'totalRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpTotalRecordCount :: Lens.Lens' BatchPrediction (Core.Maybe Core.Integer)
bpTotalRecordCount = Lens.field @"totalRecordCount"
{-# INLINEABLE bpTotalRecordCount #-}
{-# DEPRECATED totalRecordCount "Use generic-lens or generic-optics with 'totalRecordCount' instead"  #-}

instance Core.FromJSON BatchPrediction where
        parseJSON
          = Core.withObject "BatchPrediction" Core.$
              \ x ->
                BatchPrediction' Core.<$>
                  (x Core..:? "BatchPredictionDataSourceId") Core.<*>
                    x Core..:? "BatchPredictionId"
                    Core.<*> x Core..:? "ComputeTime"
                    Core.<*> x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "CreatedByIamUser"
                    Core.<*> x Core..:? "FinishedAt"
                    Core.<*> x Core..:? "InputDataLocationS3"
                    Core.<*> x Core..:? "InvalidRecordCount"
                    Core.<*> x Core..:? "LastUpdatedAt"
                    Core.<*> x Core..:? "MLModelId"
                    Core.<*> x Core..:? "Message"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "OutputUri"
                    Core.<*> x Core..:? "StartedAt"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "TotalRecordCount"
