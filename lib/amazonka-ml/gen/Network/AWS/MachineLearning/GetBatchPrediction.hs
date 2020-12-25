{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gbprrsBatchPredictionDataSourceId,
    gbprrsBatchPredictionId,
    gbprrsComputeTime,
    gbprrsCreatedAt,
    gbprrsCreatedByIamUser,
    gbprrsFinishedAt,
    gbprrsInputDataLocationS3,
    gbprrsInvalidRecordCount,
    gbprrsLastUpdatedAt,
    gbprrsLogUri,
    gbprrsMLModelId,
    gbprrsMessage,
    gbprrsName,
    gbprrsOutputUri,
    gbprrsStartedAt,
    gbprrsStatus,
    gbprrsTotalRecordCount,
    gbprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetBatchPrediction' smart constructor.
newtype GetBatchPrediction = GetBatchPrediction'
  { -- | An ID assigned to the @BatchPrediction@ at creation.
    batchPredictionId :: Types.EntityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetBatchPrediction' value with any optional fields omitted.
mkGetBatchPrediction ::
  -- | 'batchPredictionId'
  Types.EntityId ->
  GetBatchPrediction
mkGetBatchPrediction batchPredictionId =
  GetBatchPrediction' {batchPredictionId}

-- | An ID assigned to the @BatchPrediction@ at creation.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpBatchPredictionId :: Lens.Lens' GetBatchPrediction Types.EntityId
gbpBatchPredictionId = Lens.field @"batchPredictionId"
{-# DEPRECATED gbpBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

instance Core.FromJSON GetBatchPrediction where
  toJSON GetBatchPrediction {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("BatchPredictionId" Core..= batchPredictionId)]
      )

instance Core.AWSRequest GetBatchPrediction where
  type Rs GetBatchPrediction = GetBatchPredictionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.GetBatchPrediction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBatchPredictionResponse'
            Core.<$> (x Core..:? "BatchPredictionDataSourceId")
            Core.<*> (x Core..:? "BatchPredictionId")
            Core.<*> (x Core..:? "ComputeTime")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "CreatedByIamUser")
            Core.<*> (x Core..:? "FinishedAt")
            Core.<*> (x Core..:? "InputDataLocationS3")
            Core.<*> (x Core..:? "InvalidRecordCount")
            Core.<*> (x Core..:? "LastUpdatedAt")
            Core.<*> (x Core..:? "LogUri")
            Core.<*> (x Core..:? "MLModelId")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "OutputUri")
            Core.<*> (x Core..:? "StartedAt")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "TotalRecordCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @GetBatchPrediction@ operation and describes a @BatchPrediction@ .
--
-- /See:/ 'mkGetBatchPredictionResponse' smart constructor.
data GetBatchPredictionResponse = GetBatchPredictionResponse'
  { -- | The ID of the @DataSource@ that was used to create the @BatchPrediction@ .
    batchPredictionDataSourceId :: Core.Maybe Types.BatchPredictionDataSourceId,
    -- | An ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request.
    batchPredictionId :: Core.Maybe Types.BatchPredictionId,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @BatchPrediction@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @BatchPrediction@ is in the @COMPLETED@ state.
    computeTime :: Core.Maybe Core.Integer,
    -- | The time when the @BatchPrediction@ was created. The time is expressed in epoch time.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
    createdByIamUser :: Core.Maybe Types.CreatedByIamUser,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
    inputDataLocationS3 :: Core.Maybe Types.InputDataLocationS3,
    -- | The number of invalid records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
    invalidRecordCount :: Core.Maybe Core.Integer,
    -- | The time of the most recent edit to @BatchPrediction@ . The time is expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.NominalDiffTime,
    -- | A link to the file that contains logs of the @CreateBatchPrediction@ operation.
    logUri :: Core.Maybe Types.LogUri,
    -- | The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
    mLModelId :: Core.Maybe Types.MLModelId,
    -- | A description of the most recent details about processing the batch prediction request.
    message :: Core.Maybe Types.Message,
    -- | A user-supplied name or description of the @BatchPrediction@ .
    name :: Core.Maybe Types.EntityName,
    -- | The location of an Amazon S3 bucket or directory to receive the operation results.
    outputUri :: Core.Maybe Types.OutputUri,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @INPROGRESS@ . @StartedAt@ isn't available if the @BatchPrediction@ is in the @PENDING@ state.
    startedAt :: Core.Maybe Core.NominalDiffTime,
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
    status :: Core.Maybe Types.EntityStatus,
    -- | The number of total records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
    totalRecordCount :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetBatchPredictionResponse' value with any optional fields omitted.
mkGetBatchPredictionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetBatchPredictionResponse
mkGetBatchPredictionResponse responseStatus =
  GetBatchPredictionResponse'
    { batchPredictionDataSourceId =
        Core.Nothing,
      batchPredictionId = Core.Nothing,
      computeTime = Core.Nothing,
      createdAt = Core.Nothing,
      createdByIamUser = Core.Nothing,
      finishedAt = Core.Nothing,
      inputDataLocationS3 = Core.Nothing,
      invalidRecordCount = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      logUri = Core.Nothing,
      mLModelId = Core.Nothing,
      message = Core.Nothing,
      name = Core.Nothing,
      outputUri = Core.Nothing,
      startedAt = Core.Nothing,
      status = Core.Nothing,
      totalRecordCount = Core.Nothing,
      responseStatus
    }

-- | The ID of the @DataSource@ that was used to create the @BatchPrediction@ .
--
-- /Note:/ Consider using 'batchPredictionDataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsBatchPredictionDataSourceId :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.BatchPredictionDataSourceId)
gbprrsBatchPredictionDataSourceId = Lens.field @"batchPredictionDataSourceId"
{-# DEPRECATED gbprrsBatchPredictionDataSourceId "Use generic-lens or generic-optics with 'batchPredictionDataSourceId' instead." #-}

-- | An ID assigned to the @BatchPrediction@ at creation. This value should be identical to the value of the @BatchPredictionID@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsBatchPredictionId :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.BatchPredictionId)
gbprrsBatchPredictionId = Lens.field @"batchPredictionId"
{-# DEPRECATED gbprrsBatchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead." #-}

-- | The approximate CPU time in milliseconds that Amazon Machine Learning spent processing the @BatchPrediction@ , normalized and scaled on computation resources. @ComputeTime@ is only available if the @BatchPrediction@ is in the @COMPLETED@ state.
--
-- /Note:/ Consider using 'computeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsComputeTime :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Integer)
gbprrsComputeTime = Lens.field @"computeTime"
{-# DEPRECATED gbprrsComputeTime "Use generic-lens or generic-optics with 'computeTime' instead." #-}

-- | The time when the @BatchPrediction@ was created. The time is expressed in epoch time.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsCreatedAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.NominalDiffTime)
gbprrsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED gbprrsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS user account that invoked the @BatchPrediction@ . The account type can be either an AWS root account or an AWS Identity and Access Management (IAM) user account.
--
-- /Note:/ Consider using 'createdByIamUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsCreatedByIamUser :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.CreatedByIamUser)
gbprrsCreatedByIamUser = Lens.field @"createdByIamUser"
{-# DEPRECATED gbprrsCreatedByIamUser "Use generic-lens or generic-optics with 'createdByIamUser' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @COMPLETED@ or @FAILED@ . @FinishedAt@ is only available when the @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
--
-- /Note:/ Consider using 'finishedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsFinishedAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.NominalDiffTime)
gbprrsFinishedAt = Lens.field @"finishedAt"
{-# DEPRECATED gbprrsFinishedAt "Use generic-lens or generic-optics with 'finishedAt' instead." #-}

-- | The location of the data file or directory in Amazon Simple Storage Service (Amazon S3).
--
-- /Note:/ Consider using 'inputDataLocationS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsInputDataLocationS3 :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.InputDataLocationS3)
gbprrsInputDataLocationS3 = Lens.field @"inputDataLocationS3"
{-# DEPRECATED gbprrsInputDataLocationS3 "Use generic-lens or generic-optics with 'inputDataLocationS3' instead." #-}

-- | The number of invalid records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
--
-- /Note:/ Consider using 'invalidRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsInvalidRecordCount :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Integer)
gbprrsInvalidRecordCount = Lens.field @"invalidRecordCount"
{-# DEPRECATED gbprrsInvalidRecordCount "Use generic-lens or generic-optics with 'invalidRecordCount' instead." #-}

-- | The time of the most recent edit to @BatchPrediction@ . The time is expressed in epoch time.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsLastUpdatedAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.NominalDiffTime)
gbprrsLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# DEPRECATED gbprrsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | A link to the file that contains logs of the @CreateBatchPrediction@ operation.
--
-- /Note:/ Consider using 'logUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsLogUri :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.LogUri)
gbprrsLogUri = Lens.field @"logUri"
{-# DEPRECATED gbprrsLogUri "Use generic-lens or generic-optics with 'logUri' instead." #-}

-- | The ID of the @MLModel@ that generated predictions for the @BatchPrediction@ request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsMLModelId :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.MLModelId)
gbprrsMLModelId = Lens.field @"mLModelId"
{-# DEPRECATED gbprrsMLModelId "Use generic-lens or generic-optics with 'mLModelId' instead." #-}

-- | A description of the most recent details about processing the batch prediction request.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsMessage :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.Message)
gbprrsMessage = Lens.field @"message"
{-# DEPRECATED gbprrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A user-supplied name or description of the @BatchPrediction@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsName :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.EntityName)
gbprrsName = Lens.field @"name"
{-# DEPRECATED gbprrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The location of an Amazon S3 bucket or directory to receive the operation results.
--
-- /Note:/ Consider using 'outputUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsOutputUri :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.OutputUri)
gbprrsOutputUri = Lens.field @"outputUri"
{-# DEPRECATED gbprrsOutputUri "Use generic-lens or generic-optics with 'outputUri' instead." #-}

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@ as @INPROGRESS@ . @StartedAt@ isn't available if the @BatchPrediction@ is in the @PENDING@ state.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsStartedAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.NominalDiffTime)
gbprrsStartedAt = Lens.field @"startedAt"
{-# DEPRECATED gbprrsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

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
gbprrsStatus :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Types.EntityStatus)
gbprrsStatus = Lens.field @"status"
{-# DEPRECATED gbprrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The number of total records that Amazon Machine Learning saw while processing the @BatchPrediction@ .
--
-- /Note:/ Consider using 'totalRecordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsTotalRecordCount :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Integer)
gbprrsTotalRecordCount = Lens.field @"totalRecordCount"
{-# DEPRECATED gbprrsTotalRecordCount "Use generic-lens or generic-optics with 'totalRecordCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbprrsResponseStatus :: Lens.Lens' GetBatchPredictionResponse Core.Int
gbprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gbprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
