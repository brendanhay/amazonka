{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.GetBatchPrediction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @BatchPrediction@ that includes detailed metadata, status, and
-- data file information for a @Batch Prediction@ request.
module Network.AWS.MachineLearning.GetBatchPrediction
  ( -- * Creating a Request
    GetBatchPrediction (..),
    newGetBatchPrediction,

    -- * Request Lenses
    getBatchPrediction_batchPredictionId,

    -- * Destructuring the Response
    GetBatchPredictionResponse (..),
    newGetBatchPredictionResponse,

    -- * Response Lenses
    getBatchPredictionResponse_batchPredictionId,
    getBatchPredictionResponse_status,
    getBatchPredictionResponse_startedAt,
    getBatchPredictionResponse_outputUri,
    getBatchPredictionResponse_message,
    getBatchPredictionResponse_createdAt,
    getBatchPredictionResponse_finishedAt,
    getBatchPredictionResponse_createdByIamUser,
    getBatchPredictionResponse_name,
    getBatchPredictionResponse_invalidRecordCount,
    getBatchPredictionResponse_totalRecordCount,
    getBatchPredictionResponse_batchPredictionDataSourceId,
    getBatchPredictionResponse_mLModelId,
    getBatchPredictionResponse_inputDataLocationS3,
    getBatchPredictionResponse_computeTime,
    getBatchPredictionResponse_lastUpdatedAt,
    getBatchPredictionResponse_logUri,
    getBatchPredictionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBatchPrediction' smart constructor.
data GetBatchPrediction = GetBatchPrediction'
  { -- | An ID assigned to the @BatchPrediction@ at creation.
    batchPredictionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBatchPrediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'getBatchPrediction_batchPredictionId' - An ID assigned to the @BatchPrediction@ at creation.
newGetBatchPrediction ::
  -- | 'batchPredictionId'
  Core.Text ->
  GetBatchPrediction
newGetBatchPrediction pBatchPredictionId_ =
  GetBatchPrediction'
    { batchPredictionId =
        pBatchPredictionId_
    }

-- | An ID assigned to the @BatchPrediction@ at creation.
getBatchPrediction_batchPredictionId :: Lens.Lens' GetBatchPrediction Core.Text
getBatchPrediction_batchPredictionId = Lens.lens (\GetBatchPrediction' {batchPredictionId} -> batchPredictionId) (\s@GetBatchPrediction' {} a -> s {batchPredictionId = a} :: GetBatchPrediction)

instance Core.AWSRequest GetBatchPrediction where
  type
    AWSResponse GetBatchPrediction =
      GetBatchPredictionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBatchPredictionResponse'
            Core.<$> (x Core..?> "BatchPredictionId")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "StartedAt")
            Core.<*> (x Core..?> "OutputUri")
            Core.<*> (x Core..?> "Message")
            Core.<*> (x Core..?> "CreatedAt")
            Core.<*> (x Core..?> "FinishedAt")
            Core.<*> (x Core..?> "CreatedByIamUser")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "InvalidRecordCount")
            Core.<*> (x Core..?> "TotalRecordCount")
            Core.<*> (x Core..?> "BatchPredictionDataSourceId")
            Core.<*> (x Core..?> "MLModelId")
            Core.<*> (x Core..?> "InputDataLocationS3")
            Core.<*> (x Core..?> "ComputeTime")
            Core.<*> (x Core..?> "LastUpdatedAt")
            Core.<*> (x Core..?> "LogUri")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBatchPrediction

instance Core.NFData GetBatchPrediction

instance Core.ToHeaders GetBatchPrediction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.GetBatchPrediction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetBatchPrediction where
  toJSON GetBatchPrediction' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("BatchPredictionId" Core..= batchPredictionId)
          ]
      )

instance Core.ToPath GetBatchPrediction where
  toPath = Core.const "/"

instance Core.ToQuery GetBatchPrediction where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetBatchPrediction@ operation and describes
-- a @BatchPrediction@.
--
-- /See:/ 'newGetBatchPredictionResponse' smart constructor.
data GetBatchPredictionResponse = GetBatchPredictionResponse'
  { -- | An ID assigned to the @BatchPrediction@ at creation. This value should
    -- be identical to the value of the @BatchPredictionID@ in the request.
    batchPredictionId :: Core.Maybe Core.Text,
    -- | The status of the @BatchPrediction@, which can be one of the following
    -- values:
    --
    -- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
    --     to generate batch predictions.
    -- -   @INPROGRESS@ - The batch predictions are in progress.
    -- -   @FAILED@ - The request to perform a batch prediction did not run to
    --     completion. It is not usable.
    -- -   @COMPLETED@ - The batch prediction process completed successfully.
    -- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
    --     usable.
    status :: Core.Maybe EntityStatus,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
    -- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
    -- is in the @PENDING@ state.
    startedAt :: Core.Maybe Core.POSIX,
    -- | The location of an Amazon S3 bucket or directory to receive the
    -- operation results.
    outputUri :: Core.Maybe Core.Text,
    -- | A description of the most recent details about processing the batch
    -- prediction request.
    message :: Core.Maybe Core.Text,
    -- | The time when the @BatchPrediction@ was created. The time is expressed
    -- in epoch time.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
    -- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
    -- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Core.Maybe Core.POSIX,
    -- | The AWS user account that invoked the @BatchPrediction@. The account
    -- type can be either an AWS root account or an AWS Identity and Access
    -- Management (IAM) user account.
    createdByIamUser :: Core.Maybe Core.Text,
    -- | A user-supplied name or description of the @BatchPrediction@.
    name :: Core.Maybe Core.Text,
    -- | The number of invalid records that Amazon Machine Learning saw while
    -- processing the @BatchPrediction@.
    invalidRecordCount :: Core.Maybe Core.Integer,
    -- | The number of total records that Amazon Machine Learning saw while
    -- processing the @BatchPrediction@.
    totalRecordCount :: Core.Maybe Core.Integer,
    -- | The ID of the @DataSource@ that was used to create the
    -- @BatchPrediction@.
    batchPredictionDataSourceId :: Core.Maybe Core.Text,
    -- | The ID of the @MLModel@ that generated predictions for the
    -- @BatchPrediction@ request.
    mLModelId :: Core.Maybe Core.Text,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Core.Maybe Core.Text,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning
    -- spent processing the @BatchPrediction@, normalized and scaled on
    -- computation resources. @ComputeTime@ is only available if the
    -- @BatchPrediction@ is in the @COMPLETED@ state.
    computeTime :: Core.Maybe Core.Integer,
    -- | The time of the most recent edit to @BatchPrediction@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.POSIX,
    -- | A link to the file that contains logs of the @CreateBatchPrediction@
    -- operation.
    logUri :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBatchPredictionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'getBatchPredictionResponse_batchPredictionId' - An ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
--
-- 'status', 'getBatchPredictionResponse_status' - The status of the @BatchPrediction@, which can be one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate batch predictions.
-- -   @INPROGRESS@ - The batch predictions are in progress.
-- -   @FAILED@ - The request to perform a batch prediction did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The batch prediction process completed successfully.
-- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
--     usable.
--
-- 'startedAt', 'getBatchPredictionResponse_startedAt' - The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
-- is in the @PENDING@ state.
--
-- 'outputUri', 'getBatchPredictionResponse_outputUri' - The location of an Amazon S3 bucket or directory to receive the
-- operation results.
--
-- 'message', 'getBatchPredictionResponse_message' - A description of the most recent details about processing the batch
-- prediction request.
--
-- 'createdAt', 'getBatchPredictionResponse_createdAt' - The time when the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
--
-- 'finishedAt', 'getBatchPredictionResponse_finishedAt' - The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
--
-- 'createdByIamUser', 'getBatchPredictionResponse_createdByIamUser' - The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
--
-- 'name', 'getBatchPredictionResponse_name' - A user-supplied name or description of the @BatchPrediction@.
--
-- 'invalidRecordCount', 'getBatchPredictionResponse_invalidRecordCount' - The number of invalid records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
--
-- 'totalRecordCount', 'getBatchPredictionResponse_totalRecordCount' - The number of total records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
--
-- 'batchPredictionDataSourceId', 'getBatchPredictionResponse_batchPredictionDataSourceId' - The ID of the @DataSource@ that was used to create the
-- @BatchPrediction@.
--
-- 'mLModelId', 'getBatchPredictionResponse_mLModelId' - The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
--
-- 'inputDataLocationS3', 'getBatchPredictionResponse_inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
--
-- 'computeTime', 'getBatchPredictionResponse_computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @BatchPrediction@, normalized and scaled on
-- computation resources. @ComputeTime@ is only available if the
-- @BatchPrediction@ is in the @COMPLETED@ state.
--
-- 'lastUpdatedAt', 'getBatchPredictionResponse_lastUpdatedAt' - The time of the most recent edit to @BatchPrediction@. The time is
-- expressed in epoch time.
--
-- 'logUri', 'getBatchPredictionResponse_logUri' - A link to the file that contains logs of the @CreateBatchPrediction@
-- operation.
--
-- 'httpStatus', 'getBatchPredictionResponse_httpStatus' - The response's http status code.
newGetBatchPredictionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBatchPredictionResponse
newGetBatchPredictionResponse pHttpStatus_ =
  GetBatchPredictionResponse'
    { batchPredictionId =
        Core.Nothing,
      status = Core.Nothing,
      startedAt = Core.Nothing,
      outputUri = Core.Nothing,
      message = Core.Nothing,
      createdAt = Core.Nothing,
      finishedAt = Core.Nothing,
      createdByIamUser = Core.Nothing,
      name = Core.Nothing,
      invalidRecordCount = Core.Nothing,
      totalRecordCount = Core.Nothing,
      batchPredictionDataSourceId = Core.Nothing,
      mLModelId = Core.Nothing,
      inputDataLocationS3 = Core.Nothing,
      computeTime = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      logUri = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
getBatchPredictionResponse_batchPredictionId :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_batchPredictionId = Lens.lens (\GetBatchPredictionResponse' {batchPredictionId} -> batchPredictionId) (\s@GetBatchPredictionResponse' {} a -> s {batchPredictionId = a} :: GetBatchPredictionResponse)

-- | The status of the @BatchPrediction@, which can be one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate batch predictions.
-- -   @INPROGRESS@ - The batch predictions are in progress.
-- -   @FAILED@ - The request to perform a batch prediction did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The batch prediction process completed successfully.
-- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
--     usable.
getBatchPredictionResponse_status :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe EntityStatus)
getBatchPredictionResponse_status = Lens.lens (\GetBatchPredictionResponse' {status} -> status) (\s@GetBatchPredictionResponse' {} a -> s {status = a} :: GetBatchPredictionResponse)

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
-- is in the @PENDING@ state.
getBatchPredictionResponse_startedAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.UTCTime)
getBatchPredictionResponse_startedAt = Lens.lens (\GetBatchPredictionResponse' {startedAt} -> startedAt) (\s@GetBatchPredictionResponse' {} a -> s {startedAt = a} :: GetBatchPredictionResponse) Core.. Lens.mapping Core._Time

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results.
getBatchPredictionResponse_outputUri :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_outputUri = Lens.lens (\GetBatchPredictionResponse' {outputUri} -> outputUri) (\s@GetBatchPredictionResponse' {} a -> s {outputUri = a} :: GetBatchPredictionResponse)

-- | A description of the most recent details about processing the batch
-- prediction request.
getBatchPredictionResponse_message :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_message = Lens.lens (\GetBatchPredictionResponse' {message} -> message) (\s@GetBatchPredictionResponse' {} a -> s {message = a} :: GetBatchPredictionResponse)

-- | The time when the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
getBatchPredictionResponse_createdAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.UTCTime)
getBatchPredictionResponse_createdAt = Lens.lens (\GetBatchPredictionResponse' {createdAt} -> createdAt) (\s@GetBatchPredictionResponse' {} a -> s {createdAt = a} :: GetBatchPredictionResponse) Core.. Lens.mapping Core._Time

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
getBatchPredictionResponse_finishedAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.UTCTime)
getBatchPredictionResponse_finishedAt = Lens.lens (\GetBatchPredictionResponse' {finishedAt} -> finishedAt) (\s@GetBatchPredictionResponse' {} a -> s {finishedAt = a} :: GetBatchPredictionResponse) Core.. Lens.mapping Core._Time

-- | The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
getBatchPredictionResponse_createdByIamUser :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_createdByIamUser = Lens.lens (\GetBatchPredictionResponse' {createdByIamUser} -> createdByIamUser) (\s@GetBatchPredictionResponse' {} a -> s {createdByIamUser = a} :: GetBatchPredictionResponse)

-- | A user-supplied name or description of the @BatchPrediction@.
getBatchPredictionResponse_name :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_name = Lens.lens (\GetBatchPredictionResponse' {name} -> name) (\s@GetBatchPredictionResponse' {} a -> s {name = a} :: GetBatchPredictionResponse)

-- | The number of invalid records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
getBatchPredictionResponse_invalidRecordCount :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Integer)
getBatchPredictionResponse_invalidRecordCount = Lens.lens (\GetBatchPredictionResponse' {invalidRecordCount} -> invalidRecordCount) (\s@GetBatchPredictionResponse' {} a -> s {invalidRecordCount = a} :: GetBatchPredictionResponse)

-- | The number of total records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
getBatchPredictionResponse_totalRecordCount :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Integer)
getBatchPredictionResponse_totalRecordCount = Lens.lens (\GetBatchPredictionResponse' {totalRecordCount} -> totalRecordCount) (\s@GetBatchPredictionResponse' {} a -> s {totalRecordCount = a} :: GetBatchPredictionResponse)

-- | The ID of the @DataSource@ that was used to create the
-- @BatchPrediction@.
getBatchPredictionResponse_batchPredictionDataSourceId :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_batchPredictionDataSourceId = Lens.lens (\GetBatchPredictionResponse' {batchPredictionDataSourceId} -> batchPredictionDataSourceId) (\s@GetBatchPredictionResponse' {} a -> s {batchPredictionDataSourceId = a} :: GetBatchPredictionResponse)

-- | The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
getBatchPredictionResponse_mLModelId :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_mLModelId = Lens.lens (\GetBatchPredictionResponse' {mLModelId} -> mLModelId) (\s@GetBatchPredictionResponse' {} a -> s {mLModelId = a} :: GetBatchPredictionResponse)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
getBatchPredictionResponse_inputDataLocationS3 :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_inputDataLocationS3 = Lens.lens (\GetBatchPredictionResponse' {inputDataLocationS3} -> inputDataLocationS3) (\s@GetBatchPredictionResponse' {} a -> s {inputDataLocationS3 = a} :: GetBatchPredictionResponse)

-- | The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @BatchPrediction@, normalized and scaled on
-- computation resources. @ComputeTime@ is only available if the
-- @BatchPrediction@ is in the @COMPLETED@ state.
getBatchPredictionResponse_computeTime :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Integer)
getBatchPredictionResponse_computeTime = Lens.lens (\GetBatchPredictionResponse' {computeTime} -> computeTime) (\s@GetBatchPredictionResponse' {} a -> s {computeTime = a} :: GetBatchPredictionResponse)

-- | The time of the most recent edit to @BatchPrediction@. The time is
-- expressed in epoch time.
getBatchPredictionResponse_lastUpdatedAt :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.UTCTime)
getBatchPredictionResponse_lastUpdatedAt = Lens.lens (\GetBatchPredictionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetBatchPredictionResponse' {} a -> s {lastUpdatedAt = a} :: GetBatchPredictionResponse) Core.. Lens.mapping Core._Time

-- | A link to the file that contains logs of the @CreateBatchPrediction@
-- operation.
getBatchPredictionResponse_logUri :: Lens.Lens' GetBatchPredictionResponse (Core.Maybe Core.Text)
getBatchPredictionResponse_logUri = Lens.lens (\GetBatchPredictionResponse' {logUri} -> logUri) (\s@GetBatchPredictionResponse' {} a -> s {logUri = a} :: GetBatchPredictionResponse)

-- | The response's http status code.
getBatchPredictionResponse_httpStatus :: Lens.Lens' GetBatchPredictionResponse Core.Int
getBatchPredictionResponse_httpStatus = Lens.lens (\GetBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@GetBatchPredictionResponse' {} a -> s {httpStatus = a} :: GetBatchPredictionResponse)

instance Core.NFData GetBatchPredictionResponse
