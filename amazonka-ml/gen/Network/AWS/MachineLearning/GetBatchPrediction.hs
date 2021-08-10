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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBatchPrediction' smart constructor.
data GetBatchPrediction = GetBatchPrediction'
  { -- | An ID assigned to the @BatchPrediction@ at creation.
    batchPredictionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetBatchPrediction
newGetBatchPrediction pBatchPredictionId_ =
  GetBatchPrediction'
    { batchPredictionId =
        pBatchPredictionId_
    }

-- | An ID assigned to the @BatchPrediction@ at creation.
getBatchPrediction_batchPredictionId :: Lens.Lens' GetBatchPrediction Prelude.Text
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
            Prelude.<$> (x Core..?> "BatchPredictionId")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "StartedAt")
            Prelude.<*> (x Core..?> "OutputUri")
            Prelude.<*> (x Core..?> "Message")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "FinishedAt")
            Prelude.<*> (x Core..?> "CreatedByIamUser")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "InvalidRecordCount")
            Prelude.<*> (x Core..?> "TotalRecordCount")
            Prelude.<*> (x Core..?> "BatchPredictionDataSourceId")
            Prelude.<*> (x Core..?> "MLModelId")
            Prelude.<*> (x Core..?> "InputDataLocationS3")
            Prelude.<*> (x Core..?> "ComputeTime")
            Prelude.<*> (x Core..?> "LastUpdatedAt")
            Prelude.<*> (x Core..?> "LogUri")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBatchPrediction

instance Prelude.NFData GetBatchPrediction

instance Core.ToHeaders GetBatchPrediction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.GetBatchPrediction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBatchPrediction where
  toJSON GetBatchPrediction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("BatchPredictionId" Core..= batchPredictionId)
          ]
      )

instance Core.ToPath GetBatchPrediction where
  toPath = Prelude.const "/"

instance Core.ToQuery GetBatchPrediction where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetBatchPrediction@ operation and describes
-- a @BatchPrediction@.
--
-- /See:/ 'newGetBatchPredictionResponse' smart constructor.
data GetBatchPredictionResponse = GetBatchPredictionResponse'
  { -- | An ID assigned to the @BatchPrediction@ at creation. This value should
    -- be identical to the value of the @BatchPredictionID@ in the request.
    batchPredictionId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe EntityStatus,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
    -- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
    -- is in the @PENDING@ state.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | The location of an Amazon S3 bucket or directory to receive the
    -- operation results.
    outputUri :: Prelude.Maybe Prelude.Text,
    -- | A description of the most recent details about processing the batch
    -- prediction request.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time when the @BatchPrediction@ was created. The time is expressed
    -- in epoch time.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
    -- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
    -- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Prelude.Maybe Core.POSIX,
    -- | The AWS user account that invoked the @BatchPrediction@. The account
    -- type can be either an AWS root account or an AWS Identity and Access
    -- Management (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @BatchPrediction@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of invalid records that Amazon Machine Learning saw while
    -- processing the @BatchPrediction@.
    invalidRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of total records that Amazon Machine Learning saw while
    -- processing the @BatchPrediction@.
    totalRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the @DataSource@ that was used to create the
    -- @BatchPrediction@.
    batchPredictionDataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @MLModel@ that generated predictions for the
    -- @BatchPrediction@ request.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Prelude.Maybe Prelude.Text,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning
    -- spent processing the @BatchPrediction@, normalized and scaled on
    -- computation resources. @ComputeTime@ is only available if the
    -- @BatchPrediction@ is in the @COMPLETED@ state.
    computeTime :: Prelude.Maybe Prelude.Integer,
    -- | The time of the most recent edit to @BatchPrediction@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | A link to the file that contains logs of the @CreateBatchPrediction@
    -- operation.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetBatchPredictionResponse
newGetBatchPredictionResponse pHttpStatus_ =
  GetBatchPredictionResponse'
    { batchPredictionId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      outputUri = Prelude.Nothing,
      message = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      createdByIamUser = Prelude.Nothing,
      name = Prelude.Nothing,
      invalidRecordCount = Prelude.Nothing,
      totalRecordCount = Prelude.Nothing,
      batchPredictionDataSourceId = Prelude.Nothing,
      mLModelId = Prelude.Nothing,
      inputDataLocationS3 = Prelude.Nothing,
      computeTime = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      logUri = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
getBatchPredictionResponse_batchPredictionId :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
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
getBatchPredictionResponse_status :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe EntityStatus)
getBatchPredictionResponse_status = Lens.lens (\GetBatchPredictionResponse' {status} -> status) (\s@GetBatchPredictionResponse' {} a -> s {status = a} :: GetBatchPredictionResponse)

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
-- is in the @PENDING@ state.
getBatchPredictionResponse_startedAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_startedAt = Lens.lens (\GetBatchPredictionResponse' {startedAt} -> startedAt) (\s@GetBatchPredictionResponse' {} a -> s {startedAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results.
getBatchPredictionResponse_outputUri :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_outputUri = Lens.lens (\GetBatchPredictionResponse' {outputUri} -> outputUri) (\s@GetBatchPredictionResponse' {} a -> s {outputUri = a} :: GetBatchPredictionResponse)

-- | A description of the most recent details about processing the batch
-- prediction request.
getBatchPredictionResponse_message :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_message = Lens.lens (\GetBatchPredictionResponse' {message} -> message) (\s@GetBatchPredictionResponse' {} a -> s {message = a} :: GetBatchPredictionResponse)

-- | The time when the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
getBatchPredictionResponse_createdAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_createdAt = Lens.lens (\GetBatchPredictionResponse' {createdAt} -> createdAt) (\s@GetBatchPredictionResponse' {} a -> s {createdAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
getBatchPredictionResponse_finishedAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_finishedAt = Lens.lens (\GetBatchPredictionResponse' {finishedAt} -> finishedAt) (\s@GetBatchPredictionResponse' {} a -> s {finishedAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
getBatchPredictionResponse_createdByIamUser :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_createdByIamUser = Lens.lens (\GetBatchPredictionResponse' {createdByIamUser} -> createdByIamUser) (\s@GetBatchPredictionResponse' {} a -> s {createdByIamUser = a} :: GetBatchPredictionResponse)

-- | A user-supplied name or description of the @BatchPrediction@.
getBatchPredictionResponse_name :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_name = Lens.lens (\GetBatchPredictionResponse' {name} -> name) (\s@GetBatchPredictionResponse' {} a -> s {name = a} :: GetBatchPredictionResponse)

-- | The number of invalid records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
getBatchPredictionResponse_invalidRecordCount :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Integer)
getBatchPredictionResponse_invalidRecordCount = Lens.lens (\GetBatchPredictionResponse' {invalidRecordCount} -> invalidRecordCount) (\s@GetBatchPredictionResponse' {} a -> s {invalidRecordCount = a} :: GetBatchPredictionResponse)

-- | The number of total records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
getBatchPredictionResponse_totalRecordCount :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Integer)
getBatchPredictionResponse_totalRecordCount = Lens.lens (\GetBatchPredictionResponse' {totalRecordCount} -> totalRecordCount) (\s@GetBatchPredictionResponse' {} a -> s {totalRecordCount = a} :: GetBatchPredictionResponse)

-- | The ID of the @DataSource@ that was used to create the
-- @BatchPrediction@.
getBatchPredictionResponse_batchPredictionDataSourceId :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_batchPredictionDataSourceId = Lens.lens (\GetBatchPredictionResponse' {batchPredictionDataSourceId} -> batchPredictionDataSourceId) (\s@GetBatchPredictionResponse' {} a -> s {batchPredictionDataSourceId = a} :: GetBatchPredictionResponse)

-- | The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
getBatchPredictionResponse_mLModelId :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_mLModelId = Lens.lens (\GetBatchPredictionResponse' {mLModelId} -> mLModelId) (\s@GetBatchPredictionResponse' {} a -> s {mLModelId = a} :: GetBatchPredictionResponse)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
getBatchPredictionResponse_inputDataLocationS3 :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_inputDataLocationS3 = Lens.lens (\GetBatchPredictionResponse' {inputDataLocationS3} -> inputDataLocationS3) (\s@GetBatchPredictionResponse' {} a -> s {inputDataLocationS3 = a} :: GetBatchPredictionResponse)

-- | The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @BatchPrediction@, normalized and scaled on
-- computation resources. @ComputeTime@ is only available if the
-- @BatchPrediction@ is in the @COMPLETED@ state.
getBatchPredictionResponse_computeTime :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Integer)
getBatchPredictionResponse_computeTime = Lens.lens (\GetBatchPredictionResponse' {computeTime} -> computeTime) (\s@GetBatchPredictionResponse' {} a -> s {computeTime = a} :: GetBatchPredictionResponse)

-- | The time of the most recent edit to @BatchPrediction@. The time is
-- expressed in epoch time.
getBatchPredictionResponse_lastUpdatedAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_lastUpdatedAt = Lens.lens (\GetBatchPredictionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetBatchPredictionResponse' {} a -> s {lastUpdatedAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | A link to the file that contains logs of the @CreateBatchPrediction@
-- operation.
getBatchPredictionResponse_logUri :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_logUri = Lens.lens (\GetBatchPredictionResponse' {logUri} -> logUri) (\s@GetBatchPredictionResponse' {} a -> s {logUri = a} :: GetBatchPredictionResponse)

-- | The response's http status code.
getBatchPredictionResponse_httpStatus :: Lens.Lens' GetBatchPredictionResponse Prelude.Int
getBatchPredictionResponse_httpStatus = Lens.lens (\GetBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@GetBatchPredictionResponse' {} a -> s {httpStatus = a} :: GetBatchPredictionResponse)

instance Prelude.NFData GetBatchPredictionResponse
