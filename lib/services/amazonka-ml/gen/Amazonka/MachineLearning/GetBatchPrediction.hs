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
-- Module      : Amazonka.MachineLearning.GetBatchPrediction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @BatchPrediction@ that includes detailed metadata, status, and
-- data file information for a @Batch Prediction@ request.
module Amazonka.MachineLearning.GetBatchPrediction
  ( -- * Creating a Request
    GetBatchPrediction (..),
    newGetBatchPrediction,

    -- * Request Lenses
    getBatchPrediction_batchPredictionId,

    -- * Destructuring the Response
    GetBatchPredictionResponse (..),
    newGetBatchPredictionResponse,

    -- * Response Lenses
    getBatchPredictionResponse_status,
    getBatchPredictionResponse_lastUpdatedAt,
    getBatchPredictionResponse_createdAt,
    getBatchPredictionResponse_computeTime,
    getBatchPredictionResponse_inputDataLocationS3,
    getBatchPredictionResponse_mLModelId,
    getBatchPredictionResponse_batchPredictionDataSourceId,
    getBatchPredictionResponse_totalRecordCount,
    getBatchPredictionResponse_startedAt,
    getBatchPredictionResponse_batchPredictionId,
    getBatchPredictionResponse_finishedAt,
    getBatchPredictionResponse_invalidRecordCount,
    getBatchPredictionResponse_createdByIamUser,
    getBatchPredictionResponse_name,
    getBatchPredictionResponse_logUri,
    getBatchPredictionResponse_message,
    getBatchPredictionResponse_outputUri,
    getBatchPredictionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MachineLearning.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "LastUpdatedAt")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "ComputeTime")
            Prelude.<*> (x Core..?> "InputDataLocationS3")
            Prelude.<*> (x Core..?> "MLModelId")
            Prelude.<*> (x Core..?> "BatchPredictionDataSourceId")
            Prelude.<*> (x Core..?> "TotalRecordCount")
            Prelude.<*> (x Core..?> "StartedAt")
            Prelude.<*> (x Core..?> "BatchPredictionId")
            Prelude.<*> (x Core..?> "FinishedAt")
            Prelude.<*> (x Core..?> "InvalidRecordCount")
            Prelude.<*> (x Core..?> "CreatedByIamUser")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LogUri")
            Prelude.<*> (x Core..?> "Message")
            Prelude.<*> (x Core..?> "OutputUri")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBatchPrediction where
  hashWithSalt _salt GetBatchPrediction' {..} =
    _salt `Prelude.hashWithSalt` batchPredictionId

instance Prelude.NFData GetBatchPrediction where
  rnf GetBatchPrediction' {..} =
    Prelude.rnf batchPredictionId

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
  { -- | The status of the @BatchPrediction@, which can be one of the following
    -- values:
    --
    -- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
    --     to generate batch predictions.
    --
    -- -   @INPROGRESS@ - The batch predictions are in progress.
    --
    -- -   @FAILED@ - The request to perform a batch prediction did not run to
    --     completion. It is not usable.
    --
    -- -   @COMPLETED@ - The batch prediction process completed successfully.
    --
    -- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
    --     usable.
    status :: Prelude.Maybe EntityStatus,
    -- | The time of the most recent edit to @BatchPrediction@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The time when the @BatchPrediction@ was created. The time is expressed
    -- in epoch time.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The approximate CPU time in milliseconds that Amazon Machine Learning
    -- spent processing the @BatchPrediction@, normalized and scaled on
    -- computation resources. @ComputeTime@ is only available if the
    -- @BatchPrediction@ is in the @COMPLETED@ state.
    computeTime :: Prelude.Maybe Prelude.Integer,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @MLModel@ that generated predictions for the
    -- @BatchPrediction@ request.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @DataSource@ that was used to create the
    -- @BatchPrediction@.
    batchPredictionDataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The number of total records that Amazon Machine Learning saw while
    -- processing the @BatchPrediction@.
    totalRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
    -- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
    -- is in the @PENDING@ state.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | An ID assigned to the @BatchPrediction@ at creation. This value should
    -- be identical to the value of the @BatchPredictionID@ in the request.
    batchPredictionId :: Prelude.Maybe Prelude.Text,
    -- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
    -- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
    -- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
    finishedAt :: Prelude.Maybe Core.POSIX,
    -- | The number of invalid records that Amazon Machine Learning saw while
    -- processing the @BatchPrediction@.
    invalidRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | The AWS user account that invoked the @BatchPrediction@. The account
    -- type can be either an AWS root account or an AWS Identity and Access
    -- Management (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @BatchPrediction@.
    name :: Prelude.Maybe Prelude.Text,
    -- | A link to the file that contains logs of the @CreateBatchPrediction@
    -- operation.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | A description of the most recent details about processing the batch
    -- prediction request.
    message :: Prelude.Maybe Prelude.Text,
    -- | The location of an Amazon S3 bucket or directory to receive the
    -- operation results.
    outputUri :: Prelude.Maybe Prelude.Text,
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
-- 'status', 'getBatchPredictionResponse_status' - The status of the @BatchPrediction@, which can be one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate batch predictions.
--
-- -   @INPROGRESS@ - The batch predictions are in progress.
--
-- -   @FAILED@ - The request to perform a batch prediction did not run to
--     completion. It is not usable.
--
-- -   @COMPLETED@ - The batch prediction process completed successfully.
--
-- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
--     usable.
--
-- 'lastUpdatedAt', 'getBatchPredictionResponse_lastUpdatedAt' - The time of the most recent edit to @BatchPrediction@. The time is
-- expressed in epoch time.
--
-- 'createdAt', 'getBatchPredictionResponse_createdAt' - The time when the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
--
-- 'computeTime', 'getBatchPredictionResponse_computeTime' - The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @BatchPrediction@, normalized and scaled on
-- computation resources. @ComputeTime@ is only available if the
-- @BatchPrediction@ is in the @COMPLETED@ state.
--
-- 'inputDataLocationS3', 'getBatchPredictionResponse_inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
--
-- 'mLModelId', 'getBatchPredictionResponse_mLModelId' - The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
--
-- 'batchPredictionDataSourceId', 'getBatchPredictionResponse_batchPredictionDataSourceId' - The ID of the @DataSource@ that was used to create the
-- @BatchPrediction@.
--
-- 'totalRecordCount', 'getBatchPredictionResponse_totalRecordCount' - The number of total records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
--
-- 'startedAt', 'getBatchPredictionResponse_startedAt' - The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
-- is in the @PENDING@ state.
--
-- 'batchPredictionId', 'getBatchPredictionResponse_batchPredictionId' - An ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
--
-- 'finishedAt', 'getBatchPredictionResponse_finishedAt' - The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
--
-- 'invalidRecordCount', 'getBatchPredictionResponse_invalidRecordCount' - The number of invalid records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
--
-- 'createdByIamUser', 'getBatchPredictionResponse_createdByIamUser' - The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
--
-- 'name', 'getBatchPredictionResponse_name' - A user-supplied name or description of the @BatchPrediction@.
--
-- 'logUri', 'getBatchPredictionResponse_logUri' - A link to the file that contains logs of the @CreateBatchPrediction@
-- operation.
--
-- 'message', 'getBatchPredictionResponse_message' - A description of the most recent details about processing the batch
-- prediction request.
--
-- 'outputUri', 'getBatchPredictionResponse_outputUri' - The location of an Amazon S3 bucket or directory to receive the
-- operation results.
--
-- 'httpStatus', 'getBatchPredictionResponse_httpStatus' - The response's http status code.
newGetBatchPredictionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBatchPredictionResponse
newGetBatchPredictionResponse pHttpStatus_ =
  GetBatchPredictionResponse'
    { status =
        Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      computeTime = Prelude.Nothing,
      inputDataLocationS3 = Prelude.Nothing,
      mLModelId = Prelude.Nothing,
      batchPredictionDataSourceId = Prelude.Nothing,
      totalRecordCount = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      batchPredictionId = Prelude.Nothing,
      finishedAt = Prelude.Nothing,
      invalidRecordCount = Prelude.Nothing,
      createdByIamUser = Prelude.Nothing,
      name = Prelude.Nothing,
      logUri = Prelude.Nothing,
      message = Prelude.Nothing,
      outputUri = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the @BatchPrediction@, which can be one of the following
-- values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate batch predictions.
--
-- -   @INPROGRESS@ - The batch predictions are in progress.
--
-- -   @FAILED@ - The request to perform a batch prediction did not run to
--     completion. It is not usable.
--
-- -   @COMPLETED@ - The batch prediction process completed successfully.
--
-- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
--     usable.
getBatchPredictionResponse_status :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe EntityStatus)
getBatchPredictionResponse_status = Lens.lens (\GetBatchPredictionResponse' {status} -> status) (\s@GetBatchPredictionResponse' {} a -> s {status = a} :: GetBatchPredictionResponse)

-- | The time of the most recent edit to @BatchPrediction@. The time is
-- expressed in epoch time.
getBatchPredictionResponse_lastUpdatedAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_lastUpdatedAt = Lens.lens (\GetBatchPredictionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetBatchPredictionResponse' {} a -> s {lastUpdatedAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | The time when the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
getBatchPredictionResponse_createdAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_createdAt = Lens.lens (\GetBatchPredictionResponse' {createdAt} -> createdAt) (\s@GetBatchPredictionResponse' {} a -> s {createdAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | The approximate CPU time in milliseconds that Amazon Machine Learning
-- spent processing the @BatchPrediction@, normalized and scaled on
-- computation resources. @ComputeTime@ is only available if the
-- @BatchPrediction@ is in the @COMPLETED@ state.
getBatchPredictionResponse_computeTime :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Integer)
getBatchPredictionResponse_computeTime = Lens.lens (\GetBatchPredictionResponse' {computeTime} -> computeTime) (\s@GetBatchPredictionResponse' {} a -> s {computeTime = a} :: GetBatchPredictionResponse)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
getBatchPredictionResponse_inputDataLocationS3 :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_inputDataLocationS3 = Lens.lens (\GetBatchPredictionResponse' {inputDataLocationS3} -> inputDataLocationS3) (\s@GetBatchPredictionResponse' {} a -> s {inputDataLocationS3 = a} :: GetBatchPredictionResponse)

-- | The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
getBatchPredictionResponse_mLModelId :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_mLModelId = Lens.lens (\GetBatchPredictionResponse' {mLModelId} -> mLModelId) (\s@GetBatchPredictionResponse' {} a -> s {mLModelId = a} :: GetBatchPredictionResponse)

-- | The ID of the @DataSource@ that was used to create the
-- @BatchPrediction@.
getBatchPredictionResponse_batchPredictionDataSourceId :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_batchPredictionDataSourceId = Lens.lens (\GetBatchPredictionResponse' {batchPredictionDataSourceId} -> batchPredictionDataSourceId) (\s@GetBatchPredictionResponse' {} a -> s {batchPredictionDataSourceId = a} :: GetBatchPredictionResponse)

-- | The number of total records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
getBatchPredictionResponse_totalRecordCount :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Integer)
getBatchPredictionResponse_totalRecordCount = Lens.lens (\GetBatchPredictionResponse' {totalRecordCount} -> totalRecordCount) (\s@GetBatchPredictionResponse' {} a -> s {totalRecordCount = a} :: GetBatchPredictionResponse)

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @INPROGRESS@. @StartedAt@ isn\'t available if the @BatchPrediction@
-- is in the @PENDING@ state.
getBatchPredictionResponse_startedAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_startedAt = Lens.lens (\GetBatchPredictionResponse' {startedAt} -> startedAt) (\s@GetBatchPredictionResponse' {} a -> s {startedAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | An ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
getBatchPredictionResponse_batchPredictionId :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_batchPredictionId = Lens.lens (\GetBatchPredictionResponse' {batchPredictionId} -> batchPredictionId) (\s@GetBatchPredictionResponse' {} a -> s {batchPredictionId = a} :: GetBatchPredictionResponse)

-- | The epoch time when Amazon Machine Learning marked the @BatchPrediction@
-- as @COMPLETED@ or @FAILED@. @FinishedAt@ is only available when the
-- @BatchPrediction@ is in the @COMPLETED@ or @FAILED@ state.
getBatchPredictionResponse_finishedAt :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.UTCTime)
getBatchPredictionResponse_finishedAt = Lens.lens (\GetBatchPredictionResponse' {finishedAt} -> finishedAt) (\s@GetBatchPredictionResponse' {} a -> s {finishedAt = a} :: GetBatchPredictionResponse) Prelude.. Lens.mapping Core._Time

-- | The number of invalid records that Amazon Machine Learning saw while
-- processing the @BatchPrediction@.
getBatchPredictionResponse_invalidRecordCount :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Integer)
getBatchPredictionResponse_invalidRecordCount = Lens.lens (\GetBatchPredictionResponse' {invalidRecordCount} -> invalidRecordCount) (\s@GetBatchPredictionResponse' {} a -> s {invalidRecordCount = a} :: GetBatchPredictionResponse)

-- | The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
getBatchPredictionResponse_createdByIamUser :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_createdByIamUser = Lens.lens (\GetBatchPredictionResponse' {createdByIamUser} -> createdByIamUser) (\s@GetBatchPredictionResponse' {} a -> s {createdByIamUser = a} :: GetBatchPredictionResponse)

-- | A user-supplied name or description of the @BatchPrediction@.
getBatchPredictionResponse_name :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_name = Lens.lens (\GetBatchPredictionResponse' {name} -> name) (\s@GetBatchPredictionResponse' {} a -> s {name = a} :: GetBatchPredictionResponse)

-- | A link to the file that contains logs of the @CreateBatchPrediction@
-- operation.
getBatchPredictionResponse_logUri :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_logUri = Lens.lens (\GetBatchPredictionResponse' {logUri} -> logUri) (\s@GetBatchPredictionResponse' {} a -> s {logUri = a} :: GetBatchPredictionResponse)

-- | A description of the most recent details about processing the batch
-- prediction request.
getBatchPredictionResponse_message :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_message = Lens.lens (\GetBatchPredictionResponse' {message} -> message) (\s@GetBatchPredictionResponse' {} a -> s {message = a} :: GetBatchPredictionResponse)

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results.
getBatchPredictionResponse_outputUri :: Lens.Lens' GetBatchPredictionResponse (Prelude.Maybe Prelude.Text)
getBatchPredictionResponse_outputUri = Lens.lens (\GetBatchPredictionResponse' {outputUri} -> outputUri) (\s@GetBatchPredictionResponse' {} a -> s {outputUri = a} :: GetBatchPredictionResponse)

-- | The response's http status code.
getBatchPredictionResponse_httpStatus :: Lens.Lens' GetBatchPredictionResponse Prelude.Int
getBatchPredictionResponse_httpStatus = Lens.lens (\GetBatchPredictionResponse' {httpStatus} -> httpStatus) (\s@GetBatchPredictionResponse' {} a -> s {httpStatus = a} :: GetBatchPredictionResponse)

instance Prelude.NFData GetBatchPredictionResponse where
  rnf GetBatchPredictionResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf computeTime
      `Prelude.seq` Prelude.rnf inputDataLocationS3
      `Prelude.seq` Prelude.rnf mLModelId
      `Prelude.seq` Prelude.rnf batchPredictionDataSourceId
      `Prelude.seq` Prelude.rnf totalRecordCount
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf batchPredictionId
      `Prelude.seq` Prelude.rnf finishedAt
      `Prelude.seq` Prelude.rnf invalidRecordCount
      `Prelude.seq` Prelude.rnf createdByIamUser
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf logUri
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf outputUri
      `Prelude.seq` Prelude.rnf httpStatus
