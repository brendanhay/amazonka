{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.BatchPrediction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.BatchPrediction where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus

-- | Represents the output of a @GetBatchPrediction@ operation.
--
-- The content consists of the detailed metadata, the status, and the data
-- file information of a @Batch Prediction@.
--
-- /See:/ 'newBatchPrediction' smart constructor.
data BatchPrediction = BatchPrediction'
  { -- | The ID assigned to the @BatchPrediction@ at creation. This value should
    -- be identical to the value of the @BatchPredictionID@ in the request.
    batchPredictionId :: Core.Maybe Core.Text,
    -- | The status of the @BatchPrediction@. This element can have one of the
    -- following values:
    --
    -- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
    --     to generate predictions for a batch of observations.
    -- -   @INPROGRESS@ - The process is underway.
    -- -   @FAILED@ - The request to perform a batch prediction did not run to
    --     completion. It is not usable.
    -- -   @COMPLETED@ - The batch prediction process completed successfully.
    -- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
    --     usable.
    status :: Core.Maybe EntityStatus,
    startedAt :: Core.Maybe Core.POSIX,
    -- | The location of an Amazon S3 bucket or directory to receive the
    -- operation results. The following substrings are not allowed in the
    -- @s3 key@ portion of the @outputURI@ field: \':\', \'\/\/\', \'\/.\/\',
    -- \'\/..\/\'.
    outputUri :: Core.Maybe Core.Text,
    -- | A description of the most recent details about processing the batch
    -- prediction request.
    message :: Core.Maybe Core.Text,
    -- | The time that the @BatchPrediction@ was created. The time is expressed
    -- in epoch time.
    createdAt :: Core.Maybe Core.POSIX,
    finishedAt :: Core.Maybe Core.POSIX,
    -- | The AWS user account that invoked the @BatchPrediction@. The account
    -- type can be either an AWS root account or an AWS Identity and Access
    -- Management (IAM) user account.
    createdByIamUser :: Core.Maybe Core.Text,
    -- | A user-supplied name or description of the @BatchPrediction@.
    name :: Core.Maybe Core.Text,
    invalidRecordCount :: Core.Maybe Core.Integer,
    totalRecordCount :: Core.Maybe Core.Integer,
    -- | The ID of the @DataSource@ that points to the group of observations to
    -- predict.
    batchPredictionDataSourceId :: Core.Maybe Core.Text,
    -- | The ID of the @MLModel@ that generated predictions for the
    -- @BatchPrediction@ request.
    mLModelId :: Core.Maybe Core.Text,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Core.Maybe Core.Text,
    computeTime :: Core.Maybe Core.Integer,
    -- | The time of the most recent edit to the @BatchPrediction@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchPrediction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPredictionId', 'batchPrediction_batchPredictionId' - The ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
--
-- 'status', 'batchPrediction_status' - The status of the @BatchPrediction@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate predictions for a batch of observations.
-- -   @INPROGRESS@ - The process is underway.
-- -   @FAILED@ - The request to perform a batch prediction did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The batch prediction process completed successfully.
-- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
--     usable.
--
-- 'startedAt', 'batchPrediction_startedAt' - Undocumented member.
--
-- 'outputUri', 'batchPrediction_outputUri' - The location of an Amazon S3 bucket or directory to receive the
-- operation results. The following substrings are not allowed in the
-- @s3 key@ portion of the @outputURI@ field: \':\', \'\/\/\', \'\/.\/\',
-- \'\/..\/\'.
--
-- 'message', 'batchPrediction_message' - A description of the most recent details about processing the batch
-- prediction request.
--
-- 'createdAt', 'batchPrediction_createdAt' - The time that the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
--
-- 'finishedAt', 'batchPrediction_finishedAt' - Undocumented member.
--
-- 'createdByIamUser', 'batchPrediction_createdByIamUser' - The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
--
-- 'name', 'batchPrediction_name' - A user-supplied name or description of the @BatchPrediction@.
--
-- 'invalidRecordCount', 'batchPrediction_invalidRecordCount' - Undocumented member.
--
-- 'totalRecordCount', 'batchPrediction_totalRecordCount' - Undocumented member.
--
-- 'batchPredictionDataSourceId', 'batchPrediction_batchPredictionDataSourceId' - The ID of the @DataSource@ that points to the group of observations to
-- predict.
--
-- 'mLModelId', 'batchPrediction_mLModelId' - The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
--
-- 'inputDataLocationS3', 'batchPrediction_inputDataLocationS3' - The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
--
-- 'computeTime', 'batchPrediction_computeTime' - Undocumented member.
--
-- 'lastUpdatedAt', 'batchPrediction_lastUpdatedAt' - The time of the most recent edit to the @BatchPrediction@. The time is
-- expressed in epoch time.
newBatchPrediction ::
  BatchPrediction
newBatchPrediction =
  BatchPrediction'
    { batchPredictionId = Core.Nothing,
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
      lastUpdatedAt = Core.Nothing
    }

-- | The ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
batchPrediction_batchPredictionId :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_batchPredictionId = Lens.lens (\BatchPrediction' {batchPredictionId} -> batchPredictionId) (\s@BatchPrediction' {} a -> s {batchPredictionId = a} :: BatchPrediction)

-- | The status of the @BatchPrediction@. This element can have one of the
-- following values:
--
-- -   @PENDING@ - Amazon Machine Learning (Amazon ML) submitted a request
--     to generate predictions for a batch of observations.
-- -   @INPROGRESS@ - The process is underway.
-- -   @FAILED@ - The request to perform a batch prediction did not run to
--     completion. It is not usable.
-- -   @COMPLETED@ - The batch prediction process completed successfully.
-- -   @DELETED@ - The @BatchPrediction@ is marked as deleted. It is not
--     usable.
batchPrediction_status :: Lens.Lens' BatchPrediction (Core.Maybe EntityStatus)
batchPrediction_status = Lens.lens (\BatchPrediction' {status} -> status) (\s@BatchPrediction' {} a -> s {status = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_startedAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.UTCTime)
batchPrediction_startedAt = Lens.lens (\BatchPrediction' {startedAt} -> startedAt) (\s@BatchPrediction' {} a -> s {startedAt = a} :: BatchPrediction) Core.. Lens.mapping Core._Time

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results. The following substrings are not allowed in the
-- @s3 key@ portion of the @outputURI@ field: \':\', \'\/\/\', \'\/.\/\',
-- \'\/..\/\'.
batchPrediction_outputUri :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_outputUri = Lens.lens (\BatchPrediction' {outputUri} -> outputUri) (\s@BatchPrediction' {} a -> s {outputUri = a} :: BatchPrediction)

-- | A description of the most recent details about processing the batch
-- prediction request.
batchPrediction_message :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_message = Lens.lens (\BatchPrediction' {message} -> message) (\s@BatchPrediction' {} a -> s {message = a} :: BatchPrediction)

-- | The time that the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
batchPrediction_createdAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.UTCTime)
batchPrediction_createdAt = Lens.lens (\BatchPrediction' {createdAt} -> createdAt) (\s@BatchPrediction' {} a -> s {createdAt = a} :: BatchPrediction) Core.. Lens.mapping Core._Time

-- | Undocumented member.
batchPrediction_finishedAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.UTCTime)
batchPrediction_finishedAt = Lens.lens (\BatchPrediction' {finishedAt} -> finishedAt) (\s@BatchPrediction' {} a -> s {finishedAt = a} :: BatchPrediction) Core.. Lens.mapping Core._Time

-- | The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
batchPrediction_createdByIamUser :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_createdByIamUser = Lens.lens (\BatchPrediction' {createdByIamUser} -> createdByIamUser) (\s@BatchPrediction' {} a -> s {createdByIamUser = a} :: BatchPrediction)

-- | A user-supplied name or description of the @BatchPrediction@.
batchPrediction_name :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_name = Lens.lens (\BatchPrediction' {name} -> name) (\s@BatchPrediction' {} a -> s {name = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_invalidRecordCount :: Lens.Lens' BatchPrediction (Core.Maybe Core.Integer)
batchPrediction_invalidRecordCount = Lens.lens (\BatchPrediction' {invalidRecordCount} -> invalidRecordCount) (\s@BatchPrediction' {} a -> s {invalidRecordCount = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_totalRecordCount :: Lens.Lens' BatchPrediction (Core.Maybe Core.Integer)
batchPrediction_totalRecordCount = Lens.lens (\BatchPrediction' {totalRecordCount} -> totalRecordCount) (\s@BatchPrediction' {} a -> s {totalRecordCount = a} :: BatchPrediction)

-- | The ID of the @DataSource@ that points to the group of observations to
-- predict.
batchPrediction_batchPredictionDataSourceId :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_batchPredictionDataSourceId = Lens.lens (\BatchPrediction' {batchPredictionDataSourceId} -> batchPredictionDataSourceId) (\s@BatchPrediction' {} a -> s {batchPredictionDataSourceId = a} :: BatchPrediction)

-- | The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
batchPrediction_mLModelId :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_mLModelId = Lens.lens (\BatchPrediction' {mLModelId} -> mLModelId) (\s@BatchPrediction' {} a -> s {mLModelId = a} :: BatchPrediction)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
batchPrediction_inputDataLocationS3 :: Lens.Lens' BatchPrediction (Core.Maybe Core.Text)
batchPrediction_inputDataLocationS3 = Lens.lens (\BatchPrediction' {inputDataLocationS3} -> inputDataLocationS3) (\s@BatchPrediction' {} a -> s {inputDataLocationS3 = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_computeTime :: Lens.Lens' BatchPrediction (Core.Maybe Core.Integer)
batchPrediction_computeTime = Lens.lens (\BatchPrediction' {computeTime} -> computeTime) (\s@BatchPrediction' {} a -> s {computeTime = a} :: BatchPrediction)

-- | The time of the most recent edit to the @BatchPrediction@. The time is
-- expressed in epoch time.
batchPrediction_lastUpdatedAt :: Lens.Lens' BatchPrediction (Core.Maybe Core.UTCTime)
batchPrediction_lastUpdatedAt = Lens.lens (\BatchPrediction' {lastUpdatedAt} -> lastUpdatedAt) (\s@BatchPrediction' {} a -> s {lastUpdatedAt = a} :: BatchPrediction) Core.. Lens.mapping Core._Time

instance Core.FromJSON BatchPrediction where
  parseJSON =
    Core.withObject
      "BatchPrediction"
      ( \x ->
          BatchPrediction'
            Core.<$> (x Core..:? "BatchPredictionId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StartedAt")
            Core.<*> (x Core..:? "OutputUri")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "FinishedAt")
            Core.<*> (x Core..:? "CreatedByIamUser")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "InvalidRecordCount")
            Core.<*> (x Core..:? "TotalRecordCount")
            Core.<*> (x Core..:? "BatchPredictionDataSourceId")
            Core.<*> (x Core..:? "MLModelId")
            Core.<*> (x Core..:? "InputDataLocationS3")
            Core.<*> (x Core..:? "ComputeTime")
            Core.<*> (x Core..:? "LastUpdatedAt")
      )

instance Core.Hashable BatchPrediction

instance Core.NFData BatchPrediction
