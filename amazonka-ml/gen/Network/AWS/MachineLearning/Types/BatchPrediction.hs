{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types.EntityStatus
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @GetBatchPrediction@ operation.
--
-- The content consists of the detailed metadata, the status, and the data
-- file information of a @Batch Prediction@.
--
-- /See:/ 'newBatchPrediction' smart constructor.
data BatchPrediction = BatchPrediction'
  { -- | The ID assigned to the @BatchPrediction@ at creation. This value should
    -- be identical to the value of the @BatchPredictionID@ in the request.
    batchPredictionId :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe EntityStatus,
    startedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The location of an Amazon S3 bucket or directory to receive the
    -- operation results. The following substrings are not allowed in the
    -- @s3 key@ portion of the @outputURI@ field: \':\', \'\/\/\', \'\/.\/\',
    -- \'\/..\/\'.
    outputUri :: Prelude.Maybe Prelude.Text,
    -- | A description of the most recent details about processing the batch
    -- prediction request.
    message :: Prelude.Maybe Prelude.Text,
    -- | The time that the @BatchPrediction@ was created. The time is expressed
    -- in epoch time.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    finishedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS user account that invoked the @BatchPrediction@. The account
    -- type can be either an AWS root account or an AWS Identity and Access
    -- Management (IAM) user account.
    createdByIamUser :: Prelude.Maybe Prelude.Text,
    -- | A user-supplied name or description of the @BatchPrediction@.
    name :: Prelude.Maybe Prelude.Text,
    invalidRecordCount :: Prelude.Maybe Prelude.Integer,
    totalRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the @DataSource@ that points to the group of observations to
    -- predict.
    batchPredictionDataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @MLModel@ that generated predictions for the
    -- @BatchPrediction@ request.
    mLModelId :: Prelude.Maybe Prelude.Text,
    -- | The location of the data file or directory in Amazon Simple Storage
    -- Service (Amazon S3).
    inputDataLocationS3 :: Prelude.Maybe Prelude.Text,
    computeTime :: Prelude.Maybe Prelude.Integer,
    -- | The time of the most recent edit to the @BatchPrediction@. The time is
    -- expressed in epoch time.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      lastUpdatedAt = Prelude.Nothing
    }

-- | The ID assigned to the @BatchPrediction@ at creation. This value should
-- be identical to the value of the @BatchPredictionID@ in the request.
batchPrediction_batchPredictionId :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
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
batchPrediction_status :: Lens.Lens' BatchPrediction (Prelude.Maybe EntityStatus)
batchPrediction_status = Lens.lens (\BatchPrediction' {status} -> status) (\s@BatchPrediction' {} a -> s {status = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_startedAt :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.UTCTime)
batchPrediction_startedAt = Lens.lens (\BatchPrediction' {startedAt} -> startedAt) (\s@BatchPrediction' {} a -> s {startedAt = a} :: BatchPrediction) Prelude.. Lens.mapping Prelude._Time

-- | The location of an Amazon S3 bucket or directory to receive the
-- operation results. The following substrings are not allowed in the
-- @s3 key@ portion of the @outputURI@ field: \':\', \'\/\/\', \'\/.\/\',
-- \'\/..\/\'.
batchPrediction_outputUri :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_outputUri = Lens.lens (\BatchPrediction' {outputUri} -> outputUri) (\s@BatchPrediction' {} a -> s {outputUri = a} :: BatchPrediction)

-- | A description of the most recent details about processing the batch
-- prediction request.
batchPrediction_message :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_message = Lens.lens (\BatchPrediction' {message} -> message) (\s@BatchPrediction' {} a -> s {message = a} :: BatchPrediction)

-- | The time that the @BatchPrediction@ was created. The time is expressed
-- in epoch time.
batchPrediction_createdAt :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.UTCTime)
batchPrediction_createdAt = Lens.lens (\BatchPrediction' {createdAt} -> createdAt) (\s@BatchPrediction' {} a -> s {createdAt = a} :: BatchPrediction) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
batchPrediction_finishedAt :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.UTCTime)
batchPrediction_finishedAt = Lens.lens (\BatchPrediction' {finishedAt} -> finishedAt) (\s@BatchPrediction' {} a -> s {finishedAt = a} :: BatchPrediction) Prelude.. Lens.mapping Prelude._Time

-- | The AWS user account that invoked the @BatchPrediction@. The account
-- type can be either an AWS root account or an AWS Identity and Access
-- Management (IAM) user account.
batchPrediction_createdByIamUser :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_createdByIamUser = Lens.lens (\BatchPrediction' {createdByIamUser} -> createdByIamUser) (\s@BatchPrediction' {} a -> s {createdByIamUser = a} :: BatchPrediction)

-- | A user-supplied name or description of the @BatchPrediction@.
batchPrediction_name :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_name = Lens.lens (\BatchPrediction' {name} -> name) (\s@BatchPrediction' {} a -> s {name = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_invalidRecordCount :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Integer)
batchPrediction_invalidRecordCount = Lens.lens (\BatchPrediction' {invalidRecordCount} -> invalidRecordCount) (\s@BatchPrediction' {} a -> s {invalidRecordCount = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_totalRecordCount :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Integer)
batchPrediction_totalRecordCount = Lens.lens (\BatchPrediction' {totalRecordCount} -> totalRecordCount) (\s@BatchPrediction' {} a -> s {totalRecordCount = a} :: BatchPrediction)

-- | The ID of the @DataSource@ that points to the group of observations to
-- predict.
batchPrediction_batchPredictionDataSourceId :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_batchPredictionDataSourceId = Lens.lens (\BatchPrediction' {batchPredictionDataSourceId} -> batchPredictionDataSourceId) (\s@BatchPrediction' {} a -> s {batchPredictionDataSourceId = a} :: BatchPrediction)

-- | The ID of the @MLModel@ that generated predictions for the
-- @BatchPrediction@ request.
batchPrediction_mLModelId :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_mLModelId = Lens.lens (\BatchPrediction' {mLModelId} -> mLModelId) (\s@BatchPrediction' {} a -> s {mLModelId = a} :: BatchPrediction)

-- | The location of the data file or directory in Amazon Simple Storage
-- Service (Amazon S3).
batchPrediction_inputDataLocationS3 :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Text)
batchPrediction_inputDataLocationS3 = Lens.lens (\BatchPrediction' {inputDataLocationS3} -> inputDataLocationS3) (\s@BatchPrediction' {} a -> s {inputDataLocationS3 = a} :: BatchPrediction)

-- | Undocumented member.
batchPrediction_computeTime :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.Integer)
batchPrediction_computeTime = Lens.lens (\BatchPrediction' {computeTime} -> computeTime) (\s@BatchPrediction' {} a -> s {computeTime = a} :: BatchPrediction)

-- | The time of the most recent edit to the @BatchPrediction@. The time is
-- expressed in epoch time.
batchPrediction_lastUpdatedAt :: Lens.Lens' BatchPrediction (Prelude.Maybe Prelude.UTCTime)
batchPrediction_lastUpdatedAt = Lens.lens (\BatchPrediction' {lastUpdatedAt} -> lastUpdatedAt) (\s@BatchPrediction' {} a -> s {lastUpdatedAt = a} :: BatchPrediction) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON BatchPrediction where
  parseJSON =
    Prelude.withObject
      "BatchPrediction"
      ( \x ->
          BatchPrediction'
            Prelude.<$> (x Prelude..:? "BatchPredictionId")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "StartedAt")
            Prelude.<*> (x Prelude..:? "OutputUri")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "FinishedAt")
            Prelude.<*> (x Prelude..:? "CreatedByIamUser")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "InvalidRecordCount")
            Prelude.<*> (x Prelude..:? "TotalRecordCount")
            Prelude.<*> (x Prelude..:? "BatchPredictionDataSourceId")
            Prelude.<*> (x Prelude..:? "MLModelId")
            Prelude.<*> (x Prelude..:? "InputDataLocationS3")
            Prelude.<*> (x Prelude..:? "ComputeTime")
            Prelude.<*> (x Prelude..:? "LastUpdatedAt")
      )

instance Prelude.Hashable BatchPrediction

instance Prelude.NFData BatchPrediction
