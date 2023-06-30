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
-- Module      : Amazonka.SageMaker.Types.InferenceRecommendationsJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceRecommendationsJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RecommendationJobStatus
import Amazonka.SageMaker.Types.RecommendationJobType

-- | A structure that contains a list of recommendation jobs.
--
-- /See:/ 'newInferenceRecommendationsJob' smart constructor.
data InferenceRecommendationsJob = InferenceRecommendationsJob'
  { -- | A timestamp that shows when the job completed.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | If the job fails, provides information why the job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The name of the job.
    jobName :: Prelude.Text,
    -- | The job description.
    jobDescription :: Prelude.Text,
    -- | The recommendation job type.
    jobType :: RecommendationJobType,
    -- | The Amazon Resource Name (ARN) of the recommendation job.
    jobArn :: Prelude.Text,
    -- | The status of the job.
    status :: RecommendationJobStatus,
    -- | A timestamp that shows when the job was created.
    creationTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
    -- SageMaker to perform tasks on your behalf.
    roleArn :: Prelude.Text,
    -- | A timestamp that shows when the job was last modified.
    lastModifiedTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceRecommendationsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'inferenceRecommendationsJob_completionTime' - A timestamp that shows when the job completed.
--
-- 'failureReason', 'inferenceRecommendationsJob_failureReason' - If the job fails, provides information why the job failed.
--
-- 'jobName', 'inferenceRecommendationsJob_jobName' - The name of the job.
--
-- 'jobDescription', 'inferenceRecommendationsJob_jobDescription' - The job description.
--
-- 'jobType', 'inferenceRecommendationsJob_jobType' - The recommendation job type.
--
-- 'jobArn', 'inferenceRecommendationsJob_jobArn' - The Amazon Resource Name (ARN) of the recommendation job.
--
-- 'status', 'inferenceRecommendationsJob_status' - The status of the job.
--
-- 'creationTime', 'inferenceRecommendationsJob_creationTime' - A timestamp that shows when the job was created.
--
-- 'roleArn', 'inferenceRecommendationsJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
--
-- 'lastModifiedTime', 'inferenceRecommendationsJob_lastModifiedTime' - A timestamp that shows when the job was last modified.
newInferenceRecommendationsJob ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobDescription'
  Prelude.Text ->
  -- | 'jobType'
  RecommendationJobType ->
  -- | 'jobArn'
  Prelude.Text ->
  -- | 'status'
  RecommendationJobStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  InferenceRecommendationsJob
newInferenceRecommendationsJob
  pJobName_
  pJobDescription_
  pJobType_
  pJobArn_
  pStatus_
  pCreationTime_
  pRoleArn_
  pLastModifiedTime_ =
    InferenceRecommendationsJob'
      { completionTime =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        jobName = pJobName_,
        jobDescription = pJobDescription_,
        jobType = pJobType_,
        jobArn = pJobArn_,
        status = pStatus_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        roleArn = pRoleArn_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | A timestamp that shows when the job completed.
inferenceRecommendationsJob_completionTime :: Lens.Lens' InferenceRecommendationsJob (Prelude.Maybe Prelude.UTCTime)
inferenceRecommendationsJob_completionTime = Lens.lens (\InferenceRecommendationsJob' {completionTime} -> completionTime) (\s@InferenceRecommendationsJob' {} a -> s {completionTime = a} :: InferenceRecommendationsJob) Prelude.. Lens.mapping Data._Time

-- | If the job fails, provides information why the job failed.
inferenceRecommendationsJob_failureReason :: Lens.Lens' InferenceRecommendationsJob (Prelude.Maybe Prelude.Text)
inferenceRecommendationsJob_failureReason = Lens.lens (\InferenceRecommendationsJob' {failureReason} -> failureReason) (\s@InferenceRecommendationsJob' {} a -> s {failureReason = a} :: InferenceRecommendationsJob)

-- | The name of the job.
inferenceRecommendationsJob_jobName :: Lens.Lens' InferenceRecommendationsJob Prelude.Text
inferenceRecommendationsJob_jobName = Lens.lens (\InferenceRecommendationsJob' {jobName} -> jobName) (\s@InferenceRecommendationsJob' {} a -> s {jobName = a} :: InferenceRecommendationsJob)

-- | The job description.
inferenceRecommendationsJob_jobDescription :: Lens.Lens' InferenceRecommendationsJob Prelude.Text
inferenceRecommendationsJob_jobDescription = Lens.lens (\InferenceRecommendationsJob' {jobDescription} -> jobDescription) (\s@InferenceRecommendationsJob' {} a -> s {jobDescription = a} :: InferenceRecommendationsJob)

-- | The recommendation job type.
inferenceRecommendationsJob_jobType :: Lens.Lens' InferenceRecommendationsJob RecommendationJobType
inferenceRecommendationsJob_jobType = Lens.lens (\InferenceRecommendationsJob' {jobType} -> jobType) (\s@InferenceRecommendationsJob' {} a -> s {jobType = a} :: InferenceRecommendationsJob)

-- | The Amazon Resource Name (ARN) of the recommendation job.
inferenceRecommendationsJob_jobArn :: Lens.Lens' InferenceRecommendationsJob Prelude.Text
inferenceRecommendationsJob_jobArn = Lens.lens (\InferenceRecommendationsJob' {jobArn} -> jobArn) (\s@InferenceRecommendationsJob' {} a -> s {jobArn = a} :: InferenceRecommendationsJob)

-- | The status of the job.
inferenceRecommendationsJob_status :: Lens.Lens' InferenceRecommendationsJob RecommendationJobStatus
inferenceRecommendationsJob_status = Lens.lens (\InferenceRecommendationsJob' {status} -> status) (\s@InferenceRecommendationsJob' {} a -> s {status = a} :: InferenceRecommendationsJob)

-- | A timestamp that shows when the job was created.
inferenceRecommendationsJob_creationTime :: Lens.Lens' InferenceRecommendationsJob Prelude.UTCTime
inferenceRecommendationsJob_creationTime = Lens.lens (\InferenceRecommendationsJob' {creationTime} -> creationTime) (\s@InferenceRecommendationsJob' {} a -> s {creationTime = a} :: InferenceRecommendationsJob) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon
-- SageMaker to perform tasks on your behalf.
inferenceRecommendationsJob_roleArn :: Lens.Lens' InferenceRecommendationsJob Prelude.Text
inferenceRecommendationsJob_roleArn = Lens.lens (\InferenceRecommendationsJob' {roleArn} -> roleArn) (\s@InferenceRecommendationsJob' {} a -> s {roleArn = a} :: InferenceRecommendationsJob)

-- | A timestamp that shows when the job was last modified.
inferenceRecommendationsJob_lastModifiedTime :: Lens.Lens' InferenceRecommendationsJob Prelude.UTCTime
inferenceRecommendationsJob_lastModifiedTime = Lens.lens (\InferenceRecommendationsJob' {lastModifiedTime} -> lastModifiedTime) (\s@InferenceRecommendationsJob' {} a -> s {lastModifiedTime = a} :: InferenceRecommendationsJob) Prelude.. Data._Time

instance Data.FromJSON InferenceRecommendationsJob where
  parseJSON =
    Data.withObject
      "InferenceRecommendationsJob"
      ( \x ->
          InferenceRecommendationsJob'
            Prelude.<$> (x Data..:? "CompletionTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "JobName")
            Prelude.<*> (x Data..: "JobDescription")
            Prelude.<*> (x Data..: "JobType")
            Prelude.<*> (x Data..: "JobArn")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "RoleArn")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable InferenceRecommendationsJob where
  hashWithSalt _salt InferenceRecommendationsJob' {..} =
    _salt
      `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobDescription
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData InferenceRecommendationsJob where
  rnf InferenceRecommendationsJob' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobDescription
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
