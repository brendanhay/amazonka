{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.DescribeHyperParameterTuningJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description of a hyperparameter tuning job.
module Network.AWS.SageMaker.DescribeHyperParameterTuningJob
  ( -- * Creating a Request
    DescribeHyperParameterTuningJob (..),
    newDescribeHyperParameterTuningJob,

    -- * Request Lenses
    describeHyperParameterTuningJob_hyperParameterTuningJobName,

    -- * Destructuring the Response
    DescribeHyperParameterTuningJobResponse (..),
    newDescribeHyperParameterTuningJobResponse,

    -- * Response Lenses
    describeHyperParameterTuningJobResponse_bestTrainingJob,
    describeHyperParameterTuningJobResponse_warmStartConfig,
    describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime,
    describeHyperParameterTuningJobResponse_failureReason,
    describeHyperParameterTuningJobResponse_trainingJobDefinitions,
    describeHyperParameterTuningJobResponse_lastModifiedTime,
    describeHyperParameterTuningJobResponse_overallBestTrainingJob,
    describeHyperParameterTuningJobResponse_trainingJobDefinition,
    describeHyperParameterTuningJobResponse_httpStatus,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobName,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobArn,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobConfig,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobStatus,
    describeHyperParameterTuningJobResponse_creationTime,
    describeHyperParameterTuningJobResponse_trainingJobStatusCounters,
    describeHyperParameterTuningJobResponse_objectiveStatusCounters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeHyperParameterTuningJob' smart constructor.
data DescribeHyperParameterTuningJob = DescribeHyperParameterTuningJob'
  { -- | The name of the tuning job.
    hyperParameterTuningJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeHyperParameterTuningJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperParameterTuningJobName', 'describeHyperParameterTuningJob_hyperParameterTuningJobName' - The name of the tuning job.
newDescribeHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Prelude.Text ->
  DescribeHyperParameterTuningJob
newDescribeHyperParameterTuningJob
  pHyperParameterTuningJobName_ =
    DescribeHyperParameterTuningJob'
      { hyperParameterTuningJobName =
          pHyperParameterTuningJobName_
      }

-- | The name of the tuning job.
describeHyperParameterTuningJob_hyperParameterTuningJobName :: Lens.Lens' DescribeHyperParameterTuningJob Prelude.Text
describeHyperParameterTuningJob_hyperParameterTuningJobName = Lens.lens (\DescribeHyperParameterTuningJob' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@DescribeHyperParameterTuningJob' {} a -> s {hyperParameterTuningJobName = a} :: DescribeHyperParameterTuningJob)

instance
  Prelude.AWSRequest
    DescribeHyperParameterTuningJob
  where
  type
    Rs DescribeHyperParameterTuningJob =
      DescribeHyperParameterTuningJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHyperParameterTuningJobResponse'
            Prelude.<$> (x Prelude..?> "BestTrainingJob")
            Prelude.<*> (x Prelude..?> "WarmStartConfig")
            Prelude.<*> (x Prelude..?> "HyperParameterTuningEndTime")
            Prelude.<*> (x Prelude..?> "FailureReason")
            Prelude.<*> (x Prelude..?> "TrainingJobDefinitions")
            Prelude.<*> (x Prelude..?> "LastModifiedTime")
            Prelude.<*> (x Prelude..?> "OverallBestTrainingJob")
            Prelude.<*> (x Prelude..?> "TrainingJobDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "HyperParameterTuningJobName")
            Prelude.<*> (x Prelude..:> "HyperParameterTuningJobArn")
            Prelude.<*> (x Prelude..:> "HyperParameterTuningJobConfig")
            Prelude.<*> (x Prelude..:> "HyperParameterTuningJobStatus")
            Prelude.<*> (x Prelude..:> "CreationTime")
            Prelude.<*> (x Prelude..:> "TrainingJobStatusCounters")
            Prelude.<*> (x Prelude..:> "ObjectiveStatusCounters")
      )

instance
  Prelude.Hashable
    DescribeHyperParameterTuningJob

instance
  Prelude.NFData
    DescribeHyperParameterTuningJob

instance
  Prelude.ToHeaders
    DescribeHyperParameterTuningJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DescribeHyperParameterTuningJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeHyperParameterTuningJob
  where
  toJSON DescribeHyperParameterTuningJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HyperParameterTuningJobName"
                  Prelude..= hyperParameterTuningJobName
              )
          ]
      )

instance
  Prelude.ToPath
    DescribeHyperParameterTuningJob
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeHyperParameterTuningJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHyperParameterTuningJobResponse' smart constructor.
data DescribeHyperParameterTuningJobResponse = DescribeHyperParameterTuningJobResponse'
  { -- | A TrainingJobSummary object that describes the training job that
    -- completed with the best current HyperParameterTuningJobObjective.
    bestTrainingJob :: Prelude.Maybe HyperParameterTrainingJobSummary,
    -- | The configuration for starting the hyperparameter parameter tuning job
    -- using one or more previous tuning jobs as a starting point. The results
    -- of previous tuning jobs are used to inform which combinations of
    -- hyperparameters to search over in the new tuning job.
    warmStartConfig :: Prelude.Maybe HyperParameterTuningJobWarmStartConfig,
    -- | The date and time that the tuning job ended.
    hyperParameterTuningEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | If the tuning job failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A list of the HyperParameterTrainingJobDefinition objects launched for
    -- this tuning job.
    trainingJobDefinitions :: Prelude.Maybe (Prelude.NonEmpty HyperParameterTrainingJobDefinition),
    -- | The date and time that the status of the tuning job was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | If the hyperparameter tuning job is an warm start tuning job with a
    -- @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@, this is the
    -- TrainingJobSummary for the training job with the best objective metric
    -- value of all training jobs launched by this tuning job and all parent
    -- jobs specified for the warm start tuning job.
    overallBestTrainingJob :: Prelude.Maybe HyperParameterTrainingJobSummary,
    -- | The HyperParameterTrainingJobDefinition object that specifies the
    -- definition of the training jobs that this tuning job launches.
    trainingJobDefinition :: Prelude.Maybe HyperParameterTrainingJobDefinition,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the tuning job.
    hyperParameterTuningJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the tuning job.
    hyperParameterTuningJobArn :: Prelude.Text,
    -- | The HyperParameterTuningJobConfig object that specifies the
    -- configuration of the tuning job.
    hyperParameterTuningJobConfig :: HyperParameterTuningJobConfig,
    -- | The status of the tuning job: InProgress, Completed, Failed, Stopping,
    -- or Stopped.
    hyperParameterTuningJobStatus :: HyperParameterTuningJobStatus,
    -- | The date and time that the tuning job started.
    creationTime :: Prelude.POSIX,
    -- | The TrainingJobStatusCounters object that specifies the number of
    -- training jobs, categorized by status, that this tuning job launched.
    trainingJobStatusCounters :: TrainingJobStatusCounters,
    -- | The ObjectiveStatusCounters object that specifies the number of training
    -- jobs, categorized by the status of their final objective metric, that
    -- this tuning job launched.
    objectiveStatusCounters :: ObjectiveStatusCounters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeHyperParameterTuningJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bestTrainingJob', 'describeHyperParameterTuningJobResponse_bestTrainingJob' - A TrainingJobSummary object that describes the training job that
-- completed with the best current HyperParameterTuningJobObjective.
--
-- 'warmStartConfig', 'describeHyperParameterTuningJobResponse_warmStartConfig' - The configuration for starting the hyperparameter parameter tuning job
-- using one or more previous tuning jobs as a starting point. The results
-- of previous tuning jobs are used to inform which combinations of
-- hyperparameters to search over in the new tuning job.
--
-- 'hyperParameterTuningEndTime', 'describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime' - The date and time that the tuning job ended.
--
-- 'failureReason', 'describeHyperParameterTuningJobResponse_failureReason' - If the tuning job failed, the reason it failed.
--
-- 'trainingJobDefinitions', 'describeHyperParameterTuningJobResponse_trainingJobDefinitions' - A list of the HyperParameterTrainingJobDefinition objects launched for
-- this tuning job.
--
-- 'lastModifiedTime', 'describeHyperParameterTuningJobResponse_lastModifiedTime' - The date and time that the status of the tuning job was modified.
--
-- 'overallBestTrainingJob', 'describeHyperParameterTuningJobResponse_overallBestTrainingJob' - If the hyperparameter tuning job is an warm start tuning job with a
-- @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@, this is the
-- TrainingJobSummary for the training job with the best objective metric
-- value of all training jobs launched by this tuning job and all parent
-- jobs specified for the warm start tuning job.
--
-- 'trainingJobDefinition', 'describeHyperParameterTuningJobResponse_trainingJobDefinition' - The HyperParameterTrainingJobDefinition object that specifies the
-- definition of the training jobs that this tuning job launches.
--
-- 'httpStatus', 'describeHyperParameterTuningJobResponse_httpStatus' - The response's http status code.
--
-- 'hyperParameterTuningJobName', 'describeHyperParameterTuningJobResponse_hyperParameterTuningJobName' - The name of the tuning job.
--
-- 'hyperParameterTuningJobArn', 'describeHyperParameterTuningJobResponse_hyperParameterTuningJobArn' - The Amazon Resource Name (ARN) of the tuning job.
--
-- 'hyperParameterTuningJobConfig', 'describeHyperParameterTuningJobResponse_hyperParameterTuningJobConfig' - The HyperParameterTuningJobConfig object that specifies the
-- configuration of the tuning job.
--
-- 'hyperParameterTuningJobStatus', 'describeHyperParameterTuningJobResponse_hyperParameterTuningJobStatus' - The status of the tuning job: InProgress, Completed, Failed, Stopping,
-- or Stopped.
--
-- 'creationTime', 'describeHyperParameterTuningJobResponse_creationTime' - The date and time that the tuning job started.
--
-- 'trainingJobStatusCounters', 'describeHyperParameterTuningJobResponse_trainingJobStatusCounters' - The TrainingJobStatusCounters object that specifies the number of
-- training jobs, categorized by status, that this tuning job launched.
--
-- 'objectiveStatusCounters', 'describeHyperParameterTuningJobResponse_objectiveStatusCounters' - The ObjectiveStatusCounters object that specifies the number of training
-- jobs, categorized by the status of their final objective metric, that
-- this tuning job launched.
newDescribeHyperParameterTuningJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hyperParameterTuningJobName'
  Prelude.Text ->
  -- | 'hyperParameterTuningJobArn'
  Prelude.Text ->
  -- | 'hyperParameterTuningJobConfig'
  HyperParameterTuningJobConfig ->
  -- | 'hyperParameterTuningJobStatus'
  HyperParameterTuningJobStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'trainingJobStatusCounters'
  TrainingJobStatusCounters ->
  -- | 'objectiveStatusCounters'
  ObjectiveStatusCounters ->
  DescribeHyperParameterTuningJobResponse
newDescribeHyperParameterTuningJobResponse
  pHttpStatus_
  pHyperParameterTuningJobName_
  pHyperParameterTuningJobArn_
  pHyperParameterTuningJobConfig_
  pHyperParameterTuningJobStatus_
  pCreationTime_
  pTrainingJobStatusCounters_
  pObjectiveStatusCounters_ =
    DescribeHyperParameterTuningJobResponse'
      { bestTrainingJob =
          Prelude.Nothing,
        warmStartConfig = Prelude.Nothing,
        hyperParameterTuningEndTime =
          Prelude.Nothing,
        failureReason = Prelude.Nothing,
        trainingJobDefinitions =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        overallBestTrainingJob =
          Prelude.Nothing,
        trainingJobDefinition =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        hyperParameterTuningJobName =
          pHyperParameterTuningJobName_,
        hyperParameterTuningJobArn =
          pHyperParameterTuningJobArn_,
        hyperParameterTuningJobConfig =
          pHyperParameterTuningJobConfig_,
        hyperParameterTuningJobStatus =
          pHyperParameterTuningJobStatus_,
        creationTime =
          Prelude._Time
            Lens.# pCreationTime_,
        trainingJobStatusCounters =
          pTrainingJobStatusCounters_,
        objectiveStatusCounters =
          pObjectiveStatusCounters_
      }

-- | A TrainingJobSummary object that describes the training job that
-- completed with the best current HyperParameterTuningJobObjective.
describeHyperParameterTuningJobResponse_bestTrainingJob :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe HyperParameterTrainingJobSummary)
describeHyperParameterTuningJobResponse_bestTrainingJob = Lens.lens (\DescribeHyperParameterTuningJobResponse' {bestTrainingJob} -> bestTrainingJob) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {bestTrainingJob = a} :: DescribeHyperParameterTuningJobResponse)

-- | The configuration for starting the hyperparameter parameter tuning job
-- using one or more previous tuning jobs as a starting point. The results
-- of previous tuning jobs are used to inform which combinations of
-- hyperparameters to search over in the new tuning job.
describeHyperParameterTuningJobResponse_warmStartConfig :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe HyperParameterTuningJobWarmStartConfig)
describeHyperParameterTuningJobResponse_warmStartConfig = Lens.lens (\DescribeHyperParameterTuningJobResponse' {warmStartConfig} -> warmStartConfig) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {warmStartConfig = a} :: DescribeHyperParameterTuningJobResponse)

-- | The date and time that the tuning job ended.
describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe Prelude.UTCTime)
describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime = Lens.lens (\DescribeHyperParameterTuningJobResponse' {hyperParameterTuningEndTime} -> hyperParameterTuningEndTime) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {hyperParameterTuningEndTime = a} :: DescribeHyperParameterTuningJobResponse) Prelude.. Lens.mapping Prelude._Time

-- | If the tuning job failed, the reason it failed.
describeHyperParameterTuningJobResponse_failureReason :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe Prelude.Text)
describeHyperParameterTuningJobResponse_failureReason = Lens.lens (\DescribeHyperParameterTuningJobResponse' {failureReason} -> failureReason) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {failureReason = a} :: DescribeHyperParameterTuningJobResponse)

-- | A list of the HyperParameterTrainingJobDefinition objects launched for
-- this tuning job.
describeHyperParameterTuningJobResponse_trainingJobDefinitions :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe (Prelude.NonEmpty HyperParameterTrainingJobDefinition))
describeHyperParameterTuningJobResponse_trainingJobDefinitions = Lens.lens (\DescribeHyperParameterTuningJobResponse' {trainingJobDefinitions} -> trainingJobDefinitions) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {trainingJobDefinitions = a} :: DescribeHyperParameterTuningJobResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The date and time that the status of the tuning job was modified.
describeHyperParameterTuningJobResponse_lastModifiedTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe Prelude.UTCTime)
describeHyperParameterTuningJobResponse_lastModifiedTime = Lens.lens (\DescribeHyperParameterTuningJobResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {lastModifiedTime = a} :: DescribeHyperParameterTuningJobResponse) Prelude.. Lens.mapping Prelude._Time

-- | If the hyperparameter tuning job is an warm start tuning job with a
-- @WarmStartType@ of @IDENTICAL_DATA_AND_ALGORITHM@, this is the
-- TrainingJobSummary for the training job with the best objective metric
-- value of all training jobs launched by this tuning job and all parent
-- jobs specified for the warm start tuning job.
describeHyperParameterTuningJobResponse_overallBestTrainingJob :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe HyperParameterTrainingJobSummary)
describeHyperParameterTuningJobResponse_overallBestTrainingJob = Lens.lens (\DescribeHyperParameterTuningJobResponse' {overallBestTrainingJob} -> overallBestTrainingJob) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {overallBestTrainingJob = a} :: DescribeHyperParameterTuningJobResponse)

-- | The HyperParameterTrainingJobDefinition object that specifies the
-- definition of the training jobs that this tuning job launches.
describeHyperParameterTuningJobResponse_trainingJobDefinition :: Lens.Lens' DescribeHyperParameterTuningJobResponse (Prelude.Maybe HyperParameterTrainingJobDefinition)
describeHyperParameterTuningJobResponse_trainingJobDefinition = Lens.lens (\DescribeHyperParameterTuningJobResponse' {trainingJobDefinition} -> trainingJobDefinition) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {trainingJobDefinition = a} :: DescribeHyperParameterTuningJobResponse)

-- | The response's http status code.
describeHyperParameterTuningJobResponse_httpStatus :: Lens.Lens' DescribeHyperParameterTuningJobResponse Prelude.Int
describeHyperParameterTuningJobResponse_httpStatus = Lens.lens (\DescribeHyperParameterTuningJobResponse' {httpStatus} -> httpStatus) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {httpStatus = a} :: DescribeHyperParameterTuningJobResponse)

-- | The name of the tuning job.
describeHyperParameterTuningJobResponse_hyperParameterTuningJobName :: Lens.Lens' DescribeHyperParameterTuningJobResponse Prelude.Text
describeHyperParameterTuningJobResponse_hyperParameterTuningJobName = Lens.lens (\DescribeHyperParameterTuningJobResponse' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {hyperParameterTuningJobName = a} :: DescribeHyperParameterTuningJobResponse)

-- | The Amazon Resource Name (ARN) of the tuning job.
describeHyperParameterTuningJobResponse_hyperParameterTuningJobArn :: Lens.Lens' DescribeHyperParameterTuningJobResponse Prelude.Text
describeHyperParameterTuningJobResponse_hyperParameterTuningJobArn = Lens.lens (\DescribeHyperParameterTuningJobResponse' {hyperParameterTuningJobArn} -> hyperParameterTuningJobArn) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {hyperParameterTuningJobArn = a} :: DescribeHyperParameterTuningJobResponse)

-- | The HyperParameterTuningJobConfig object that specifies the
-- configuration of the tuning job.
describeHyperParameterTuningJobResponse_hyperParameterTuningJobConfig :: Lens.Lens' DescribeHyperParameterTuningJobResponse HyperParameterTuningJobConfig
describeHyperParameterTuningJobResponse_hyperParameterTuningJobConfig = Lens.lens (\DescribeHyperParameterTuningJobResponse' {hyperParameterTuningJobConfig} -> hyperParameterTuningJobConfig) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {hyperParameterTuningJobConfig = a} :: DescribeHyperParameterTuningJobResponse)

-- | The status of the tuning job: InProgress, Completed, Failed, Stopping,
-- or Stopped.
describeHyperParameterTuningJobResponse_hyperParameterTuningJobStatus :: Lens.Lens' DescribeHyperParameterTuningJobResponse HyperParameterTuningJobStatus
describeHyperParameterTuningJobResponse_hyperParameterTuningJobStatus = Lens.lens (\DescribeHyperParameterTuningJobResponse' {hyperParameterTuningJobStatus} -> hyperParameterTuningJobStatus) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {hyperParameterTuningJobStatus = a} :: DescribeHyperParameterTuningJobResponse)

-- | The date and time that the tuning job started.
describeHyperParameterTuningJobResponse_creationTime :: Lens.Lens' DescribeHyperParameterTuningJobResponse Prelude.UTCTime
describeHyperParameterTuningJobResponse_creationTime = Lens.lens (\DescribeHyperParameterTuningJobResponse' {creationTime} -> creationTime) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {creationTime = a} :: DescribeHyperParameterTuningJobResponse) Prelude.. Prelude._Time

-- | The TrainingJobStatusCounters object that specifies the number of
-- training jobs, categorized by status, that this tuning job launched.
describeHyperParameterTuningJobResponse_trainingJobStatusCounters :: Lens.Lens' DescribeHyperParameterTuningJobResponse TrainingJobStatusCounters
describeHyperParameterTuningJobResponse_trainingJobStatusCounters = Lens.lens (\DescribeHyperParameterTuningJobResponse' {trainingJobStatusCounters} -> trainingJobStatusCounters) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {trainingJobStatusCounters = a} :: DescribeHyperParameterTuningJobResponse)

-- | The ObjectiveStatusCounters object that specifies the number of training
-- jobs, categorized by the status of their final objective metric, that
-- this tuning job launched.
describeHyperParameterTuningJobResponse_objectiveStatusCounters :: Lens.Lens' DescribeHyperParameterTuningJobResponse ObjectiveStatusCounters
describeHyperParameterTuningJobResponse_objectiveStatusCounters = Lens.lens (\DescribeHyperParameterTuningJobResponse' {objectiveStatusCounters} -> objectiveStatusCounters) (\s@DescribeHyperParameterTuningJobResponse' {} a -> s {objectiveStatusCounters = a} :: DescribeHyperParameterTuningJobResponse)

instance
  Prelude.NFData
    DescribeHyperParameterTuningJobResponse
