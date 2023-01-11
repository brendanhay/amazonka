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
-- Module      : Amazonka.EMRServerless.Types.JobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.JobRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types.ConfigurationOverrides
import Amazonka.EMRServerless.Types.JobDriver
import Amazonka.EMRServerless.Types.JobRunState
import Amazonka.EMRServerless.Types.NetworkConfiguration
import Amazonka.EMRServerless.Types.TotalResourceUtilization
import qualified Amazonka.Prelude as Prelude

-- | Information about a job run. A job run is a unit of work, such as a
-- Spark JAR, Hive query, or SparkSQL query, that you submit to an EMR
-- Serverless application.
--
-- /See:/ 'newJobRun' smart constructor.
data JobRun = JobRun'
  { -- | The configuration settings that are used to override default
    -- configuration.
    configurationOverrides :: Prelude.Maybe ConfigurationOverrides,
    -- | The optional job run name. This doesn\'t have to be unique.
    name :: Prelude.Maybe Prelude.Text,
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The tags assigned to the job run.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The job run total execution duration in seconds. This field is only
    -- available for job runs in a @COMPLETED@, @FAILED@, or @CANCELLED@ state.
    totalExecutionDurationSeconds :: Prelude.Maybe Prelude.Int,
    -- | The aggregate vCPU, memory, and storage resources used from the time job
    -- start executing till the time job is terminated, rounded up to the
    -- nearest second.
    totalResourceUtilization :: Prelude.Maybe TotalResourceUtilization,
    -- | The ID of the application the job is running on.
    applicationId :: Prelude.Text,
    -- | The ID of the job run.
    jobRunId :: Prelude.Text,
    -- | The execution role ARN of the job run.
    arn :: Prelude.Text,
    -- | The user who created the job run.
    createdBy :: Prelude.Text,
    -- | The date and time when the job run was created.
    createdAt :: Data.POSIX,
    -- | The date and time when the job run was updated.
    updatedAt :: Data.POSIX,
    -- | The execution role ARN of the job run.
    executionRole :: Prelude.Text,
    -- | The state of the job run.
    state :: JobRunState,
    -- | The state details of the job run.
    stateDetails :: Prelude.Text,
    -- | The EMR release associated with the application your job is running on.
    releaseLabel :: Prelude.Text,
    -- | The job driver for the job run.
    jobDriver :: JobDriver
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationOverrides', 'jobRun_configurationOverrides' - The configuration settings that are used to override default
-- configuration.
--
-- 'name', 'jobRun_name' - The optional job run name. This doesn\'t have to be unique.
--
-- 'networkConfiguration', 'jobRun_networkConfiguration' - Undocumented member.
--
-- 'tags', 'jobRun_tags' - The tags assigned to the job run.
--
-- 'totalExecutionDurationSeconds', 'jobRun_totalExecutionDurationSeconds' - The job run total execution duration in seconds. This field is only
-- available for job runs in a @COMPLETED@, @FAILED@, or @CANCELLED@ state.
--
-- 'totalResourceUtilization', 'jobRun_totalResourceUtilization' - The aggregate vCPU, memory, and storage resources used from the time job
-- start executing till the time job is terminated, rounded up to the
-- nearest second.
--
-- 'applicationId', 'jobRun_applicationId' - The ID of the application the job is running on.
--
-- 'jobRunId', 'jobRun_jobRunId' - The ID of the job run.
--
-- 'arn', 'jobRun_arn' - The execution role ARN of the job run.
--
-- 'createdBy', 'jobRun_createdBy' - The user who created the job run.
--
-- 'createdAt', 'jobRun_createdAt' - The date and time when the job run was created.
--
-- 'updatedAt', 'jobRun_updatedAt' - The date and time when the job run was updated.
--
-- 'executionRole', 'jobRun_executionRole' - The execution role ARN of the job run.
--
-- 'state', 'jobRun_state' - The state of the job run.
--
-- 'stateDetails', 'jobRun_stateDetails' - The state details of the job run.
--
-- 'releaseLabel', 'jobRun_releaseLabel' - The EMR release associated with the application your job is running on.
--
-- 'jobDriver', 'jobRun_jobDriver' - The job driver for the job run.
newJobRun ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'jobRunId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdBy'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'executionRole'
  Prelude.Text ->
  -- | 'state'
  JobRunState ->
  -- | 'stateDetails'
  Prelude.Text ->
  -- | 'releaseLabel'
  Prelude.Text ->
  -- | 'jobDriver'
  JobDriver ->
  JobRun
newJobRun
  pApplicationId_
  pJobRunId_
  pArn_
  pCreatedBy_
  pCreatedAt_
  pUpdatedAt_
  pExecutionRole_
  pState_
  pStateDetails_
  pReleaseLabel_
  pJobDriver_ =
    JobRun'
      { configurationOverrides = Prelude.Nothing,
        name = Prelude.Nothing,
        networkConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        totalExecutionDurationSeconds = Prelude.Nothing,
        totalResourceUtilization = Prelude.Nothing,
        applicationId = pApplicationId_,
        jobRunId = pJobRunId_,
        arn = pArn_,
        createdBy = pCreatedBy_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        executionRole = pExecutionRole_,
        state = pState_,
        stateDetails = pStateDetails_,
        releaseLabel = pReleaseLabel_,
        jobDriver = pJobDriver_
      }

-- | The configuration settings that are used to override default
-- configuration.
jobRun_configurationOverrides :: Lens.Lens' JobRun (Prelude.Maybe ConfigurationOverrides)
jobRun_configurationOverrides = Lens.lens (\JobRun' {configurationOverrides} -> configurationOverrides) (\s@JobRun' {} a -> s {configurationOverrides = a} :: JobRun)

-- | The optional job run name. This doesn\'t have to be unique.
jobRun_name :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Text)
jobRun_name = Lens.lens (\JobRun' {name} -> name) (\s@JobRun' {} a -> s {name = a} :: JobRun)

-- | Undocumented member.
jobRun_networkConfiguration :: Lens.Lens' JobRun (Prelude.Maybe NetworkConfiguration)
jobRun_networkConfiguration = Lens.lens (\JobRun' {networkConfiguration} -> networkConfiguration) (\s@JobRun' {} a -> s {networkConfiguration = a} :: JobRun)

-- | The tags assigned to the job run.
jobRun_tags :: Lens.Lens' JobRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobRun_tags = Lens.lens (\JobRun' {tags} -> tags) (\s@JobRun' {} a -> s {tags = a} :: JobRun) Prelude.. Lens.mapping Lens.coerced

-- | The job run total execution duration in seconds. This field is only
-- available for job runs in a @COMPLETED@, @FAILED@, or @CANCELLED@ state.
jobRun_totalExecutionDurationSeconds :: Lens.Lens' JobRun (Prelude.Maybe Prelude.Int)
jobRun_totalExecutionDurationSeconds = Lens.lens (\JobRun' {totalExecutionDurationSeconds} -> totalExecutionDurationSeconds) (\s@JobRun' {} a -> s {totalExecutionDurationSeconds = a} :: JobRun)

-- | The aggregate vCPU, memory, and storage resources used from the time job
-- start executing till the time job is terminated, rounded up to the
-- nearest second.
jobRun_totalResourceUtilization :: Lens.Lens' JobRun (Prelude.Maybe TotalResourceUtilization)
jobRun_totalResourceUtilization = Lens.lens (\JobRun' {totalResourceUtilization} -> totalResourceUtilization) (\s@JobRun' {} a -> s {totalResourceUtilization = a} :: JobRun)

-- | The ID of the application the job is running on.
jobRun_applicationId :: Lens.Lens' JobRun Prelude.Text
jobRun_applicationId = Lens.lens (\JobRun' {applicationId} -> applicationId) (\s@JobRun' {} a -> s {applicationId = a} :: JobRun)

-- | The ID of the job run.
jobRun_jobRunId :: Lens.Lens' JobRun Prelude.Text
jobRun_jobRunId = Lens.lens (\JobRun' {jobRunId} -> jobRunId) (\s@JobRun' {} a -> s {jobRunId = a} :: JobRun)

-- | The execution role ARN of the job run.
jobRun_arn :: Lens.Lens' JobRun Prelude.Text
jobRun_arn = Lens.lens (\JobRun' {arn} -> arn) (\s@JobRun' {} a -> s {arn = a} :: JobRun)

-- | The user who created the job run.
jobRun_createdBy :: Lens.Lens' JobRun Prelude.Text
jobRun_createdBy = Lens.lens (\JobRun' {createdBy} -> createdBy) (\s@JobRun' {} a -> s {createdBy = a} :: JobRun)

-- | The date and time when the job run was created.
jobRun_createdAt :: Lens.Lens' JobRun Prelude.UTCTime
jobRun_createdAt = Lens.lens (\JobRun' {createdAt} -> createdAt) (\s@JobRun' {} a -> s {createdAt = a} :: JobRun) Prelude.. Data._Time

-- | The date and time when the job run was updated.
jobRun_updatedAt :: Lens.Lens' JobRun Prelude.UTCTime
jobRun_updatedAt = Lens.lens (\JobRun' {updatedAt} -> updatedAt) (\s@JobRun' {} a -> s {updatedAt = a} :: JobRun) Prelude.. Data._Time

-- | The execution role ARN of the job run.
jobRun_executionRole :: Lens.Lens' JobRun Prelude.Text
jobRun_executionRole = Lens.lens (\JobRun' {executionRole} -> executionRole) (\s@JobRun' {} a -> s {executionRole = a} :: JobRun)

-- | The state of the job run.
jobRun_state :: Lens.Lens' JobRun JobRunState
jobRun_state = Lens.lens (\JobRun' {state} -> state) (\s@JobRun' {} a -> s {state = a} :: JobRun)

-- | The state details of the job run.
jobRun_stateDetails :: Lens.Lens' JobRun Prelude.Text
jobRun_stateDetails = Lens.lens (\JobRun' {stateDetails} -> stateDetails) (\s@JobRun' {} a -> s {stateDetails = a} :: JobRun)

-- | The EMR release associated with the application your job is running on.
jobRun_releaseLabel :: Lens.Lens' JobRun Prelude.Text
jobRun_releaseLabel = Lens.lens (\JobRun' {releaseLabel} -> releaseLabel) (\s@JobRun' {} a -> s {releaseLabel = a} :: JobRun)

-- | The job driver for the job run.
jobRun_jobDriver :: Lens.Lens' JobRun JobDriver
jobRun_jobDriver = Lens.lens (\JobRun' {jobDriver} -> jobDriver) (\s@JobRun' {} a -> s {jobDriver = a} :: JobRun)

instance Data.FromJSON JobRun where
  parseJSON =
    Data.withObject
      "JobRun"
      ( \x ->
          JobRun'
            Prelude.<$> (x Data..:? "configurationOverrides")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "networkConfiguration")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "totalExecutionDurationSeconds")
            Prelude.<*> (x Data..:? "totalResourceUtilization")
            Prelude.<*> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "jobRunId")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdBy")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
            Prelude.<*> (x Data..: "executionRole")
            Prelude.<*> (x Data..: "state")
            Prelude.<*> (x Data..: "stateDetails")
            Prelude.<*> (x Data..: "releaseLabel")
            Prelude.<*> (x Data..: "jobDriver")
      )

instance Prelude.Hashable JobRun where
  hashWithSalt _salt JobRun' {..} =
    _salt `Prelude.hashWithSalt` configurationOverrides
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` totalExecutionDurationSeconds
      `Prelude.hashWithSalt` totalResourceUtilization
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` jobRunId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateDetails
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` jobDriver

instance Prelude.NFData JobRun where
  rnf JobRun' {..} =
    Prelude.rnf configurationOverrides
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf totalExecutionDurationSeconds
      `Prelude.seq` Prelude.rnf totalResourceUtilization
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf jobRunId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateDetails
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf jobDriver
