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
-- Module      : Amazonka.Batch.Types.JobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.JobSummary where

import Amazonka.Batch.Types.ArrayPropertiesSummary
import Amazonka.Batch.Types.ContainerSummary
import Amazonka.Batch.Types.JobStatus
import Amazonka.Batch.Types.NodePropertiesSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents summary details of a job.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The array properties of the job, if it\'s an array job.
    arrayProperties :: Prelude.Maybe ArrayPropertiesSummary,
    -- | An object that represents the details of the container that\'s
    -- associated with the job.
    container :: Prelude.Maybe ContainerSummary,
    -- | The Unix timestamp (in milliseconds) for when the job was created. For
    -- non-array jobs and parent array jobs, this is when the job entered the
    -- @SUBMITTED@ state (at the time SubmitJob was called). For array child
    -- jobs, this is when the child job was spawned by its parent and entered
    -- the @PENDING@ state.
    createdAt :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the job definition.
    jobDefinition :: Prelude.Maybe Prelude.Text,
    -- | The node properties for a single node in a job summary list.
    --
    -- This isn\'t applicable to jobs that are running on Fargate resources.
    nodeProperties :: Prelude.Maybe NodePropertiesSummary,
    -- | The Unix timestamp for when the job was started. More specifically,
    -- it\'s when the job transitioned from the @STARTING@ state to the
    -- @RUNNING@ state.
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The current status for the job.
    status :: Prelude.Maybe JobStatus,
    -- | A short, human-readable string to provide more details for the current
    -- status of the job.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the job was stopped. More specifically,
    -- it\'s when the job transitioned from the @RUNNING@ state to a terminal
    -- state, such as @SUCCEEDED@ or @FAILED@.
    stoppedAt :: Prelude.Maybe Prelude.Integer,
    -- | The job ID.
    jobId :: Prelude.Text,
    -- | The job name.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arrayProperties', 'jobSummary_arrayProperties' - The array properties of the job, if it\'s an array job.
--
-- 'container', 'jobSummary_container' - An object that represents the details of the container that\'s
-- associated with the job.
--
-- 'createdAt', 'jobSummary_createdAt' - The Unix timestamp (in milliseconds) for when the job was created. For
-- non-array jobs and parent array jobs, this is when the job entered the
-- @SUBMITTED@ state (at the time SubmitJob was called). For array child
-- jobs, this is when the child job was spawned by its parent and entered
-- the @PENDING@ state.
--
-- 'jobArn', 'jobSummary_jobArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'jobDefinition', 'jobSummary_jobDefinition' - The Amazon Resource Name (ARN) of the job definition.
--
-- 'nodeProperties', 'jobSummary_nodeProperties' - The node properties for a single node in a job summary list.
--
-- This isn\'t applicable to jobs that are running on Fargate resources.
--
-- 'startedAt', 'jobSummary_startedAt' - The Unix timestamp for when the job was started. More specifically,
-- it\'s when the job transitioned from the @STARTING@ state to the
-- @RUNNING@ state.
--
-- 'status', 'jobSummary_status' - The current status for the job.
--
-- 'statusReason', 'jobSummary_statusReason' - A short, human-readable string to provide more details for the current
-- status of the job.
--
-- 'stoppedAt', 'jobSummary_stoppedAt' - The Unix timestamp for when the job was stopped. More specifically,
-- it\'s when the job transitioned from the @RUNNING@ state to a terminal
-- state, such as @SUCCEEDED@ or @FAILED@.
--
-- 'jobId', 'jobSummary_jobId' - The job ID.
--
-- 'jobName', 'jobSummary_jobName' - The job name.
newJobSummary ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  JobSummary
newJobSummary pJobId_ pJobName_ =
  JobSummary'
    { arrayProperties = Prelude.Nothing,
      container = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobDefinition = Prelude.Nothing,
      nodeProperties = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      stoppedAt = Prelude.Nothing,
      jobId = pJobId_,
      jobName = pJobName_
    }

-- | The array properties of the job, if it\'s an array job.
jobSummary_arrayProperties :: Lens.Lens' JobSummary (Prelude.Maybe ArrayPropertiesSummary)
jobSummary_arrayProperties = Lens.lens (\JobSummary' {arrayProperties} -> arrayProperties) (\s@JobSummary' {} a -> s {arrayProperties = a} :: JobSummary)

-- | An object that represents the details of the container that\'s
-- associated with the job.
jobSummary_container :: Lens.Lens' JobSummary (Prelude.Maybe ContainerSummary)
jobSummary_container = Lens.lens (\JobSummary' {container} -> container) (\s@JobSummary' {} a -> s {container = a} :: JobSummary)

-- | The Unix timestamp (in milliseconds) for when the job was created. For
-- non-array jobs and parent array jobs, this is when the job entered the
-- @SUBMITTED@ state (at the time SubmitJob was called). For array child
-- jobs, this is when the child job was spawned by its parent and entered
-- the @PENDING@ state.
jobSummary_createdAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Integer)
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary)

-- | The Amazon Resource Name (ARN) of the job.
jobSummary_jobArn :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The Amazon Resource Name (ARN) of the job definition.
jobSummary_jobDefinition :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobDefinition = Lens.lens (\JobSummary' {jobDefinition} -> jobDefinition) (\s@JobSummary' {} a -> s {jobDefinition = a} :: JobSummary)

-- | The node properties for a single node in a job summary list.
--
-- This isn\'t applicable to jobs that are running on Fargate resources.
jobSummary_nodeProperties :: Lens.Lens' JobSummary (Prelude.Maybe NodePropertiesSummary)
jobSummary_nodeProperties = Lens.lens (\JobSummary' {nodeProperties} -> nodeProperties) (\s@JobSummary' {} a -> s {nodeProperties = a} :: JobSummary)

-- | The Unix timestamp for when the job was started. More specifically,
-- it\'s when the job transitioned from the @STARTING@ state to the
-- @RUNNING@ state.
jobSummary_startedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Integer)
jobSummary_startedAt = Lens.lens (\JobSummary' {startedAt} -> startedAt) (\s@JobSummary' {} a -> s {startedAt = a} :: JobSummary)

-- | The current status for the job.
jobSummary_status :: Lens.Lens' JobSummary (Prelude.Maybe JobStatus)
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

-- | A short, human-readable string to provide more details for the current
-- status of the job.
jobSummary_statusReason :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_statusReason = Lens.lens (\JobSummary' {statusReason} -> statusReason) (\s@JobSummary' {} a -> s {statusReason = a} :: JobSummary)

-- | The Unix timestamp for when the job was stopped. More specifically,
-- it\'s when the job transitioned from the @RUNNING@ state to a terminal
-- state, such as @SUCCEEDED@ or @FAILED@.
jobSummary_stoppedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Integer)
jobSummary_stoppedAt = Lens.lens (\JobSummary' {stoppedAt} -> stoppedAt) (\s@JobSummary' {} a -> s {stoppedAt = a} :: JobSummary)

-- | The job ID.
jobSummary_jobId :: Lens.Lens' JobSummary Prelude.Text
jobSummary_jobId = Lens.lens (\JobSummary' {jobId} -> jobId) (\s@JobSummary' {} a -> s {jobId = a} :: JobSummary)

-- | The job name.
jobSummary_jobName :: Lens.Lens' JobSummary Prelude.Text
jobSummary_jobName = Lens.lens (\JobSummary' {jobName} -> jobName) (\s@JobSummary' {} a -> s {jobName = a} :: JobSummary)

instance Data.FromJSON JobSummary where
  parseJSON =
    Data.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Prelude.<$> (x Data..:? "arrayProperties")
            Prelude.<*> (x Data..:? "container")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "jobArn")
            Prelude.<*> (x Data..:? "jobDefinition")
            Prelude.<*> (x Data..:? "nodeProperties")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "stoppedAt")
            Prelude.<*> (x Data..: "jobId")
            Prelude.<*> (x Data..: "jobName")
      )

instance Prelude.Hashable JobSummary where
  hashWithSalt _salt JobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arrayProperties
      `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` jobDefinition
      `Prelude.hashWithSalt` nodeProperties
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` stoppedAt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData JobSummary where
  rnf JobSummary' {..} =
    Prelude.rnf arrayProperties
      `Prelude.seq` Prelude.rnf container
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobDefinition
      `Prelude.seq` Prelude.rnf nodeProperties
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf stoppedAt
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobName
