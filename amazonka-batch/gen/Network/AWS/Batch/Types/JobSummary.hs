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
-- Module      : Network.AWS.Batch.Types.JobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobSummary where

import Network.AWS.Batch.Types.ArrayPropertiesSummary
import Network.AWS.Batch.Types.ContainerSummary
import Network.AWS.Batch.Types.JobStatus
import Network.AWS.Batch.Types.NodePropertiesSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing summary details of a job.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | An object representing the details of the container that\'s associated
    -- with the job.
    container :: Prelude.Maybe ContainerSummary,
    -- | The Unix timestamp for when the job was started (when the job
    -- transitioned from the @STARTING@ state to the @RUNNING@ state).
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The current status for the job.
    status :: Prelude.Maybe JobStatus,
    -- | The array properties of the job, if it is an array job.
    arrayProperties :: Prelude.Maybe ArrayPropertiesSummary,
    -- | The Unix timestamp for when the job was created. For non-array jobs and
    -- parent array jobs, this is when the job entered the @SUBMITTED@ state
    -- (at the time SubmitJob was called). For array child jobs, this is when
    -- the child job was spawned by its parent and entered the @PENDING@ state.
    createdAt :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the job was stopped (when the job
    -- transitioned from the @RUNNING@ state to a terminal state, such as
    -- @SUCCEEDED@ or @FAILED@).
    stoppedAt :: Prelude.Maybe Prelude.Integer,
    -- | The node properties for a single node in a job summary list.
    --
    -- This isn\'t applicable to jobs running on Fargate resources.
    nodeProperties :: Prelude.Maybe NodePropertiesSummary,
    -- | A short, human-readable string to provide additional details about the
    -- current status of the job.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the job.
    jobId :: Prelude.Text,
    -- | The name of the job.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'container', 'jobSummary_container' - An object representing the details of the container that\'s associated
-- with the job.
--
-- 'startedAt', 'jobSummary_startedAt' - The Unix timestamp for when the job was started (when the job
-- transitioned from the @STARTING@ state to the @RUNNING@ state).
--
-- 'status', 'jobSummary_status' - The current status for the job.
--
-- 'arrayProperties', 'jobSummary_arrayProperties' - The array properties of the job, if it is an array job.
--
-- 'createdAt', 'jobSummary_createdAt' - The Unix timestamp for when the job was created. For non-array jobs and
-- parent array jobs, this is when the job entered the @SUBMITTED@ state
-- (at the time SubmitJob was called). For array child jobs, this is when
-- the child job was spawned by its parent and entered the @PENDING@ state.
--
-- 'jobArn', 'jobSummary_jobArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'stoppedAt', 'jobSummary_stoppedAt' - The Unix timestamp for when the job was stopped (when the job
-- transitioned from the @RUNNING@ state to a terminal state, such as
-- @SUCCEEDED@ or @FAILED@).
--
-- 'nodeProperties', 'jobSummary_nodeProperties' - The node properties for a single node in a job summary list.
--
-- This isn\'t applicable to jobs running on Fargate resources.
--
-- 'statusReason', 'jobSummary_statusReason' - A short, human-readable string to provide additional details about the
-- current status of the job.
--
-- 'jobId', 'jobSummary_jobId' - The ID of the job.
--
-- 'jobName', 'jobSummary_jobName' - The name of the job.
newJobSummary ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  JobSummary
newJobSummary pJobId_ pJobName_ =
  JobSummary'
    { container = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      arrayProperties = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      stoppedAt = Prelude.Nothing,
      nodeProperties = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      jobId = pJobId_,
      jobName = pJobName_
    }

-- | An object representing the details of the container that\'s associated
-- with the job.
jobSummary_container :: Lens.Lens' JobSummary (Prelude.Maybe ContainerSummary)
jobSummary_container = Lens.lens (\JobSummary' {container} -> container) (\s@JobSummary' {} a -> s {container = a} :: JobSummary)

-- | The Unix timestamp for when the job was started (when the job
-- transitioned from the @STARTING@ state to the @RUNNING@ state).
jobSummary_startedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Integer)
jobSummary_startedAt = Lens.lens (\JobSummary' {startedAt} -> startedAt) (\s@JobSummary' {} a -> s {startedAt = a} :: JobSummary)

-- | The current status for the job.
jobSummary_status :: Lens.Lens' JobSummary (Prelude.Maybe JobStatus)
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

-- | The array properties of the job, if it is an array job.
jobSummary_arrayProperties :: Lens.Lens' JobSummary (Prelude.Maybe ArrayPropertiesSummary)
jobSummary_arrayProperties = Lens.lens (\JobSummary' {arrayProperties} -> arrayProperties) (\s@JobSummary' {} a -> s {arrayProperties = a} :: JobSummary)

-- | The Unix timestamp for when the job was created. For non-array jobs and
-- parent array jobs, this is when the job entered the @SUBMITTED@ state
-- (at the time SubmitJob was called). For array child jobs, this is when
-- the child job was spawned by its parent and entered the @PENDING@ state.
jobSummary_createdAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Integer)
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary)

-- | The Amazon Resource Name (ARN) of the job.
jobSummary_jobArn :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The Unix timestamp for when the job was stopped (when the job
-- transitioned from the @RUNNING@ state to a terminal state, such as
-- @SUCCEEDED@ or @FAILED@).
jobSummary_stoppedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Integer)
jobSummary_stoppedAt = Lens.lens (\JobSummary' {stoppedAt} -> stoppedAt) (\s@JobSummary' {} a -> s {stoppedAt = a} :: JobSummary)

-- | The node properties for a single node in a job summary list.
--
-- This isn\'t applicable to jobs running on Fargate resources.
jobSummary_nodeProperties :: Lens.Lens' JobSummary (Prelude.Maybe NodePropertiesSummary)
jobSummary_nodeProperties = Lens.lens (\JobSummary' {nodeProperties} -> nodeProperties) (\s@JobSummary' {} a -> s {nodeProperties = a} :: JobSummary)

-- | A short, human-readable string to provide additional details about the
-- current status of the job.
jobSummary_statusReason :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_statusReason = Lens.lens (\JobSummary' {statusReason} -> statusReason) (\s@JobSummary' {} a -> s {statusReason = a} :: JobSummary)

-- | The ID of the job.
jobSummary_jobId :: Lens.Lens' JobSummary Prelude.Text
jobSummary_jobId = Lens.lens (\JobSummary' {jobId} -> jobId) (\s@JobSummary' {} a -> s {jobId = a} :: JobSummary)

-- | The name of the job.
jobSummary_jobName :: Lens.Lens' JobSummary Prelude.Text
jobSummary_jobName = Lens.lens (\JobSummary' {jobName} -> jobName) (\s@JobSummary' {} a -> s {jobName = a} :: JobSummary)

instance Prelude.FromJSON JobSummary where
  parseJSON =
    Prelude.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Prelude.<$> (x Prelude..:? "container")
            Prelude.<*> (x Prelude..:? "startedAt")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "arrayProperties")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "jobArn")
            Prelude.<*> (x Prelude..:? "stoppedAt")
            Prelude.<*> (x Prelude..:? "nodeProperties")
            Prelude.<*> (x Prelude..:? "statusReason")
            Prelude.<*> (x Prelude..: "jobId")
            Prelude.<*> (x Prelude..: "jobName")
      )

instance Prelude.Hashable JobSummary

instance Prelude.NFData JobSummary
