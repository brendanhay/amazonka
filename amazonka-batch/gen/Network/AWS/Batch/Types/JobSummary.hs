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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing summary details of a job.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | An object representing the details of the container that\'s associated
    -- with the job.
    container :: Core.Maybe ContainerSummary,
    -- | The Unix timestamp for when the job was started (when the job
    -- transitioned from the @STARTING@ state to the @RUNNING@ state).
    startedAt :: Core.Maybe Core.Integer,
    -- | The current status for the job.
    status :: Core.Maybe JobStatus,
    -- | The array properties of the job, if it is an array job.
    arrayProperties :: Core.Maybe ArrayPropertiesSummary,
    -- | The Unix timestamp for when the job was created. For non-array jobs and
    -- parent array jobs, this is when the job entered the @SUBMITTED@ state
    -- (at the time SubmitJob was called). For array child jobs, this is when
    -- the child job was spawned by its parent and entered the @PENDING@ state.
    createdAt :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Core.Maybe Core.Text,
    -- | The Unix timestamp for when the job was stopped (when the job
    -- transitioned from the @RUNNING@ state to a terminal state, such as
    -- @SUCCEEDED@ or @FAILED@).
    stoppedAt :: Core.Maybe Core.Integer,
    -- | The node properties for a single node in a job summary list.
    --
    -- This isn\'t applicable to jobs running on Fargate resources.
    nodeProperties :: Core.Maybe NodePropertiesSummary,
    -- | A short, human-readable string to provide additional details about the
    -- current status of the job.
    statusReason :: Core.Maybe Core.Text,
    -- | The ID of the job.
    jobId :: Core.Text,
    -- | The name of the job.
    jobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'jobName'
  Core.Text ->
  JobSummary
newJobSummary pJobId_ pJobName_ =
  JobSummary'
    { container = Core.Nothing,
      startedAt = Core.Nothing,
      status = Core.Nothing,
      arrayProperties = Core.Nothing,
      createdAt = Core.Nothing,
      jobArn = Core.Nothing,
      stoppedAt = Core.Nothing,
      nodeProperties = Core.Nothing,
      statusReason = Core.Nothing,
      jobId = pJobId_,
      jobName = pJobName_
    }

-- | An object representing the details of the container that\'s associated
-- with the job.
jobSummary_container :: Lens.Lens' JobSummary (Core.Maybe ContainerSummary)
jobSummary_container = Lens.lens (\JobSummary' {container} -> container) (\s@JobSummary' {} a -> s {container = a} :: JobSummary)

-- | The Unix timestamp for when the job was started (when the job
-- transitioned from the @STARTING@ state to the @RUNNING@ state).
jobSummary_startedAt :: Lens.Lens' JobSummary (Core.Maybe Core.Integer)
jobSummary_startedAt = Lens.lens (\JobSummary' {startedAt} -> startedAt) (\s@JobSummary' {} a -> s {startedAt = a} :: JobSummary)

-- | The current status for the job.
jobSummary_status :: Lens.Lens' JobSummary (Core.Maybe JobStatus)
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

-- | The array properties of the job, if it is an array job.
jobSummary_arrayProperties :: Lens.Lens' JobSummary (Core.Maybe ArrayPropertiesSummary)
jobSummary_arrayProperties = Lens.lens (\JobSummary' {arrayProperties} -> arrayProperties) (\s@JobSummary' {} a -> s {arrayProperties = a} :: JobSummary)

-- | The Unix timestamp for when the job was created. For non-array jobs and
-- parent array jobs, this is when the job entered the @SUBMITTED@ state
-- (at the time SubmitJob was called). For array child jobs, this is when
-- the child job was spawned by its parent and entered the @PENDING@ state.
jobSummary_createdAt :: Lens.Lens' JobSummary (Core.Maybe Core.Integer)
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary)

-- | The Amazon Resource Name (ARN) of the job.
jobSummary_jobArn :: Lens.Lens' JobSummary (Core.Maybe Core.Text)
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The Unix timestamp for when the job was stopped (when the job
-- transitioned from the @RUNNING@ state to a terminal state, such as
-- @SUCCEEDED@ or @FAILED@).
jobSummary_stoppedAt :: Lens.Lens' JobSummary (Core.Maybe Core.Integer)
jobSummary_stoppedAt = Lens.lens (\JobSummary' {stoppedAt} -> stoppedAt) (\s@JobSummary' {} a -> s {stoppedAt = a} :: JobSummary)

-- | The node properties for a single node in a job summary list.
--
-- This isn\'t applicable to jobs running on Fargate resources.
jobSummary_nodeProperties :: Lens.Lens' JobSummary (Core.Maybe NodePropertiesSummary)
jobSummary_nodeProperties = Lens.lens (\JobSummary' {nodeProperties} -> nodeProperties) (\s@JobSummary' {} a -> s {nodeProperties = a} :: JobSummary)

-- | A short, human-readable string to provide additional details about the
-- current status of the job.
jobSummary_statusReason :: Lens.Lens' JobSummary (Core.Maybe Core.Text)
jobSummary_statusReason = Lens.lens (\JobSummary' {statusReason} -> statusReason) (\s@JobSummary' {} a -> s {statusReason = a} :: JobSummary)

-- | The ID of the job.
jobSummary_jobId :: Lens.Lens' JobSummary Core.Text
jobSummary_jobId = Lens.lens (\JobSummary' {jobId} -> jobId) (\s@JobSummary' {} a -> s {jobId = a} :: JobSummary)

-- | The name of the job.
jobSummary_jobName :: Lens.Lens' JobSummary Core.Text
jobSummary_jobName = Lens.lens (\JobSummary' {jobName} -> jobName) (\s@JobSummary' {} a -> s {jobName = a} :: JobSummary)

instance Core.FromJSON JobSummary where
  parseJSON =
    Core.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Core.<$> (x Core..:? "container")
            Core.<*> (x Core..:? "startedAt")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "arrayProperties")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "jobArn")
            Core.<*> (x Core..:? "stoppedAt")
            Core.<*> (x Core..:? "nodeProperties")
            Core.<*> (x Core..:? "statusReason")
            Core.<*> (x Core..: "jobId")
            Core.<*> (x Core..: "jobName")
      )

instance Core.Hashable JobSummary

instance Core.NFData JobSummary
