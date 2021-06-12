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
-- Module      : Network.AWS.Batch.Types.JobDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDetail where

import Network.AWS.Batch.Types.ArrayPropertiesDetail
import Network.AWS.Batch.Types.AttemptDetail
import Network.AWS.Batch.Types.ContainerDetail
import Network.AWS.Batch.Types.JobDependency
import Network.AWS.Batch.Types.JobStatus
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.PlatformCapability
import Network.AWS.Batch.Types.RetryStrategy
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing an AWS Batch job.
--
-- /See:/ 'newJobDetail' smart constructor.
data JobDetail = JobDetail'
  { -- | An object representing the details of the container that\'s associated
    -- with the job.
    container :: Core.Maybe ContainerDetail,
    -- | The Unix timestamp (in milliseconds) for when the job was started (when
    -- the job transitioned from the @STARTING@ state to the @RUNNING@ state).
    -- This parameter isn\'t provided for child jobs of array jobs or
    -- multi-node parallel jobs.
    startedAt :: Core.Maybe Core.Integer,
    -- | A list of job IDs that this job depends on.
    dependsOn :: Core.Maybe [JobDependency],
    -- | The platform capabilities required by the job definition. If no value is
    -- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
    -- @FARGATE@.
    platformCapabilities :: Core.Maybe [PlatformCapability],
    -- | The timeout configuration for the job.
    timeout :: Core.Maybe JobTimeout,
    -- | The array properties of the job, if it is an array job.
    arrayProperties :: Core.Maybe ArrayPropertiesDetail,
    -- | The Unix timestamp (in milliseconds) for when the job was created. For
    -- non-array jobs and parent array jobs, this is when the job entered the
    -- @SUBMITTED@ state (at the time SubmitJob was called). For array child
    -- jobs, this is when the child job was spawned by its parent and entered
    -- the @PENDING@ state.
    createdAt :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Core.Maybe Core.Text,
    -- | An object representing the details of a node that\'s associated with a
    -- multi-node parallel job.
    nodeDetails :: Core.Maybe NodeDetails,
    -- | The Unix timestamp (in milliseconds) for when the job was stopped (when
    -- the job transitioned from the @RUNNING@ state to a terminal state, such
    -- as @SUCCEEDED@ or @FAILED@).
    stoppedAt :: Core.Maybe Core.Integer,
    -- | An object representing the node properties of a multi-node parallel job.
    --
    -- This isn\'t applicable to jobs running on Fargate resources.
    nodeProperties :: Core.Maybe NodeProperties,
    -- | The tags applied to the job.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A list of job attempts associated with this job.
    attempts :: Core.Maybe [AttemptDetail],
    -- | The retry strategy to use for this job if an attempt fails.
    retryStrategy :: Core.Maybe RetryStrategy,
    -- | A short, human-readable string to provide additional details about the
    -- current status of the job.
    statusReason :: Core.Maybe Core.Text,
    -- | Additional parameters passed to the job that replace parameter
    -- substitution placeholders or override any corresponding parameter
    -- defaults from the job definition.
    parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Specifies whether to propagate the tags from the job or job definition
    -- to the corresponding Amazon ECS task. If no value is specified, the tags
    -- are not propagated. Tags can only be propagated to the tasks during task
    -- creation. For tags with the same name, job tags are given priority over
    -- job definitions tags. If the total number of combined tags from the job
    -- and job definition is over 50, the job is moved to the @FAILED@ state.
    propagateTags :: Core.Maybe Core.Bool,
    -- | The name of the job.
    jobName :: Core.Text,
    -- | The ID for the job.
    jobId :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the job queue that the job is
    -- associated with.
    jobQueue :: Core.Text,
    -- | The current status for the job.
    --
    -- If your jobs don\'t progress to @STARTING@, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#job_stuck_in_runnable Jobs Stuck in RUNNABLE Status>
    -- in the troubleshooting section of the /AWS Batch User Guide/.
    status :: JobStatus,
    -- | The job definition that\'s used by this job.
    jobDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JobDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'container', 'jobDetail_container' - An object representing the details of the container that\'s associated
-- with the job.
--
-- 'startedAt', 'jobDetail_startedAt' - The Unix timestamp (in milliseconds) for when the job was started (when
-- the job transitioned from the @STARTING@ state to the @RUNNING@ state).
-- This parameter isn\'t provided for child jobs of array jobs or
-- multi-node parallel jobs.
--
-- 'dependsOn', 'jobDetail_dependsOn' - A list of job IDs that this job depends on.
--
-- 'platformCapabilities', 'jobDetail_platformCapabilities' - The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
-- @FARGATE@.
--
-- 'timeout', 'jobDetail_timeout' - The timeout configuration for the job.
--
-- 'arrayProperties', 'jobDetail_arrayProperties' - The array properties of the job, if it is an array job.
--
-- 'createdAt', 'jobDetail_createdAt' - The Unix timestamp (in milliseconds) for when the job was created. For
-- non-array jobs and parent array jobs, this is when the job entered the
-- @SUBMITTED@ state (at the time SubmitJob was called). For array child
-- jobs, this is when the child job was spawned by its parent and entered
-- the @PENDING@ state.
--
-- 'jobArn', 'jobDetail_jobArn' - The Amazon Resource Name (ARN) of the job.
--
-- 'nodeDetails', 'jobDetail_nodeDetails' - An object representing the details of a node that\'s associated with a
-- multi-node parallel job.
--
-- 'stoppedAt', 'jobDetail_stoppedAt' - The Unix timestamp (in milliseconds) for when the job was stopped (when
-- the job transitioned from the @RUNNING@ state to a terminal state, such
-- as @SUCCEEDED@ or @FAILED@).
--
-- 'nodeProperties', 'jobDetail_nodeProperties' - An object representing the node properties of a multi-node parallel job.
--
-- This isn\'t applicable to jobs running on Fargate resources.
--
-- 'tags', 'jobDetail_tags' - The tags applied to the job.
--
-- 'attempts', 'jobDetail_attempts' - A list of job attempts associated with this job.
--
-- 'retryStrategy', 'jobDetail_retryStrategy' - The retry strategy to use for this job if an attempt fails.
--
-- 'statusReason', 'jobDetail_statusReason' - A short, human-readable string to provide additional details about the
-- current status of the job.
--
-- 'parameters', 'jobDetail_parameters' - Additional parameters passed to the job that replace parameter
-- substitution placeholders or override any corresponding parameter
-- defaults from the job definition.
--
-- 'propagateTags', 'jobDetail_propagateTags' - Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
--
-- 'jobName', 'jobDetail_jobName' - The name of the job.
--
-- 'jobId', 'jobDetail_jobId' - The ID for the job.
--
-- 'jobQueue', 'jobDetail_jobQueue' - The Amazon Resource Name (ARN) of the job queue that the job is
-- associated with.
--
-- 'status', 'jobDetail_status' - The current status for the job.
--
-- If your jobs don\'t progress to @STARTING@, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#job_stuck_in_runnable Jobs Stuck in RUNNABLE Status>
-- in the troubleshooting section of the /AWS Batch User Guide/.
--
-- 'jobDefinition', 'jobDetail_jobDefinition' - The job definition that\'s used by this job.
newJobDetail ::
  -- | 'jobName'
  Core.Text ->
  -- | 'jobId'
  Core.Text ->
  -- | 'jobQueue'
  Core.Text ->
  -- | 'status'
  JobStatus ->
  -- | 'jobDefinition'
  Core.Text ->
  JobDetail
newJobDetail
  pJobName_
  pJobId_
  pJobQueue_
  pStatus_
  pJobDefinition_ =
    JobDetail'
      { container = Core.Nothing,
        startedAt = Core.Nothing,
        dependsOn = Core.Nothing,
        platformCapabilities = Core.Nothing,
        timeout = Core.Nothing,
        arrayProperties = Core.Nothing,
        createdAt = Core.Nothing,
        jobArn = Core.Nothing,
        nodeDetails = Core.Nothing,
        stoppedAt = Core.Nothing,
        nodeProperties = Core.Nothing,
        tags = Core.Nothing,
        attempts = Core.Nothing,
        retryStrategy = Core.Nothing,
        statusReason = Core.Nothing,
        parameters = Core.Nothing,
        propagateTags = Core.Nothing,
        jobName = pJobName_,
        jobId = pJobId_,
        jobQueue = pJobQueue_,
        status = pStatus_,
        jobDefinition = pJobDefinition_
      }

-- | An object representing the details of the container that\'s associated
-- with the job.
jobDetail_container :: Lens.Lens' JobDetail (Core.Maybe ContainerDetail)
jobDetail_container = Lens.lens (\JobDetail' {container} -> container) (\s@JobDetail' {} a -> s {container = a} :: JobDetail)

-- | The Unix timestamp (in milliseconds) for when the job was started (when
-- the job transitioned from the @STARTING@ state to the @RUNNING@ state).
-- This parameter isn\'t provided for child jobs of array jobs or
-- multi-node parallel jobs.
jobDetail_startedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jobDetail_startedAt = Lens.lens (\JobDetail' {startedAt} -> startedAt) (\s@JobDetail' {} a -> s {startedAt = a} :: JobDetail)

-- | A list of job IDs that this job depends on.
jobDetail_dependsOn :: Lens.Lens' JobDetail (Core.Maybe [JobDependency])
jobDetail_dependsOn = Lens.lens (\JobDetail' {dependsOn} -> dependsOn) (\s@JobDetail' {} a -> s {dependsOn = a} :: JobDetail) Core.. Lens.mapping Lens._Coerce

-- | The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
-- @FARGATE@.
jobDetail_platformCapabilities :: Lens.Lens' JobDetail (Core.Maybe [PlatformCapability])
jobDetail_platformCapabilities = Lens.lens (\JobDetail' {platformCapabilities} -> platformCapabilities) (\s@JobDetail' {} a -> s {platformCapabilities = a} :: JobDetail) Core.. Lens.mapping Lens._Coerce

-- | The timeout configuration for the job.
jobDetail_timeout :: Lens.Lens' JobDetail (Core.Maybe JobTimeout)
jobDetail_timeout = Lens.lens (\JobDetail' {timeout} -> timeout) (\s@JobDetail' {} a -> s {timeout = a} :: JobDetail)

-- | The array properties of the job, if it is an array job.
jobDetail_arrayProperties :: Lens.Lens' JobDetail (Core.Maybe ArrayPropertiesDetail)
jobDetail_arrayProperties = Lens.lens (\JobDetail' {arrayProperties} -> arrayProperties) (\s@JobDetail' {} a -> s {arrayProperties = a} :: JobDetail)

-- | The Unix timestamp (in milliseconds) for when the job was created. For
-- non-array jobs and parent array jobs, this is when the job entered the
-- @SUBMITTED@ state (at the time SubmitJob was called). For array child
-- jobs, this is when the child job was spawned by its parent and entered
-- the @PENDING@ state.
jobDetail_createdAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jobDetail_createdAt = Lens.lens (\JobDetail' {createdAt} -> createdAt) (\s@JobDetail' {} a -> s {createdAt = a} :: JobDetail)

-- | The Amazon Resource Name (ARN) of the job.
jobDetail_jobArn :: Lens.Lens' JobDetail (Core.Maybe Core.Text)
jobDetail_jobArn = Lens.lens (\JobDetail' {jobArn} -> jobArn) (\s@JobDetail' {} a -> s {jobArn = a} :: JobDetail)

-- | An object representing the details of a node that\'s associated with a
-- multi-node parallel job.
jobDetail_nodeDetails :: Lens.Lens' JobDetail (Core.Maybe NodeDetails)
jobDetail_nodeDetails = Lens.lens (\JobDetail' {nodeDetails} -> nodeDetails) (\s@JobDetail' {} a -> s {nodeDetails = a} :: JobDetail)

-- | The Unix timestamp (in milliseconds) for when the job was stopped (when
-- the job transitioned from the @RUNNING@ state to a terminal state, such
-- as @SUCCEEDED@ or @FAILED@).
jobDetail_stoppedAt :: Lens.Lens' JobDetail (Core.Maybe Core.Integer)
jobDetail_stoppedAt = Lens.lens (\JobDetail' {stoppedAt} -> stoppedAt) (\s@JobDetail' {} a -> s {stoppedAt = a} :: JobDetail)

-- | An object representing the node properties of a multi-node parallel job.
--
-- This isn\'t applicable to jobs running on Fargate resources.
jobDetail_nodeProperties :: Lens.Lens' JobDetail (Core.Maybe NodeProperties)
jobDetail_nodeProperties = Lens.lens (\JobDetail' {nodeProperties} -> nodeProperties) (\s@JobDetail' {} a -> s {nodeProperties = a} :: JobDetail)

-- | The tags applied to the job.
jobDetail_tags :: Lens.Lens' JobDetail (Core.Maybe (Core.HashMap Core.Text Core.Text))
jobDetail_tags = Lens.lens (\JobDetail' {tags} -> tags) (\s@JobDetail' {} a -> s {tags = a} :: JobDetail) Core.. Lens.mapping Lens._Coerce

-- | A list of job attempts associated with this job.
jobDetail_attempts :: Lens.Lens' JobDetail (Core.Maybe [AttemptDetail])
jobDetail_attempts = Lens.lens (\JobDetail' {attempts} -> attempts) (\s@JobDetail' {} a -> s {attempts = a} :: JobDetail) Core.. Lens.mapping Lens._Coerce

-- | The retry strategy to use for this job if an attempt fails.
jobDetail_retryStrategy :: Lens.Lens' JobDetail (Core.Maybe RetryStrategy)
jobDetail_retryStrategy = Lens.lens (\JobDetail' {retryStrategy} -> retryStrategy) (\s@JobDetail' {} a -> s {retryStrategy = a} :: JobDetail)

-- | A short, human-readable string to provide additional details about the
-- current status of the job.
jobDetail_statusReason :: Lens.Lens' JobDetail (Core.Maybe Core.Text)
jobDetail_statusReason = Lens.lens (\JobDetail' {statusReason} -> statusReason) (\s@JobDetail' {} a -> s {statusReason = a} :: JobDetail)

-- | Additional parameters passed to the job that replace parameter
-- substitution placeholders or override any corresponding parameter
-- defaults from the job definition.
jobDetail_parameters :: Lens.Lens' JobDetail (Core.Maybe (Core.HashMap Core.Text Core.Text))
jobDetail_parameters = Lens.lens (\JobDetail' {parameters} -> parameters) (\s@JobDetail' {} a -> s {parameters = a} :: JobDetail) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- are not propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
jobDetail_propagateTags :: Lens.Lens' JobDetail (Core.Maybe Core.Bool)
jobDetail_propagateTags = Lens.lens (\JobDetail' {propagateTags} -> propagateTags) (\s@JobDetail' {} a -> s {propagateTags = a} :: JobDetail)

-- | The name of the job.
jobDetail_jobName :: Lens.Lens' JobDetail Core.Text
jobDetail_jobName = Lens.lens (\JobDetail' {jobName} -> jobName) (\s@JobDetail' {} a -> s {jobName = a} :: JobDetail)

-- | The ID for the job.
jobDetail_jobId :: Lens.Lens' JobDetail Core.Text
jobDetail_jobId = Lens.lens (\JobDetail' {jobId} -> jobId) (\s@JobDetail' {} a -> s {jobId = a} :: JobDetail)

-- | The Amazon Resource Name (ARN) of the job queue that the job is
-- associated with.
jobDetail_jobQueue :: Lens.Lens' JobDetail Core.Text
jobDetail_jobQueue = Lens.lens (\JobDetail' {jobQueue} -> jobQueue) (\s@JobDetail' {} a -> s {jobQueue = a} :: JobDetail)

-- | The current status for the job.
--
-- If your jobs don\'t progress to @STARTING@, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#job_stuck_in_runnable Jobs Stuck in RUNNABLE Status>
-- in the troubleshooting section of the /AWS Batch User Guide/.
jobDetail_status :: Lens.Lens' JobDetail JobStatus
jobDetail_status = Lens.lens (\JobDetail' {status} -> status) (\s@JobDetail' {} a -> s {status = a} :: JobDetail)

-- | The job definition that\'s used by this job.
jobDetail_jobDefinition :: Lens.Lens' JobDetail Core.Text
jobDetail_jobDefinition = Lens.lens (\JobDetail' {jobDefinition} -> jobDefinition) (\s@JobDetail' {} a -> s {jobDefinition = a} :: JobDetail)

instance Core.FromJSON JobDetail where
  parseJSON =
    Core.withObject
      "JobDetail"
      ( \x ->
          JobDetail'
            Core.<$> (x Core..:? "container")
            Core.<*> (x Core..:? "startedAt")
            Core.<*> (x Core..:? "dependsOn" Core..!= Core.mempty)
            Core.<*> ( x Core..:? "platformCapabilities"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "timeout")
            Core.<*> (x Core..:? "arrayProperties")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "jobArn")
            Core.<*> (x Core..:? "nodeDetails")
            Core.<*> (x Core..:? "stoppedAt")
            Core.<*> (x Core..:? "nodeProperties")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "attempts" Core..!= Core.mempty)
            Core.<*> (x Core..:? "retryStrategy")
            Core.<*> (x Core..:? "statusReason")
            Core.<*> (x Core..:? "parameters" Core..!= Core.mempty)
            Core.<*> (x Core..:? "propagateTags")
            Core.<*> (x Core..: "jobName")
            Core.<*> (x Core..: "jobId")
            Core.<*> (x Core..: "jobQueue")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..: "jobDefinition")
      )

instance Core.Hashable JobDetail

instance Core.NFData JobDetail
