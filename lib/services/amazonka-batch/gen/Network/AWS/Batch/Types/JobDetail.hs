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
import qualified Network.AWS.Prelude as Prelude

-- | An object representing an Batch job.
--
-- /See:/ 'newJobDetail' smart constructor.
data JobDetail = JobDetail'
  { -- | An object representing the details of the container that\'s associated
    -- with the job.
    container :: Prelude.Maybe ContainerDetail,
    -- | A list of job IDs that this job depends on.
    dependsOn :: Prelude.Maybe [JobDependency],
    -- | The Unix timestamp (in milliseconds) for when the job was started (when
    -- the job transitioned from the @STARTING@ state to the @RUNNING@ state).
    -- This parameter isn\'t provided for child jobs of array jobs or
    -- multi-node parallel jobs.
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The platform capabilities required by the job definition. If no value is
    -- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
    -- @FARGATE@.
    platformCapabilities :: Prelude.Maybe [PlatformCapability],
    -- | The timeout configuration for the job.
    timeout :: Prelude.Maybe JobTimeout,
    -- | The array properties of the job, if it is an array job.
    arrayProperties :: Prelude.Maybe ArrayPropertiesDetail,
    -- | The Unix timestamp (in milliseconds) for when the job was created. For
    -- non-array jobs and parent array jobs, this is when the job entered the
    -- @SUBMITTED@ state (at the time SubmitJob was called). For array child
    -- jobs, this is when the child job was spawned by its parent and entered
    -- the @PENDING@ state.
    createdAt :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the job.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | An object representing the details of a node that\'s associated with a
    -- multi-node parallel job.
    nodeDetails :: Prelude.Maybe NodeDetails,
    -- | The Unix timestamp (in milliseconds) for when the job was stopped (when
    -- the job transitioned from the @RUNNING@ state to a terminal state, such
    -- as @SUCCEEDED@ or @FAILED@).
    stoppedAt :: Prelude.Maybe Prelude.Integer,
    -- | An object representing the node properties of a multi-node parallel job.
    --
    -- This isn\'t applicable to jobs that are running on Fargate resources.
    nodeProperties :: Prelude.Maybe NodeProperties,
    -- | The tags applied to the job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of job attempts associated with this job.
    attempts :: Prelude.Maybe [AttemptDetail],
    -- | The retry strategy to use for this job if an attempt fails.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | A short, human-readable string to provide additional details about the
    -- current status of the job.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Additional parameters passed to the job that replace parameter
    -- substitution placeholders or override any corresponding parameter
    -- defaults from the job definition.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether to propagate the tags from the job or job definition
    -- to the corresponding Amazon ECS task. If no value is specified, the tags
    -- aren\'t propagated. Tags can only be propagated to the tasks during task
    -- creation. For tags with the same name, job tags are given priority over
    -- job definitions tags. If the total number of combined tags from the job
    -- and job definition is over 50, the job is moved to the @FAILED@ state.
    propagateTags :: Prelude.Maybe Prelude.Bool,
    -- | The name of the job.
    jobName :: Prelude.Text,
    -- | The ID for the job.
    jobId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the job queue that the job is
    -- associated with.
    jobQueue :: Prelude.Text,
    -- | The current status for the job.
    --
    -- If your jobs don\'t progress to @STARTING@, see
    -- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#job_stuck_in_runnable Jobs Stuck in RUNNABLE Status>
    -- in the troubleshooting section of the /Batch User Guide/.
    status :: JobStatus,
    -- | The job definition that\'s used by this job.
    jobDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dependsOn', 'jobDetail_dependsOn' - A list of job IDs that this job depends on.
--
-- 'startedAt', 'jobDetail_startedAt' - The Unix timestamp (in milliseconds) for when the job was started (when
-- the job transitioned from the @STARTING@ state to the @RUNNING@ state).
-- This parameter isn\'t provided for child jobs of array jobs or
-- multi-node parallel jobs.
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
-- This isn\'t applicable to jobs that are running on Fargate resources.
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
-- aren\'t propagated. Tags can only be propagated to the tasks during task
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
-- in the troubleshooting section of the /Batch User Guide/.
--
-- 'jobDefinition', 'jobDetail_jobDefinition' - The job definition that\'s used by this job.
newJobDetail ::
  -- | 'jobName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobQueue'
  Prelude.Text ->
  -- | 'status'
  JobStatus ->
  -- | 'jobDefinition'
  Prelude.Text ->
  JobDetail
newJobDetail
  pJobName_
  pJobId_
  pJobQueue_
  pStatus_
  pJobDefinition_ =
    JobDetail'
      { container = Prelude.Nothing,
        dependsOn = Prelude.Nothing,
        startedAt = Prelude.Nothing,
        platformCapabilities = Prelude.Nothing,
        timeout = Prelude.Nothing,
        arrayProperties = Prelude.Nothing,
        createdAt = Prelude.Nothing,
        jobArn = Prelude.Nothing,
        nodeDetails = Prelude.Nothing,
        stoppedAt = Prelude.Nothing,
        nodeProperties = Prelude.Nothing,
        tags = Prelude.Nothing,
        attempts = Prelude.Nothing,
        retryStrategy = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        parameters = Prelude.Nothing,
        propagateTags = Prelude.Nothing,
        jobName = pJobName_,
        jobId = pJobId_,
        jobQueue = pJobQueue_,
        status = pStatus_,
        jobDefinition = pJobDefinition_
      }

-- | An object representing the details of the container that\'s associated
-- with the job.
jobDetail_container :: Lens.Lens' JobDetail (Prelude.Maybe ContainerDetail)
jobDetail_container = Lens.lens (\JobDetail' {container} -> container) (\s@JobDetail' {} a -> s {container = a} :: JobDetail)

-- | A list of job IDs that this job depends on.
jobDetail_dependsOn :: Lens.Lens' JobDetail (Prelude.Maybe [JobDependency])
jobDetail_dependsOn = Lens.lens (\JobDetail' {dependsOn} -> dependsOn) (\s@JobDetail' {} a -> s {dependsOn = a} :: JobDetail) Prelude.. Lens.mapping Lens._Coerce

-- | The Unix timestamp (in milliseconds) for when the job was started (when
-- the job transitioned from the @STARTING@ state to the @RUNNING@ state).
-- This parameter isn\'t provided for child jobs of array jobs or
-- multi-node parallel jobs.
jobDetail_startedAt :: Lens.Lens' JobDetail (Prelude.Maybe Prelude.Integer)
jobDetail_startedAt = Lens.lens (\JobDetail' {startedAt} -> startedAt) (\s@JobDetail' {} a -> s {startedAt = a} :: JobDetail)

-- | The platform capabilities required by the job definition. If no value is
-- specified, it defaults to @EC2@. Jobs run on Fargate resources specify
-- @FARGATE@.
jobDetail_platformCapabilities :: Lens.Lens' JobDetail (Prelude.Maybe [PlatformCapability])
jobDetail_platformCapabilities = Lens.lens (\JobDetail' {platformCapabilities} -> platformCapabilities) (\s@JobDetail' {} a -> s {platformCapabilities = a} :: JobDetail) Prelude.. Lens.mapping Lens._Coerce

-- | The timeout configuration for the job.
jobDetail_timeout :: Lens.Lens' JobDetail (Prelude.Maybe JobTimeout)
jobDetail_timeout = Lens.lens (\JobDetail' {timeout} -> timeout) (\s@JobDetail' {} a -> s {timeout = a} :: JobDetail)

-- | The array properties of the job, if it is an array job.
jobDetail_arrayProperties :: Lens.Lens' JobDetail (Prelude.Maybe ArrayPropertiesDetail)
jobDetail_arrayProperties = Lens.lens (\JobDetail' {arrayProperties} -> arrayProperties) (\s@JobDetail' {} a -> s {arrayProperties = a} :: JobDetail)

-- | The Unix timestamp (in milliseconds) for when the job was created. For
-- non-array jobs and parent array jobs, this is when the job entered the
-- @SUBMITTED@ state (at the time SubmitJob was called). For array child
-- jobs, this is when the child job was spawned by its parent and entered
-- the @PENDING@ state.
jobDetail_createdAt :: Lens.Lens' JobDetail (Prelude.Maybe Prelude.Integer)
jobDetail_createdAt = Lens.lens (\JobDetail' {createdAt} -> createdAt) (\s@JobDetail' {} a -> s {createdAt = a} :: JobDetail)

-- | The Amazon Resource Name (ARN) of the job.
jobDetail_jobArn :: Lens.Lens' JobDetail (Prelude.Maybe Prelude.Text)
jobDetail_jobArn = Lens.lens (\JobDetail' {jobArn} -> jobArn) (\s@JobDetail' {} a -> s {jobArn = a} :: JobDetail)

-- | An object representing the details of a node that\'s associated with a
-- multi-node parallel job.
jobDetail_nodeDetails :: Lens.Lens' JobDetail (Prelude.Maybe NodeDetails)
jobDetail_nodeDetails = Lens.lens (\JobDetail' {nodeDetails} -> nodeDetails) (\s@JobDetail' {} a -> s {nodeDetails = a} :: JobDetail)

-- | The Unix timestamp (in milliseconds) for when the job was stopped (when
-- the job transitioned from the @RUNNING@ state to a terminal state, such
-- as @SUCCEEDED@ or @FAILED@).
jobDetail_stoppedAt :: Lens.Lens' JobDetail (Prelude.Maybe Prelude.Integer)
jobDetail_stoppedAt = Lens.lens (\JobDetail' {stoppedAt} -> stoppedAt) (\s@JobDetail' {} a -> s {stoppedAt = a} :: JobDetail)

-- | An object representing the node properties of a multi-node parallel job.
--
-- This isn\'t applicable to jobs that are running on Fargate resources.
jobDetail_nodeProperties :: Lens.Lens' JobDetail (Prelude.Maybe NodeProperties)
jobDetail_nodeProperties = Lens.lens (\JobDetail' {nodeProperties} -> nodeProperties) (\s@JobDetail' {} a -> s {nodeProperties = a} :: JobDetail)

-- | The tags applied to the job.
jobDetail_tags :: Lens.Lens' JobDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobDetail_tags = Lens.lens (\JobDetail' {tags} -> tags) (\s@JobDetail' {} a -> s {tags = a} :: JobDetail) Prelude.. Lens.mapping Lens._Coerce

-- | A list of job attempts associated with this job.
jobDetail_attempts :: Lens.Lens' JobDetail (Prelude.Maybe [AttemptDetail])
jobDetail_attempts = Lens.lens (\JobDetail' {attempts} -> attempts) (\s@JobDetail' {} a -> s {attempts = a} :: JobDetail) Prelude.. Lens.mapping Lens._Coerce

-- | The retry strategy to use for this job if an attempt fails.
jobDetail_retryStrategy :: Lens.Lens' JobDetail (Prelude.Maybe RetryStrategy)
jobDetail_retryStrategy = Lens.lens (\JobDetail' {retryStrategy} -> retryStrategy) (\s@JobDetail' {} a -> s {retryStrategy = a} :: JobDetail)

-- | A short, human-readable string to provide additional details about the
-- current status of the job.
jobDetail_statusReason :: Lens.Lens' JobDetail (Prelude.Maybe Prelude.Text)
jobDetail_statusReason = Lens.lens (\JobDetail' {statusReason} -> statusReason) (\s@JobDetail' {} a -> s {statusReason = a} :: JobDetail)

-- | Additional parameters passed to the job that replace parameter
-- substitution placeholders or override any corresponding parameter
-- defaults from the job definition.
jobDetail_parameters :: Lens.Lens' JobDetail (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobDetail_parameters = Lens.lens (\JobDetail' {parameters} -> parameters) (\s@JobDetail' {} a -> s {parameters = a} :: JobDetail) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies whether to propagate the tags from the job or job definition
-- to the corresponding Amazon ECS task. If no value is specified, the tags
-- aren\'t propagated. Tags can only be propagated to the tasks during task
-- creation. For tags with the same name, job tags are given priority over
-- job definitions tags. If the total number of combined tags from the job
-- and job definition is over 50, the job is moved to the @FAILED@ state.
jobDetail_propagateTags :: Lens.Lens' JobDetail (Prelude.Maybe Prelude.Bool)
jobDetail_propagateTags = Lens.lens (\JobDetail' {propagateTags} -> propagateTags) (\s@JobDetail' {} a -> s {propagateTags = a} :: JobDetail)

-- | The name of the job.
jobDetail_jobName :: Lens.Lens' JobDetail Prelude.Text
jobDetail_jobName = Lens.lens (\JobDetail' {jobName} -> jobName) (\s@JobDetail' {} a -> s {jobName = a} :: JobDetail)

-- | The ID for the job.
jobDetail_jobId :: Lens.Lens' JobDetail Prelude.Text
jobDetail_jobId = Lens.lens (\JobDetail' {jobId} -> jobId) (\s@JobDetail' {} a -> s {jobId = a} :: JobDetail)

-- | The Amazon Resource Name (ARN) of the job queue that the job is
-- associated with.
jobDetail_jobQueue :: Lens.Lens' JobDetail Prelude.Text
jobDetail_jobQueue = Lens.lens (\JobDetail' {jobQueue} -> jobQueue) (\s@JobDetail' {} a -> s {jobQueue = a} :: JobDetail)

-- | The current status for the job.
--
-- If your jobs don\'t progress to @STARTING@, see
-- <https://docs.aws.amazon.com/batch/latest/userguide/troubleshooting.html#job_stuck_in_runnable Jobs Stuck in RUNNABLE Status>
-- in the troubleshooting section of the /Batch User Guide/.
jobDetail_status :: Lens.Lens' JobDetail JobStatus
jobDetail_status = Lens.lens (\JobDetail' {status} -> status) (\s@JobDetail' {} a -> s {status = a} :: JobDetail)

-- | The job definition that\'s used by this job.
jobDetail_jobDefinition :: Lens.Lens' JobDetail Prelude.Text
jobDetail_jobDefinition = Lens.lens (\JobDetail' {jobDefinition} -> jobDefinition) (\s@JobDetail' {} a -> s {jobDefinition = a} :: JobDetail)

instance Core.FromJSON JobDetail where
  parseJSON =
    Core.withObject
      "JobDetail"
      ( \x ->
          JobDetail'
            Prelude.<$> (x Core..:? "container")
            Prelude.<*> (x Core..:? "dependsOn" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "startedAt")
            Prelude.<*> ( x Core..:? "platformCapabilities"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "timeout")
            Prelude.<*> (x Core..:? "arrayProperties")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "jobArn")
            Prelude.<*> (x Core..:? "nodeDetails")
            Prelude.<*> (x Core..:? "stoppedAt")
            Prelude.<*> (x Core..:? "nodeProperties")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "attempts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "retryStrategy")
            Prelude.<*> (x Core..:? "statusReason")
            Prelude.<*> (x Core..:? "parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "propagateTags")
            Prelude.<*> (x Core..: "jobName")
            Prelude.<*> (x Core..: "jobId")
            Prelude.<*> (x Core..: "jobQueue")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "jobDefinition")
      )

instance Prelude.Hashable JobDetail

instance Prelude.NFData JobDetail
