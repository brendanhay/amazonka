{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDetail
  ( JobDetail (..),

    -- * Smart constructor
    mkJobDetail,

    -- * Lenses
    jdStoppedAt,
    jdStatus,
    jdJobId,
    jdJobARN,
    jdCreatedAt,
    jdJobName,
    jdRetryStrategy,
    jdAttempts,
    jdStartedAt,
    jdDependsOn,
    jdContainer,
    jdJobDefinition,
    jdNodeDetails,
    jdParameters,
    jdStatusReason,
    jdArrayProperties,
    jdTimeout,
    jdJobQueue,
    jdNodeProperties,
    jdTags,
  )
where

import Network.AWS.Batch.Types.ArrayPropertiesDetail
import Network.AWS.Batch.Types.AttemptDetail
import Network.AWS.Batch.Types.ContainerDetail
import Network.AWS.Batch.Types.JobDependency
import Network.AWS.Batch.Types.JobStatus
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.RetryStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an AWS Batch job.
--
-- /See:/ 'mkJobDetail' smart constructor.
data JobDetail = JobDetail'
  { -- | The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
    stoppedAt :: Lude.Maybe Lude.Integer,
    -- | The current status for the job.
    status :: JobStatus,
    -- | The ID for the job.
    jobId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the job.
    jobARN :: Lude.Maybe Lude.Text,
    -- | The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
    createdAt :: Lude.Maybe Lude.Integer,
    -- | The name of the job.
    jobName :: Lude.Text,
    -- | The retry strategy to use for this job if an attempt fails.
    retryStrategy :: Lude.Maybe RetryStrategy,
    -- | A list of job attempts associated with this job.
    attempts :: Lude.Maybe [AttemptDetail],
    -- | The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
    startedAt :: Lude.Maybe Lude.Integer,
    -- | A list of job IDs on which this job depends.
    dependsOn :: Lude.Maybe [JobDependency],
    -- | An object representing the details of the container that is associated with the job.
    container :: Lude.Maybe ContainerDetail,
    -- | The job definition that is used by this job.
    jobDefinition :: Lude.Text,
    -- | An object representing the details of a node that is associated with a multi-node parallel job.
    nodeDetails :: Lude.Maybe NodeDetails,
    -- | Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A short, human-readable string to provide additional details about the current status of the job.
    statusReason :: Lude.Maybe Lude.Text,
    -- | The array properties of the job, if it is an array job.
    arrayProperties :: Lude.Maybe ArrayPropertiesDetail,
    -- | The timeout configuration for the job.
    timeout :: Lude.Maybe JobTimeout,
    -- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
    jobQueue :: Lude.Text,
    -- | An object representing the node properties of a multi-node parallel job.
    nodeProperties :: Lude.Maybe NodeProperties,
    -- | The tags applied to the job.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobDetail' with the minimum fields required to make a request.
--
-- * 'stoppedAt' - The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
-- * 'status' - The current status for the job.
-- * 'jobId' - The ID for the job.
-- * 'jobARN' - The Amazon Resource Name (ARN) of the job.
-- * 'createdAt' - The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
-- * 'jobName' - The name of the job.
-- * 'retryStrategy' - The retry strategy to use for this job if an attempt fails.
-- * 'attempts' - A list of job attempts associated with this job.
-- * 'startedAt' - The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
-- * 'dependsOn' - A list of job IDs on which this job depends.
-- * 'container' - An object representing the details of the container that is associated with the job.
-- * 'jobDefinition' - The job definition that is used by this job.
-- * 'nodeDetails' - An object representing the details of a node that is associated with a multi-node parallel job.
-- * 'parameters' - Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
-- * 'statusReason' - A short, human-readable string to provide additional details about the current status of the job.
-- * 'arrayProperties' - The array properties of the job, if it is an array job.
-- * 'timeout' - The timeout configuration for the job.
-- * 'jobQueue' - The Amazon Resource Name (ARN) of the job queue with which the job is associated.
-- * 'nodeProperties' - An object representing the node properties of a multi-node parallel job.
-- * 'tags' - The tags applied to the job.
mkJobDetail ::
  -- | 'status'
  JobStatus ->
  -- | 'jobId'
  Lude.Text ->
  -- | 'jobName'
  Lude.Text ->
  -- | 'jobDefinition'
  Lude.Text ->
  -- | 'jobQueue'
  Lude.Text ->
  JobDetail
mkJobDetail pStatus_ pJobId_ pJobName_ pJobDefinition_ pJobQueue_ =
  JobDetail'
    { stoppedAt = Lude.Nothing,
      status = pStatus_,
      jobId = pJobId_,
      jobARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      jobName = pJobName_,
      retryStrategy = Lude.Nothing,
      attempts = Lude.Nothing,
      startedAt = Lude.Nothing,
      dependsOn = Lude.Nothing,
      container = Lude.Nothing,
      jobDefinition = pJobDefinition_,
      nodeDetails = Lude.Nothing,
      parameters = Lude.Nothing,
      statusReason = Lude.Nothing,
      arrayProperties = Lude.Nothing,
      timeout = Lude.Nothing,
      jobQueue = pJobQueue_,
      nodeProperties = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStoppedAt :: Lens.Lens' JobDetail (Lude.Maybe Lude.Integer)
jdStoppedAt = Lens.lens (stoppedAt :: JobDetail -> Lude.Maybe Lude.Integer) (\s a -> s {stoppedAt = a} :: JobDetail)
{-# DEPRECATED jdStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

-- | The current status for the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStatus :: Lens.Lens' JobDetail JobStatus
jdStatus = Lens.lens (status :: JobDetail -> JobStatus) (\s a -> s {status = a} :: JobDetail)
{-# DEPRECATED jdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobId :: Lens.Lens' JobDetail Lude.Text
jdJobId = Lens.lens (jobId :: JobDetail -> Lude.Text) (\s a -> s {jobId = a} :: JobDetail)
{-# DEPRECATED jdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The Amazon Resource Name (ARN) of the job.
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobARN :: Lens.Lens' JobDetail (Lude.Maybe Lude.Text)
jdJobARN = Lens.lens (jobARN :: JobDetail -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: JobDetail)
{-# DEPRECATED jdJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdCreatedAt :: Lens.Lens' JobDetail (Lude.Maybe Lude.Integer)
jdCreatedAt = Lens.lens (createdAt :: JobDetail -> Lude.Maybe Lude.Integer) (\s a -> s {createdAt = a} :: JobDetail)
{-# DEPRECATED jdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobName :: Lens.Lens' JobDetail Lude.Text
jdJobName = Lens.lens (jobName :: JobDetail -> Lude.Text) (\s a -> s {jobName = a} :: JobDetail)
{-# DEPRECATED jdJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The retry strategy to use for this job if an attempt fails.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdRetryStrategy :: Lens.Lens' JobDetail (Lude.Maybe RetryStrategy)
jdRetryStrategy = Lens.lens (retryStrategy :: JobDetail -> Lude.Maybe RetryStrategy) (\s a -> s {retryStrategy = a} :: JobDetail)
{-# DEPRECATED jdRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | A list of job attempts associated with this job.
--
-- /Note:/ Consider using 'attempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdAttempts :: Lens.Lens' JobDetail (Lude.Maybe [AttemptDetail])
jdAttempts = Lens.lens (attempts :: JobDetail -> Lude.Maybe [AttemptDetail]) (\s a -> s {attempts = a} :: JobDetail)
{-# DEPRECATED jdAttempts "Use generic-lens or generic-optics with 'attempts' instead." #-}

-- | The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStartedAt :: Lens.Lens' JobDetail (Lude.Maybe Lude.Integer)
jdStartedAt = Lens.lens (startedAt :: JobDetail -> Lude.Maybe Lude.Integer) (\s a -> s {startedAt = a} :: JobDetail)
{-# DEPRECATED jdStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | A list of job IDs on which this job depends.
--
-- /Note:/ Consider using 'dependsOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdDependsOn :: Lens.Lens' JobDetail (Lude.Maybe [JobDependency])
jdDependsOn = Lens.lens (dependsOn :: JobDetail -> Lude.Maybe [JobDependency]) (\s a -> s {dependsOn = a} :: JobDetail)
{-# DEPRECATED jdDependsOn "Use generic-lens or generic-optics with 'dependsOn' instead." #-}

-- | An object representing the details of the container that is associated with the job.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdContainer :: Lens.Lens' JobDetail (Lude.Maybe ContainerDetail)
jdContainer = Lens.lens (container :: JobDetail -> Lude.Maybe ContainerDetail) (\s a -> s {container = a} :: JobDetail)
{-# DEPRECATED jdContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | The job definition that is used by this job.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobDefinition :: Lens.Lens' JobDetail Lude.Text
jdJobDefinition = Lens.lens (jobDefinition :: JobDetail -> Lude.Text) (\s a -> s {jobDefinition = a} :: JobDetail)
{-# DEPRECATED jdJobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead." #-}

-- | An object representing the details of a node that is associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdNodeDetails :: Lens.Lens' JobDetail (Lude.Maybe NodeDetails)
jdNodeDetails = Lens.lens (nodeDetails :: JobDetail -> Lude.Maybe NodeDetails) (\s a -> s {nodeDetails = a} :: JobDetail)
{-# DEPRECATED jdNodeDetails "Use generic-lens or generic-optics with 'nodeDetails' instead." #-}

-- | Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdParameters :: Lens.Lens' JobDetail (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jdParameters = Lens.lens (parameters :: JobDetail -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: JobDetail)
{-# DEPRECATED jdParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStatusReason :: Lens.Lens' JobDetail (Lude.Maybe Lude.Text)
jdStatusReason = Lens.lens (statusReason :: JobDetail -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: JobDetail)
{-# DEPRECATED jdStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The array properties of the job, if it is an array job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdArrayProperties :: Lens.Lens' JobDetail (Lude.Maybe ArrayPropertiesDetail)
jdArrayProperties = Lens.lens (arrayProperties :: JobDetail -> Lude.Maybe ArrayPropertiesDetail) (\s a -> s {arrayProperties = a} :: JobDetail)
{-# DEPRECATED jdArrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead." #-}

-- | The timeout configuration for the job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTimeout :: Lens.Lens' JobDetail (Lude.Maybe JobTimeout)
jdTimeout = Lens.lens (timeout :: JobDetail -> Lude.Maybe JobTimeout) (\s a -> s {timeout = a} :: JobDetail)
{-# DEPRECATED jdTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

-- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobQueue :: Lens.Lens' JobDetail Lude.Text
jdJobQueue = Lens.lens (jobQueue :: JobDetail -> Lude.Text) (\s a -> s {jobQueue = a} :: JobDetail)
{-# DEPRECATED jdJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

-- | An object representing the node properties of a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdNodeProperties :: Lens.Lens' JobDetail (Lude.Maybe NodeProperties)
jdNodeProperties = Lens.lens (nodeProperties :: JobDetail -> Lude.Maybe NodeProperties) (\s a -> s {nodeProperties = a} :: JobDetail)
{-# DEPRECATED jdNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | The tags applied to the job.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdTags :: Lens.Lens' JobDetail (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jdTags = Lens.lens (tags :: JobDetail -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: JobDetail)
{-# DEPRECATED jdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON JobDetail where
  parseJSON =
    Lude.withObject
      "JobDetail"
      ( \x ->
          JobDetail'
            Lude.<$> (x Lude..:? "stoppedAt")
            Lude.<*> (x Lude..: "status")
            Lude.<*> (x Lude..: "jobId")
            Lude.<*> (x Lude..:? "jobArn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..: "jobName")
            Lude.<*> (x Lude..:? "retryStrategy")
            Lude.<*> (x Lude..:? "attempts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..:? "dependsOn" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "container")
            Lude.<*> (x Lude..: "jobDefinition")
            Lude.<*> (x Lude..:? "nodeDetails")
            Lude.<*> (x Lude..:? "parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "statusReason")
            Lude.<*> (x Lude..:? "arrayProperties")
            Lude.<*> (x Lude..:? "timeout")
            Lude.<*> (x Lude..: "jobQueue")
            Lude.<*> (x Lude..:? "nodeProperties")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
