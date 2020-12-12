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
    jdJobARN,
    jdCreatedAt,
    jdRetryStrategy,
    jdAttempts,
    jdStartedAt,
    jdDependsOn,
    jdContainer,
    jdNodeDetails,
    jdParameters,
    jdStatusReason,
    jdArrayProperties,
    jdTimeout,
    jdNodeProperties,
    jdTags,
    jdJobName,
    jdJobId,
    jdJobQueue,
    jdStatus,
    jdJobDefinition,
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
  { stoppedAt :: Lude.Maybe Lude.Integer,
    jobARN :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Integer,
    retryStrategy :: Lude.Maybe RetryStrategy,
    attempts :: Lude.Maybe [AttemptDetail],
    startedAt :: Lude.Maybe Lude.Integer,
    dependsOn :: Lude.Maybe [JobDependency],
    container :: Lude.Maybe ContainerDetail,
    nodeDetails :: Lude.Maybe NodeDetails,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    statusReason :: Lude.Maybe Lude.Text,
    arrayProperties :: Lude.Maybe ArrayPropertiesDetail,
    timeout :: Lude.Maybe JobTimeout,
    nodeProperties :: Lude.Maybe NodeProperties,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    jobName :: Lude.Text,
    jobId :: Lude.Text,
    jobQueue :: Lude.Text,
    status :: JobStatus,
    jobDefinition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobDetail' with the minimum fields required to make a request.
--
-- * 'arrayProperties' - The array properties of the job, if it is an array job.
-- * 'attempts' - A list of job attempts associated with this job.
-- * 'container' - An object representing the details of the container that is associated with the job.
-- * 'createdAt' - The Unix timestamp (in milliseconds) for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
-- * 'dependsOn' - A list of job IDs on which this job depends.
-- * 'jobARN' - The Amazon Resource Name (ARN) of the job.
-- * 'jobDefinition' - The job definition that is used by this job.
-- * 'jobId' - The ID for the job.
-- * 'jobName' - The name of the job.
-- * 'jobQueue' - The Amazon Resource Name (ARN) of the job queue with which the job is associated.
-- * 'nodeDetails' - An object representing the details of a node that is associated with a multi-node parallel job.
-- * 'nodeProperties' - An object representing the node properties of a multi-node parallel job.
-- * 'parameters' - Additional parameters passed to the job that replace parameter substitution placeholders or override any corresponding parameter defaults from the job definition.
-- * 'retryStrategy' - The retry strategy to use for this job if an attempt fails.
-- * 'startedAt' - The Unix timestamp (in milliseconds) for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state). This parameter is not provided for child jobs of array jobs or multi-node parallel jobs.
-- * 'status' - The current status for the job.
-- * 'statusReason' - A short, human-readable string to provide additional details about the current status of the job.
-- * 'stoppedAt' - The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
-- * 'tags' - The tags applied to the job.
-- * 'timeout' - The timeout configuration for the job.
mkJobDetail ::
  -- | 'jobName'
  Lude.Text ->
  -- | 'jobId'
  Lude.Text ->
  -- | 'jobQueue'
  Lude.Text ->
  -- | 'status'
  JobStatus ->
  -- | 'jobDefinition'
  Lude.Text ->
  JobDetail
mkJobDetail pJobName_ pJobId_ pJobQueue_ pStatus_ pJobDefinition_ =
  JobDetail'
    { stoppedAt = Lude.Nothing,
      jobARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      retryStrategy = Lude.Nothing,
      attempts = Lude.Nothing,
      startedAt = Lude.Nothing,
      dependsOn = Lude.Nothing,
      container = Lude.Nothing,
      nodeDetails = Lude.Nothing,
      parameters = Lude.Nothing,
      statusReason = Lude.Nothing,
      arrayProperties = Lude.Nothing,
      timeout = Lude.Nothing,
      nodeProperties = Lude.Nothing,
      tags = Lude.Nothing,
      jobName = pJobName_,
      jobId = pJobId_,
      jobQueue = pJobQueue_,
      status = pStatus_,
      jobDefinition = pJobDefinition_
    }

-- | The Unix timestamp (in milliseconds) for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStoppedAt :: Lens.Lens' JobDetail (Lude.Maybe Lude.Integer)
jdStoppedAt = Lens.lens (stoppedAt :: JobDetail -> Lude.Maybe Lude.Integer) (\s a -> s {stoppedAt = a} :: JobDetail)
{-# DEPRECATED jdStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

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

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobName :: Lens.Lens' JobDetail Lude.Text
jdJobName = Lens.lens (jobName :: JobDetail -> Lude.Text) (\s a -> s {jobName = a} :: JobDetail)
{-# DEPRECATED jdJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The ID for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobId :: Lens.Lens' JobDetail Lude.Text
jdJobId = Lens.lens (jobId :: JobDetail -> Lude.Text) (\s a -> s {jobId = a} :: JobDetail)
{-# DEPRECATED jdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The Amazon Resource Name (ARN) of the job queue with which the job is associated.
--
-- /Note:/ Consider using 'jobQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobQueue :: Lens.Lens' JobDetail Lude.Text
jdJobQueue = Lens.lens (jobQueue :: JobDetail -> Lude.Text) (\s a -> s {jobQueue = a} :: JobDetail)
{-# DEPRECATED jdJobQueue "Use generic-lens or generic-optics with 'jobQueue' instead." #-}

-- | The current status for the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdStatus :: Lens.Lens' JobDetail JobStatus
jdStatus = Lens.lens (status :: JobDetail -> JobStatus) (\s a -> s {status = a} :: JobDetail)
{-# DEPRECATED jdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The job definition that is used by this job.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdJobDefinition :: Lens.Lens' JobDetail Lude.Text
jdJobDefinition = Lens.lens (jobDefinition :: JobDetail -> Lude.Text) (\s a -> s {jobDefinition = a} :: JobDetail)
{-# DEPRECATED jdJobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead." #-}

instance Lude.FromJSON JobDetail where
  parseJSON =
    Lude.withObject
      "JobDetail"
      ( \x ->
          JobDetail'
            Lude.<$> (x Lude..:? "stoppedAt")
            Lude.<*> (x Lude..:? "jobArn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "retryStrategy")
            Lude.<*> (x Lude..:? "attempts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..:? "dependsOn" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "container")
            Lude.<*> (x Lude..:? "nodeDetails")
            Lude.<*> (x Lude..:? "parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "statusReason")
            Lude.<*> (x Lude..:? "arrayProperties")
            Lude.<*> (x Lude..:? "timeout")
            Lude.<*> (x Lude..:? "nodeProperties")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "jobName")
            Lude.<*> (x Lude..: "jobId")
            Lude.<*> (x Lude..: "jobQueue")
            Lude.<*> (x Lude..: "status")
            Lude.<*> (x Lude..: "jobDefinition")
      )
