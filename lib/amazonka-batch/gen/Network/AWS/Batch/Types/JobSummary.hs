-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobSummary
  ( JobSummary (..),

    -- * Smart constructor
    mkJobSummary,

    -- * Lenses
    jsStoppedAt,
    jsStatus,
    jsJobARN,
    jsCreatedAt,
    jsStartedAt,
    jsContainer,
    jsStatusReason,
    jsArrayProperties,
    jsNodeProperties,
    jsJobId,
    jsJobName,
  )
where

import Network.AWS.Batch.Types.ArrayPropertiesSummary
import Network.AWS.Batch.Types.ContainerSummary
import Network.AWS.Batch.Types.JobStatus
import Network.AWS.Batch.Types.NodePropertiesSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing summary details of a job.
--
-- /See:/ 'mkJobSummary' smart constructor.
data JobSummary = JobSummary'
  { stoppedAt :: Lude.Maybe Lude.Integer,
    status :: Lude.Maybe JobStatus,
    jobARN :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Integer,
    container :: Lude.Maybe ContainerSummary,
    statusReason :: Lude.Maybe Lude.Text,
    arrayProperties :: Lude.Maybe ArrayPropertiesSummary,
    nodeProperties :: Lude.Maybe NodePropertiesSummary,
    jobId :: Lude.Text,
    jobName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobSummary' with the minimum fields required to make a request.
--
-- * 'arrayProperties' - The array properties of the job, if it is an array job.
-- * 'container' - An object representing the details of the container that is associated with the job.
-- * 'createdAt' - The Unix timestamp for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
-- * 'jobARN' - The Amazon Resource Name (ARN) of the job.
-- * 'jobId' - The ID of the job.
-- * 'jobName' - The name of the job.
-- * 'nodeProperties' - The node properties for a single node in a job summary list.
-- * 'startedAt' - The Unix timestamp for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state).
-- * 'status' - The current status for the job.
-- * 'statusReason' - A short, human-readable string to provide additional details about the current status of the job.
-- * 'stoppedAt' - The Unix timestamp for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
mkJobSummary ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'jobName'
  Lude.Text ->
  JobSummary
mkJobSummary pJobId_ pJobName_ =
  JobSummary'
    { stoppedAt = Lude.Nothing,
      status = Lude.Nothing,
      jobARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      startedAt = Lude.Nothing,
      container = Lude.Nothing,
      statusReason = Lude.Nothing,
      arrayProperties = Lude.Nothing,
      nodeProperties = Lude.Nothing,
      jobId = pJobId_,
      jobName = pJobName_
    }

-- | The Unix timestamp for when the job was stopped (when the job transitioned from the @RUNNING@ state to a terminal state, such as @SUCCEEDED@ or @FAILED@ ).
--
-- /Note:/ Consider using 'stoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStoppedAt :: Lens.Lens' JobSummary (Lude.Maybe Lude.Integer)
jsStoppedAt = Lens.lens (stoppedAt :: JobSummary -> Lude.Maybe Lude.Integer) (\s a -> s {stoppedAt = a} :: JobSummary)
{-# DEPRECATED jsStoppedAt "Use generic-lens or generic-optics with 'stoppedAt' instead." #-}

-- | The current status for the job.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStatus :: Lens.Lens' JobSummary (Lude.Maybe JobStatus)
jsStatus = Lens.lens (status :: JobSummary -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: JobSummary)
{-# DEPRECATED jsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Name (ARN) of the job.
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobARN :: Lens.Lens' JobSummary (Lude.Maybe Lude.Text)
jsJobARN = Lens.lens (jobARN :: JobSummary -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: JobSummary)
{-# DEPRECATED jsJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | The Unix timestamp for when the job was created. For non-array jobs and parent array jobs, this is when the job entered the @SUBMITTED@ state (at the time 'SubmitJob' was called). For array child jobs, this is when the child job was spawned by its parent and entered the @PENDING@ state.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsCreatedAt :: Lens.Lens' JobSummary (Lude.Maybe Lude.Integer)
jsCreatedAt = Lens.lens (createdAt :: JobSummary -> Lude.Maybe Lude.Integer) (\s a -> s {createdAt = a} :: JobSummary)
{-# DEPRECATED jsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Unix timestamp for when the job was started (when the job transitioned from the @STARTING@ state to the @RUNNING@ state).
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStartedAt :: Lens.Lens' JobSummary (Lude.Maybe Lude.Integer)
jsStartedAt = Lens.lens (startedAt :: JobSummary -> Lude.Maybe Lude.Integer) (\s a -> s {startedAt = a} :: JobSummary)
{-# DEPRECATED jsStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | An object representing the details of the container that is associated with the job.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsContainer :: Lens.Lens' JobSummary (Lude.Maybe ContainerSummary)
jsContainer = Lens.lens (container :: JobSummary -> Lude.Maybe ContainerSummary) (\s a -> s {container = a} :: JobSummary)
{-# DEPRECATED jsContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | A short, human-readable string to provide additional details about the current status of the job.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStatusReason :: Lens.Lens' JobSummary (Lude.Maybe Lude.Text)
jsStatusReason = Lens.lens (statusReason :: JobSummary -> Lude.Maybe Lude.Text) (\s a -> s {statusReason = a} :: JobSummary)
{-# DEPRECATED jsStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | The array properties of the job, if it is an array job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsArrayProperties :: Lens.Lens' JobSummary (Lude.Maybe ArrayPropertiesSummary)
jsArrayProperties = Lens.lens (arrayProperties :: JobSummary -> Lude.Maybe ArrayPropertiesSummary) (\s a -> s {arrayProperties = a} :: JobSummary)
{-# DEPRECATED jsArrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead." #-}

-- | The node properties for a single node in a job summary list.
--
-- /Note:/ Consider using 'nodeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsNodeProperties :: Lens.Lens' JobSummary (Lude.Maybe NodePropertiesSummary)
jsNodeProperties = Lens.lens (nodeProperties :: JobSummary -> Lude.Maybe NodePropertiesSummary) (\s a -> s {nodeProperties = a} :: JobSummary)
{-# DEPRECATED jsNodeProperties "Use generic-lens or generic-optics with 'nodeProperties' instead." #-}

-- | The ID of the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobId :: Lens.Lens' JobSummary Lude.Text
jsJobId = Lens.lens (jobId :: JobSummary -> Lude.Text) (\s a -> s {jobId = a} :: JobSummary)
{-# DEPRECATED jsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobName :: Lens.Lens' JobSummary Lude.Text
jsJobName = Lens.lens (jobName :: JobSummary -> Lude.Text) (\s a -> s {jobName = a} :: JobSummary)
{-# DEPRECATED jsJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

instance Lude.FromJSON JobSummary where
  parseJSON =
    Lude.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Lude.<$> (x Lude..:? "stoppedAt")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "jobArn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..:? "container")
            Lude.<*> (x Lude..:? "statusReason")
            Lude.<*> (x Lude..:? "arrayProperties")
            Lude.<*> (x Lude..:? "nodeProperties")
            Lude.<*> (x Lude..: "jobId")
            Lude.<*> (x Lude..: "jobName")
      )
