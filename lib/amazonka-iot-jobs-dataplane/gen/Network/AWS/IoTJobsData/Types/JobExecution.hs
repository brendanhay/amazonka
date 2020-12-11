-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecution
  ( JobExecution (..),

    -- * Smart constructor
    mkJobExecution,

    -- * Lenses
    jeStatus,
    jeJobId,
    jeLastUpdatedAt,
    jeApproximateSecondsBeforeTimedOut,
    jeQueuedAt,
    jeJobDocument,
    jeStatusDetails,
    jeExecutionNumber,
    jeVersionNumber,
    jeStartedAt,
    jeThingName,
  )
where

import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains data about a job execution.
--
-- /See:/ 'mkJobExecution' smart constructor.
data JobExecution = JobExecution'
  { status ::
      Lude.Maybe JobExecutionStatus,
    jobId :: Lude.Maybe Lude.Text,
    lastUpdatedAt :: Lude.Maybe Lude.Integer,
    approximateSecondsBeforeTimedOut :: Lude.Maybe Lude.Integer,
    queuedAt :: Lude.Maybe Lude.Integer,
    jobDocument :: Lude.Maybe Lude.Text,
    statusDetails :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    executionNumber :: Lude.Maybe Lude.Integer,
    versionNumber :: Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Integer,
    thingName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecution' with the minimum fields required to make a request.
--
-- * 'approximateSecondsBeforeTimedOut' - The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ .
-- * 'executionNumber' - A number that identifies a particular job execution on a particular device. It can be used later in commands that return or update job execution information.
-- * 'jobDocument' - The content of the job document.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'lastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was last updated.
-- * 'queuedAt' - The time, in milliseconds since the epoch, when the job execution was enqueued.
-- * 'startedAt' - The time, in milliseconds since the epoch, when the job execution was started.
-- * 'status' - The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
-- * 'statusDetails' - A collection of name/value pairs that describe the status of the job execution.
-- * 'thingName' - The name of the thing that is executing the job.
-- * 'versionNumber' - The version of the job execution. Job execution versions are incremented each time they are updated by a device.
mkJobExecution ::
  JobExecution
mkJobExecution =
  JobExecution'
    { status = Lude.Nothing,
      jobId = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      approximateSecondsBeforeTimedOut = Lude.Nothing,
      queuedAt = Lude.Nothing,
      jobDocument = Lude.Nothing,
      statusDetails = Lude.Nothing,
      executionNumber = Lude.Nothing,
      versionNumber = Lude.Nothing,
      startedAt = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatus :: Lens.Lens' JobExecution (Lude.Maybe JobExecutionStatus)
jeStatus = Lens.lens (status :: JobExecution -> Lude.Maybe JobExecutionStatus) (\s a -> s {status = a} :: JobExecution)
{-# DEPRECATED jeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeJobId :: Lens.Lens' JobExecution (Lude.Maybe Lude.Text)
jeJobId = Lens.lens (jobId :: JobExecution -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobExecution)
{-# DEPRECATED jeJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeLastUpdatedAt :: Lens.Lens' JobExecution (Lude.Maybe Lude.Integer)
jeLastUpdatedAt = Lens.lens (lastUpdatedAt :: JobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {lastUpdatedAt = a} :: JobExecution)
{-# DEPRECATED jeLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'approximateSecondsBeforeTimedOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeApproximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Lude.Maybe Lude.Integer)
jeApproximateSecondsBeforeTimedOut = Lens.lens (approximateSecondsBeforeTimedOut :: JobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {approximateSecondsBeforeTimedOut = a} :: JobExecution)
{-# DEPRECATED jeApproximateSecondsBeforeTimedOut "Use generic-lens or generic-optics with 'approximateSecondsBeforeTimedOut' instead." #-}

-- | The time, in milliseconds since the epoch, when the job execution was enqueued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeQueuedAt :: Lens.Lens' JobExecution (Lude.Maybe Lude.Integer)
jeQueuedAt = Lens.lens (queuedAt :: JobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {queuedAt = a} :: JobExecution)
{-# DEPRECATED jeQueuedAt "Use generic-lens or generic-optics with 'queuedAt' instead." #-}

-- | The content of the job document.
--
-- /Note:/ Consider using 'jobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeJobDocument :: Lens.Lens' JobExecution (Lude.Maybe Lude.Text)
jeJobDocument = Lens.lens (jobDocument :: JobExecution -> Lude.Maybe Lude.Text) (\s a -> s {jobDocument = a} :: JobExecution)
{-# DEPRECATED jeJobDocument "Use generic-lens or generic-optics with 'jobDocument' instead." #-}

-- | A collection of name/value pairs that describe the status of the job execution.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatusDetails :: Lens.Lens' JobExecution (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jeStatusDetails = Lens.lens (statusDetails :: JobExecution -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {statusDetails = a} :: JobExecution)
{-# DEPRECATED jeStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | A number that identifies a particular job execution on a particular device. It can be used later in commands that return or update job execution information.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeExecutionNumber :: Lens.Lens' JobExecution (Lude.Maybe Lude.Integer)
jeExecutionNumber = Lens.lens (executionNumber :: JobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {executionNumber = a} :: JobExecution)
{-# DEPRECATED jeExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeVersionNumber :: Lens.Lens' JobExecution (Lude.Maybe Lude.Integer)
jeVersionNumber = Lens.lens (versionNumber :: JobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {versionNumber = a} :: JobExecution)
{-# DEPRECATED jeVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The time, in milliseconds since the epoch, when the job execution was started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStartedAt :: Lens.Lens' JobExecution (Lude.Maybe Lude.Integer)
jeStartedAt = Lens.lens (startedAt :: JobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {startedAt = a} :: JobExecution)
{-# DEPRECATED jeStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | The name of the thing that is executing the job.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeThingName :: Lens.Lens' JobExecution (Lude.Maybe Lude.Text)
jeThingName = Lens.lens (thingName :: JobExecution -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: JobExecution)
{-# DEPRECATED jeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.FromJSON JobExecution where
  parseJSON =
    Lude.withObject
      "JobExecution"
      ( \x ->
          JobExecution'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "jobId")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "approximateSecondsBeforeTimedOut")
            Lude.<*> (x Lude..:? "queuedAt")
            Lude.<*> (x Lude..:? "jobDocument")
            Lude.<*> (x Lude..:? "statusDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "executionNumber")
            Lude.<*> (x Lude..:? "versionNumber")
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..:? "thingName")
      )
