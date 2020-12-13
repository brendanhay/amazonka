{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecution
  ( JobExecution (..),

    -- * Smart constructor
    mkJobExecution,

    -- * Lenses
    jeStatus,
    jeJobId,
    jeLastUpdatedAt,
    jeApproximateSecondsBeforeTimedOut,
    jeQueuedAt,
    jeStatusDetails,
    jeThingARN,
    jeExecutionNumber,
    jeVersionNumber,
    jeStartedAt,
    jeForceCanceled,
  )
where

import Network.AWS.IoT.Types.JobExecutionStatus
import Network.AWS.IoT.Types.JobExecutionStatusDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The job execution object represents the execution of a job on a particular device.
--
-- /See:/ 'mkJobExecution' smart constructor.
data JobExecution = JobExecution'
  { -- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED, TIMED_OUT, CANCELED, or REJECTED).
    status :: Lude.Maybe JobExecutionStatus,
    -- | The unique identifier you assigned to the job when it was created.
    jobId :: Lude.Maybe Lude.Text,
    -- | The time, in seconds since the epoch, when the job execution was last updated.
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    -- | The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ . The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual job execution timeout can occur up to 60 seconds later than the estimated duration. This value will not be included if the job execution has reached a terminal status.
    approximateSecondsBeforeTimedOut :: Lude.Maybe Lude.Integer,
    -- | The time, in seconds since the epoch, when the job execution was queued.
    queuedAt :: Lude.Maybe Lude.Timestamp,
    -- | A collection of name/value pairs that describe the status of the job execution.
    statusDetails :: Lude.Maybe JobExecutionStatusDetails,
    -- | The ARN of the thing on which the job execution is running.
    thingARN :: Lude.Maybe Lude.Text,
    -- | A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used in commands which return or update job execution information.
    executionNumber :: Lude.Maybe Lude.Integer,
    -- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
    versionNumber :: Lude.Maybe Lude.Integer,
    -- | The time, in seconds since the epoch, when the job execution started.
    startedAt :: Lude.Maybe Lude.Timestamp,
    -- | Will be @true@ if the job execution was canceled with the optional @force@ parameter set to @true@ .
    forceCanceled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecution' with the minimum fields required to make a request.
--
-- * 'status' - The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED, TIMED_OUT, CANCELED, or REJECTED).
-- * 'jobId' - The unique identifier you assigned to the job when it was created.
-- * 'lastUpdatedAt' - The time, in seconds since the epoch, when the job execution was last updated.
-- * 'approximateSecondsBeforeTimedOut' - The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ . The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual job execution timeout can occur up to 60 seconds later than the estimated duration. This value will not be included if the job execution has reached a terminal status.
-- * 'queuedAt' - The time, in seconds since the epoch, when the job execution was queued.
-- * 'statusDetails' - A collection of name/value pairs that describe the status of the job execution.
-- * 'thingARN' - The ARN of the thing on which the job execution is running.
-- * 'executionNumber' - A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used in commands which return or update job execution information.
-- * 'versionNumber' - The version of the job execution. Job execution versions are incremented each time they are updated by a device.
-- * 'startedAt' - The time, in seconds since the epoch, when the job execution started.
-- * 'forceCanceled' - Will be @true@ if the job execution was canceled with the optional @force@ parameter set to @true@ .
mkJobExecution ::
  JobExecution
mkJobExecution =
  JobExecution'
    { status = Lude.Nothing,
      jobId = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      approximateSecondsBeforeTimedOut = Lude.Nothing,
      queuedAt = Lude.Nothing,
      statusDetails = Lude.Nothing,
      thingARN = Lude.Nothing,
      executionNumber = Lude.Nothing,
      versionNumber = Lude.Nothing,
      startedAt = Lude.Nothing,
      forceCanceled = Lude.Nothing
    }

-- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED, TIMED_OUT, CANCELED, or REJECTED).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatus :: Lens.Lens' JobExecution (Lude.Maybe JobExecutionStatus)
jeStatus = Lens.lens (status :: JobExecution -> Lude.Maybe JobExecutionStatus) (\s a -> s {status = a} :: JobExecution)
{-# DEPRECATED jeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier you assigned to the job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeJobId :: Lens.Lens' JobExecution (Lude.Maybe Lude.Text)
jeJobId = Lens.lens (jobId :: JobExecution -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobExecution)
{-# DEPRECATED jeJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The time, in seconds since the epoch, when the job execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeLastUpdatedAt :: Lens.Lens' JobExecution (Lude.Maybe Lude.Timestamp)
jeLastUpdatedAt = Lens.lens (lastUpdatedAt :: JobExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: JobExecution)
{-# DEPRECATED jeLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ . The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual job execution timeout can occur up to 60 seconds later than the estimated duration. This value will not be included if the job execution has reached a terminal status.
--
-- /Note:/ Consider using 'approximateSecondsBeforeTimedOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeApproximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Lude.Maybe Lude.Integer)
jeApproximateSecondsBeforeTimedOut = Lens.lens (approximateSecondsBeforeTimedOut :: JobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {approximateSecondsBeforeTimedOut = a} :: JobExecution)
{-# DEPRECATED jeApproximateSecondsBeforeTimedOut "Use generic-lens or generic-optics with 'approximateSecondsBeforeTimedOut' instead." #-}

-- | The time, in seconds since the epoch, when the job execution was queued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeQueuedAt :: Lens.Lens' JobExecution (Lude.Maybe Lude.Timestamp)
jeQueuedAt = Lens.lens (queuedAt :: JobExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {queuedAt = a} :: JobExecution)
{-# DEPRECATED jeQueuedAt "Use generic-lens or generic-optics with 'queuedAt' instead." #-}

-- | A collection of name/value pairs that describe the status of the job execution.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatusDetails :: Lens.Lens' JobExecution (Lude.Maybe JobExecutionStatusDetails)
jeStatusDetails = Lens.lens (statusDetails :: JobExecution -> Lude.Maybe JobExecutionStatusDetails) (\s a -> s {statusDetails = a} :: JobExecution)
{-# DEPRECATED jeStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | The ARN of the thing on which the job execution is running.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeThingARN :: Lens.Lens' JobExecution (Lude.Maybe Lude.Text)
jeThingARN = Lens.lens (thingARN :: JobExecution -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: JobExecution)
{-# DEPRECATED jeThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used in commands which return or update job execution information.
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

-- | The time, in seconds since the epoch, when the job execution started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStartedAt :: Lens.Lens' JobExecution (Lude.Maybe Lude.Timestamp)
jeStartedAt = Lens.lens (startedAt :: JobExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: JobExecution)
{-# DEPRECATED jeStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

-- | Will be @true@ if the job execution was canceled with the optional @force@ parameter set to @true@ .
--
-- /Note:/ Consider using 'forceCanceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeForceCanceled :: Lens.Lens' JobExecution (Lude.Maybe Lude.Bool)
jeForceCanceled = Lens.lens (forceCanceled :: JobExecution -> Lude.Maybe Lude.Bool) (\s a -> s {forceCanceled = a} :: JobExecution)
{-# DEPRECATED jeForceCanceled "Use generic-lens or generic-optics with 'forceCanceled' instead." #-}

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
            Lude.<*> (x Lude..:? "statusDetails")
            Lude.<*> (x Lude..:? "thingArn")
            Lude.<*> (x Lude..:? "executionNumber")
            Lude.<*> (x Lude..:? "versionNumber")
            Lude.<*> (x Lude..:? "startedAt")
            Lude.<*> (x Lude..:? "forceCanceled")
      )
