{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecutionSummary
  ( JobExecutionSummary (..),

    -- * Smart constructor
    mkJobExecutionSummary,

    -- * Lenses
    jJobId,
    jLastUpdatedAt,
    jQueuedAt,
    jExecutionNumber,
    jVersionNumber,
    jStartedAt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a subset of information about a job execution.
--
-- /See:/ 'mkJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { jobId ::
      Lude.Maybe Lude.Text,
    lastUpdatedAt :: Lude.Maybe Lude.Integer,
    queuedAt :: Lude.Maybe Lude.Integer,
    executionNumber :: Lude.Maybe Lude.Integer,
    versionNumber :: Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecutionSummary' with the minimum fields required to make a request.
--
-- * 'executionNumber' - A number that identifies a particular job execution on a particular device.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'lastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was last updated.
-- * 'queuedAt' - The time, in milliseconds since the epoch, when the job execution was enqueued.
-- * 'startedAt' - The time, in milliseconds since the epoch, when the job execution started.
-- * 'versionNumber' - The version of the job execution. Job execution versions are incremented each time AWS IoT Jobs receives an update from a device.
mkJobExecutionSummary ::
  JobExecutionSummary
mkJobExecutionSummary =
  JobExecutionSummary'
    { jobId = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      queuedAt = Lude.Nothing,
      executionNumber = Lude.Nothing,
      versionNumber = Lude.Nothing,
      startedAt = Lude.Nothing
    }

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Text)
jJobId = Lens.lens (jobId :: JobExecutionSummary -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobExecutionSummary)
{-# DEPRECATED jJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLastUpdatedAt :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Integer)
jLastUpdatedAt = Lens.lens (lastUpdatedAt :: JobExecutionSummary -> Lude.Maybe Lude.Integer) (\s a -> s {lastUpdatedAt = a} :: JobExecutionSummary)
{-# DEPRECATED jLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time, in milliseconds since the epoch, when the job execution was enqueued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jQueuedAt :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Integer)
jQueuedAt = Lens.lens (queuedAt :: JobExecutionSummary -> Lude.Maybe Lude.Integer) (\s a -> s {queuedAt = a} :: JobExecutionSummary)
{-# DEPRECATED jQueuedAt "Use generic-lens or generic-optics with 'queuedAt' instead." #-}

-- | A number that identifies a particular job execution on a particular device.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jExecutionNumber :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Integer)
jExecutionNumber = Lens.lens (executionNumber :: JobExecutionSummary -> Lude.Maybe Lude.Integer) (\s a -> s {executionNumber = a} :: JobExecutionSummary)
{-# DEPRECATED jExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | The version of the job execution. Job execution versions are incremented each time AWS IoT Jobs receives an update from a device.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jVersionNumber :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Integer)
jVersionNumber = Lens.lens (versionNumber :: JobExecutionSummary -> Lude.Maybe Lude.Integer) (\s a -> s {versionNumber = a} :: JobExecutionSummary)
{-# DEPRECATED jVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The time, in milliseconds since the epoch, when the job execution started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStartedAt :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Integer)
jStartedAt = Lens.lens (startedAt :: JobExecutionSummary -> Lude.Maybe Lude.Integer) (\s a -> s {startedAt = a} :: JobExecutionSummary)
{-# DEPRECATED jStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

instance Lude.FromJSON JobExecutionSummary where
  parseJSON =
    Lude.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Lude.<$> (x Lude..:? "jobId")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "queuedAt")
            Lude.<*> (x Lude..:? "executionNumber")
            Lude.<*> (x Lude..:? "versionNumber")
            Lude.<*> (x Lude..:? "startedAt")
      )
