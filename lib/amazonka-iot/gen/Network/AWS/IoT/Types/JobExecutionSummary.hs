-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionSummary
  ( JobExecutionSummary (..),

    -- * Smart constructor
    mkJobExecutionSummary,

    -- * Lenses
    jesStatus,
    jesLastUpdatedAt,
    jesQueuedAt,
    jesExecutionNumber,
    jesStartedAt,
  )
where

import Network.AWS.IoT.Types.JobExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The job execution summary.
--
-- /See:/ 'mkJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { status ::
      Lude.Maybe JobExecutionStatus,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    queuedAt :: Lude.Maybe Lude.Timestamp,
    executionNumber :: Lude.Maybe Lude.Integer,
    startedAt :: Lude.Maybe Lude.Timestamp
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
-- * 'executionNumber' - A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used later in commands which return or update job execution information.
-- * 'lastUpdatedAt' - The time, in seconds since the epoch, when the job execution was last updated.
-- * 'queuedAt' - The time, in seconds since the epoch, when the job execution was queued.
-- * 'startedAt' - The time, in seconds since the epoch, when the job execution started.
-- * 'status' - The status of the job execution.
mkJobExecutionSummary ::
  JobExecutionSummary
mkJobExecutionSummary =
  JobExecutionSummary'
    { status = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      queuedAt = Lude.Nothing,
      executionNumber = Lude.Nothing,
      startedAt = Lude.Nothing
    }

-- | The status of the job execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStatus :: Lens.Lens' JobExecutionSummary (Lude.Maybe JobExecutionStatus)
jesStatus = Lens.lens (status :: JobExecutionSummary -> Lude.Maybe JobExecutionStatus) (\s a -> s {status = a} :: JobExecutionSummary)
{-# DEPRECATED jesStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time, in seconds since the epoch, when the job execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesLastUpdatedAt :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Timestamp)
jesLastUpdatedAt = Lens.lens (lastUpdatedAt :: JobExecutionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: JobExecutionSummary)
{-# DEPRECATED jesLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The time, in seconds since the epoch, when the job execution was queued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesQueuedAt :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Timestamp)
jesQueuedAt = Lens.lens (queuedAt :: JobExecutionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {queuedAt = a} :: JobExecutionSummary)
{-# DEPRECATED jesQueuedAt "Use generic-lens or generic-optics with 'queuedAt' instead." #-}

-- | A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used later in commands which return or update job execution information.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesExecutionNumber :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Integer)
jesExecutionNumber = Lens.lens (executionNumber :: JobExecutionSummary -> Lude.Maybe Lude.Integer) (\s a -> s {executionNumber = a} :: JobExecutionSummary)
{-# DEPRECATED jesExecutionNumber "Use generic-lens or generic-optics with 'executionNumber' instead." #-}

-- | The time, in seconds since the epoch, when the job execution started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStartedAt :: Lens.Lens' JobExecutionSummary (Lude.Maybe Lude.Timestamp)
jesStartedAt = Lens.lens (startedAt :: JobExecutionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedAt = a} :: JobExecutionSummary)
{-# DEPRECATED jesStartedAt "Use generic-lens or generic-optics with 'startedAt' instead." #-}

instance Lude.FromJSON JobExecutionSummary where
  parseJSON =
    Lude.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "queuedAt")
            Lude.<*> (x Lude..:? "executionNumber")
            Lude.<*> (x Lude..:? "startedAt")
      )
