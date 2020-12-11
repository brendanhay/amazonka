-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobSummary
  ( JobSummary (..),

    -- * Smart constructor
    mkJobSummary,

    -- * Lenses
    jsStatus,
    jsJobId,
    jsLastUpdatedAt,
    jsJobARN,
    jsCreatedAt,
    jsThingGroupId,
    jsCompletedAt,
    jsTargetSelection,
  )
where

import Network.AWS.IoT.Types.JobStatus
import Network.AWS.IoT.Types.TargetSelection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The job summary.
--
-- /See:/ 'mkJobSummary' smart constructor.
data JobSummary = JobSummary'
  { status :: Lude.Maybe JobStatus,
    jobId :: Lude.Maybe Lude.Text,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    jobARN :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    thingGroupId :: Lude.Maybe Lude.Text,
    completedAt :: Lude.Maybe Lude.Timestamp,
    targetSelection :: Lude.Maybe TargetSelection
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
-- * 'completedAt' - The time, in seconds since the epoch, when the job completed.
-- * 'createdAt' - The time, in seconds since the epoch, when the job was created.
-- * 'jobARN' - The job ARN.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
-- * 'lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
-- * 'status' - The job summary status.
-- * 'targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
-- * 'thingGroupId' - The ID of the thing group.
mkJobSummary ::
  JobSummary
mkJobSummary =
  JobSummary'
    { status = Lude.Nothing,
      jobId = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      jobARN = Lude.Nothing,
      createdAt = Lude.Nothing,
      thingGroupId = Lude.Nothing,
      completedAt = Lude.Nothing,
      targetSelection = Lude.Nothing
    }

-- | The job summary status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStatus :: Lens.Lens' JobSummary (Lude.Maybe JobStatus)
jsStatus = Lens.lens (status :: JobSummary -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: JobSummary)
{-# DEPRECATED jsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobId :: Lens.Lens' JobSummary (Lude.Maybe Lude.Text)
jsJobId = Lens.lens (jobId :: JobSummary -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobSummary)
{-# DEPRECATED jsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The time, in seconds since the epoch, when the job was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsLastUpdatedAt :: Lens.Lens' JobSummary (Lude.Maybe Lude.Timestamp)
jsLastUpdatedAt = Lens.lens (lastUpdatedAt :: JobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: JobSummary)
{-# DEPRECATED jsLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The job ARN.
--
-- /Note:/ Consider using 'jobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobARN :: Lens.Lens' JobSummary (Lude.Maybe Lude.Text)
jsJobARN = Lens.lens (jobARN :: JobSummary -> Lude.Maybe Lude.Text) (\s a -> s {jobARN = a} :: JobSummary)
{-# DEPRECATED jsJobARN "Use generic-lens or generic-optics with 'jobARN' instead." #-}

-- | The time, in seconds since the epoch, when the job was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsCreatedAt :: Lens.Lens' JobSummary (Lude.Maybe Lude.Timestamp)
jsCreatedAt = Lens.lens (createdAt :: JobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: JobSummary)
{-# DEPRECATED jsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID of the thing group.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsThingGroupId :: Lens.Lens' JobSummary (Lude.Maybe Lude.Text)
jsThingGroupId = Lens.lens (thingGroupId :: JobSummary -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupId = a} :: JobSummary)
{-# DEPRECATED jsThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The time, in seconds since the epoch, when the job completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsCompletedAt :: Lens.Lens' JobSummary (Lude.Maybe Lude.Timestamp)
jsCompletedAt = Lens.lens (completedAt :: JobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedAt = a} :: JobSummary)
{-# DEPRECATED jsCompletedAt "Use generic-lens or generic-optics with 'completedAt' instead." #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTargetSelection :: Lens.Lens' JobSummary (Lude.Maybe TargetSelection)
jsTargetSelection = Lens.lens (targetSelection :: JobSummary -> Lude.Maybe TargetSelection) (\s a -> s {targetSelection = a} :: JobSummary)
{-# DEPRECATED jsTargetSelection "Use generic-lens or generic-optics with 'targetSelection' instead." #-}

instance Lude.FromJSON JobSummary where
  parseJSON =
    Lude.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "jobId")
            Lude.<*> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "jobArn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "thingGroupId")
            Lude.<*> (x Lude..:? "completedAt")
            Lude.<*> (x Lude..:? "targetSelection")
      )
