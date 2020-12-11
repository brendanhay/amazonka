-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
  ( EntitiesDetectionJobFilter (..),

    -- * Smart constructor
    mkEntitiesDetectionJobFilter,

    -- * Lenses
    edjfSubmitTimeAfter,
    edjfSubmitTimeBefore,
    edjfJobName,
    edjfJobStatus,
  )
where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
-- /See:/ 'mkEntitiesDetectionJobFilter' smart constructor.
data EntitiesDetectionJobFilter = EntitiesDetectionJobFilter'
  { submitTimeAfter ::
      Lude.Maybe Lude.Timestamp,
    submitTimeBefore ::
      Lude.Maybe Lude.Timestamp,
    jobName :: Lude.Maybe Lude.Text,
    jobStatus :: Lude.Maybe JobStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntitiesDetectionJobFilter' with the minimum fields required to make a request.
--
-- * 'jobName' - Filters on the name of the job.
-- * 'jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
-- * 'submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
-- * 'submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
mkEntitiesDetectionJobFilter ::
  EntitiesDetectionJobFilter
mkEntitiesDetectionJobFilter =
  EntitiesDetectionJobFilter'
    { submitTimeAfter = Lude.Nothing,
      submitTimeBefore = Lude.Nothing,
      jobName = Lude.Nothing,
      jobStatus = Lude.Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfSubmitTimeAfter :: Lens.Lens' EntitiesDetectionJobFilter (Lude.Maybe Lude.Timestamp)
edjfSubmitTimeAfter = Lens.lens (submitTimeAfter :: EntitiesDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: EntitiesDetectionJobFilter)
{-# DEPRECATED edjfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfSubmitTimeBefore :: Lens.Lens' EntitiesDetectionJobFilter (Lude.Maybe Lude.Timestamp)
edjfSubmitTimeBefore = Lens.lens (submitTimeBefore :: EntitiesDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: EntitiesDetectionJobFilter)
{-# DEPRECATED edjfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfJobName :: Lens.Lens' EntitiesDetectionJobFilter (Lude.Maybe Lude.Text)
edjfJobName = Lens.lens (jobName :: EntitiesDetectionJobFilter -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: EntitiesDetectionJobFilter)
{-# DEPRECATED edjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfJobStatus :: Lens.Lens' EntitiesDetectionJobFilter (Lude.Maybe JobStatus)
edjfJobStatus = Lens.lens (jobStatus :: EntitiesDetectionJobFilter -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: EntitiesDetectionJobFilter)
{-# DEPRECATED edjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

instance Lude.ToJSON EntitiesDetectionJobFilter where
  toJSON EntitiesDetectionJobFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("JobStatus" Lude..=) Lude.<$> jobStatus
          ]
      )
