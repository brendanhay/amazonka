-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
  ( PiiEntitiesDetectionJobFilter (..),

    -- * Smart constructor
    mkPiiEntitiesDetectionJobFilter,

    -- * Lenses
    pedjfSubmitTimeAfter,
    pedjfSubmitTimeBefore,
    pedjfJobName,
    pedjfJobStatus,
  )
where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering a list of PII entity detection jobs.
--
-- /See:/ 'mkPiiEntitiesDetectionJobFilter' smart constructor.
data PiiEntitiesDetectionJobFilter = PiiEntitiesDetectionJobFilter'
  { submitTimeAfter ::
      Lude.Maybe Lude.Timestamp,
    submitTimeBefore ::
      Lude.Maybe Lude.Timestamp,
    jobName :: Lude.Maybe Lude.Text,
    jobStatus ::
      Lude.Maybe JobStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PiiEntitiesDetectionJobFilter' with the minimum fields required to make a request.
--
-- * 'jobName' - Filters on the name of the job.
-- * 'jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
-- * 'submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
-- * 'submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
mkPiiEntitiesDetectionJobFilter ::
  PiiEntitiesDetectionJobFilter
mkPiiEntitiesDetectionJobFilter =
  PiiEntitiesDetectionJobFilter'
    { submitTimeAfter = Lude.Nothing,
      submitTimeBefore = Lude.Nothing,
      jobName = Lude.Nothing,
      jobStatus = Lude.Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfSubmitTimeAfter :: Lens.Lens' PiiEntitiesDetectionJobFilter (Lude.Maybe Lude.Timestamp)
pedjfSubmitTimeAfter = Lens.lens (submitTimeAfter :: PiiEntitiesDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: PiiEntitiesDetectionJobFilter)
{-# DEPRECATED pedjfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfSubmitTimeBefore :: Lens.Lens' PiiEntitiesDetectionJobFilter (Lude.Maybe Lude.Timestamp)
pedjfSubmitTimeBefore = Lens.lens (submitTimeBefore :: PiiEntitiesDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: PiiEntitiesDetectionJobFilter)
{-# DEPRECATED pedjfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfJobName :: Lens.Lens' PiiEntitiesDetectionJobFilter (Lude.Maybe Lude.Text)
pedjfJobName = Lens.lens (jobName :: PiiEntitiesDetectionJobFilter -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: PiiEntitiesDetectionJobFilter)
{-# DEPRECATED pedjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfJobStatus :: Lens.Lens' PiiEntitiesDetectionJobFilter (Lude.Maybe JobStatus)
pedjfJobStatus = Lens.lens (jobStatus :: PiiEntitiesDetectionJobFilter -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: PiiEntitiesDetectionJobFilter)
{-# DEPRECATED pedjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

instance Lude.ToJSON PiiEntitiesDetectionJobFilter where
  toJSON PiiEntitiesDetectionJobFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("JobStatus" Lude..=) Lude.<$> jobStatus
          ]
      )
