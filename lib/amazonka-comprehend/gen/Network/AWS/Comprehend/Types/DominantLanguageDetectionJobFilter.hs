{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
  ( DominantLanguageDetectionJobFilter (..),

    -- * Smart constructor
    mkDominantLanguageDetectionJobFilter,

    -- * Lenses
    dldjfSubmitTimeAfter,
    dldjfSubmitTimeBefore,
    dldjfJobName,
    dldjfJobStatus,
  )
where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
-- /See:/ 'mkDominantLanguageDetectionJobFilter' smart constructor.
data DominantLanguageDetectionJobFilter = DominantLanguageDetectionJobFilter'
  { -- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
    submitTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
    submitTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | Filters on the name of the job.
    jobName :: Lude.Maybe Lude.Text,
    -- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
    jobStatus :: Lude.Maybe JobStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DominantLanguageDetectionJobFilter' with the minimum fields required to make a request.
--
-- * 'submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
-- * 'submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
-- * 'jobName' - Filters on the name of the job.
-- * 'jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
mkDominantLanguageDetectionJobFilter ::
  DominantLanguageDetectionJobFilter
mkDominantLanguageDetectionJobFilter =
  DominantLanguageDetectionJobFilter'
    { submitTimeAfter =
        Lude.Nothing,
      submitTimeBefore = Lude.Nothing,
      jobName = Lude.Nothing,
      jobStatus = Lude.Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfSubmitTimeAfter :: Lens.Lens' DominantLanguageDetectionJobFilter (Lude.Maybe Lude.Timestamp)
dldjfSubmitTimeAfter = Lens.lens (submitTimeAfter :: DominantLanguageDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: DominantLanguageDetectionJobFilter)
{-# DEPRECATED dldjfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfSubmitTimeBefore :: Lens.Lens' DominantLanguageDetectionJobFilter (Lude.Maybe Lude.Timestamp)
dldjfSubmitTimeBefore = Lens.lens (submitTimeBefore :: DominantLanguageDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: DominantLanguageDetectionJobFilter)
{-# DEPRECATED dldjfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfJobName :: Lens.Lens' DominantLanguageDetectionJobFilter (Lude.Maybe Lude.Text)
dldjfJobName = Lens.lens (jobName :: DominantLanguageDetectionJobFilter -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: DominantLanguageDetectionJobFilter)
{-# DEPRECATED dldjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfJobStatus :: Lens.Lens' DominantLanguageDetectionJobFilter (Lude.Maybe JobStatus)
dldjfJobStatus = Lens.lens (jobStatus :: DominantLanguageDetectionJobFilter -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: DominantLanguageDetectionJobFilter)
{-# DEPRECATED dldjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

instance Lude.ToJSON DominantLanguageDetectionJobFilter where
  toJSON DominantLanguageDetectionJobFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("JobStatus" Lude..=) Lude.<$> jobStatus
          ]
      )
