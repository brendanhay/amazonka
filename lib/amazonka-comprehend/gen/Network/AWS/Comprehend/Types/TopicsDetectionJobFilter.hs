-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
  ( TopicsDetectionJobFilter (..),

    -- * Smart constructor
    mkTopicsDetectionJobFilter,

    -- * Lenses
    tdjfSubmitTimeAfter,
    tdjfSubmitTimeBefore,
    tdjfJobName,
    tdjfJobStatus,
  )
where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering topic detection jobs. For more information, see .
--
-- /See:/ 'mkTopicsDetectionJobFilter' smart constructor.
data TopicsDetectionJobFilter = TopicsDetectionJobFilter'
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

-- | Creates a value of 'TopicsDetectionJobFilter' with the minimum fields required to make a request.
--
-- * 'jobName' -
-- * 'jobStatus' - Filters the list of topic detection jobs based on job status. Returns only jobs with the specified status.
-- * 'submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
-- * 'submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
mkTopicsDetectionJobFilter ::
  TopicsDetectionJobFilter
mkTopicsDetectionJobFilter =
  TopicsDetectionJobFilter'
    { submitTimeAfter = Lude.Nothing,
      submitTimeBefore = Lude.Nothing,
      jobName = Lude.Nothing,
      jobStatus = Lude.Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfSubmitTimeAfter :: Lens.Lens' TopicsDetectionJobFilter (Lude.Maybe Lude.Timestamp)
tdjfSubmitTimeAfter = Lens.lens (submitTimeAfter :: TopicsDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: TopicsDetectionJobFilter)
{-# DEPRECATED tdjfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfSubmitTimeBefore :: Lens.Lens' TopicsDetectionJobFilter (Lude.Maybe Lude.Timestamp)
tdjfSubmitTimeBefore = Lens.lens (submitTimeBefore :: TopicsDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: TopicsDetectionJobFilter)
{-# DEPRECATED tdjfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

-- |
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfJobName :: Lens.Lens' TopicsDetectionJobFilter (Lude.Maybe Lude.Text)
tdjfJobName = Lens.lens (jobName :: TopicsDetectionJobFilter -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: TopicsDetectionJobFilter)
{-# DEPRECATED tdjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list of topic detection jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfJobStatus :: Lens.Lens' TopicsDetectionJobFilter (Lude.Maybe JobStatus)
tdjfJobStatus = Lens.lens (jobStatus :: TopicsDetectionJobFilter -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: TopicsDetectionJobFilter)
{-# DEPRECATED tdjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

instance Lude.ToJSON TopicsDetectionJobFilter where
  toJSON TopicsDetectionJobFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("JobStatus" Lude..=) Lude.<$> jobStatus
          ]
      )
