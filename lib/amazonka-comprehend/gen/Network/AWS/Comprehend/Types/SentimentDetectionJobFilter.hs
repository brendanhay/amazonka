{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
  ( SentimentDetectionJobFilter (..),

    -- * Smart constructor
    mkSentimentDetectionJobFilter,

    -- * Lenses
    sdjfSubmitTimeAfter,
    sdjfSubmitTimeBefore,
    sdjfJobName,
    sdjfJobStatus,
  )
where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
-- /See:/ 'mkSentimentDetectionJobFilter' smart constructor.
data SentimentDetectionJobFilter = SentimentDetectionJobFilter'
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

-- | Creates a value of 'SentimentDetectionJobFilter' with the minimum fields required to make a request.
--
-- * 'jobName' - Filters on the name of the job.
-- * 'jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the specified status.
-- * 'submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
-- * 'submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
mkSentimentDetectionJobFilter ::
  SentimentDetectionJobFilter
mkSentimentDetectionJobFilter =
  SentimentDetectionJobFilter'
    { submitTimeAfter = Lude.Nothing,
      submitTimeBefore = Lude.Nothing,
      jobName = Lude.Nothing,
      jobStatus = Lude.Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjfSubmitTimeAfter :: Lens.Lens' SentimentDetectionJobFilter (Lude.Maybe Lude.Timestamp)
sdjfSubmitTimeAfter = Lens.lens (submitTimeAfter :: SentimentDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: SentimentDetectionJobFilter)
{-# DEPRECATED sdjfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjfSubmitTimeBefore :: Lens.Lens' SentimentDetectionJobFilter (Lude.Maybe Lude.Timestamp)
sdjfSubmitTimeBefore = Lens.lens (submitTimeBefore :: SentimentDetectionJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: SentimentDetectionJobFilter)
{-# DEPRECATED sdjfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjfJobName :: Lens.Lens' SentimentDetectionJobFilter (Lude.Maybe Lude.Text)
sdjfJobName = Lens.lens (jobName :: SentimentDetectionJobFilter -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: SentimentDetectionJobFilter)
{-# DEPRECATED sdjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdjfJobStatus :: Lens.Lens' SentimentDetectionJobFilter (Lude.Maybe JobStatus)
sdjfJobStatus = Lens.lens (jobStatus :: SentimentDetectionJobFilter -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: SentimentDetectionJobFilter)
{-# DEPRECATED sdjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

instance Lude.ToJSON SentimentDetectionJobFilter where
  toJSON SentimentDetectionJobFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("JobStatus" Lude..=) Lude.<$> jobStatus
          ]
      )
