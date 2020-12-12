{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.TextTranslationJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.TextTranslationJobFilter
  ( TextTranslationJobFilter (..),

    -- * Smart constructor
    mkTextTranslationJobFilter,

    -- * Lenses
    ttjfSubmittedBeforeTime,
    ttjfSubmittedAfterTime,
    ttjfJobName,
    ttjfJobStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Translate.Types.JobStatus

-- | Provides information for filtering a list of translation jobs. For more information, see 'ListTextTranslationJobs' .
--
-- /See:/ 'mkTextTranslationJobFilter' smart constructor.
data TextTranslationJobFilter = TextTranslationJobFilter'
  { submittedBeforeTime ::
      Lude.Maybe Lude.Timestamp,
    submittedAfterTime ::
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

-- | Creates a value of 'TextTranslationJobFilter' with the minimum fields required to make a request.
--
-- * 'jobName' - Filters the list of jobs by name.
-- * 'jobStatus' - Filters the list of jobs based by job status.
-- * 'submittedAfterTime' - Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
-- * 'submittedBeforeTime' - Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
mkTextTranslationJobFilter ::
  TextTranslationJobFilter
mkTextTranslationJobFilter =
  TextTranslationJobFilter'
    { submittedBeforeTime = Lude.Nothing,
      submittedAfterTime = Lude.Nothing,
      jobName = Lude.Nothing,
      jobStatus = Lude.Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submittedBeforeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfSubmittedBeforeTime :: Lens.Lens' TextTranslationJobFilter (Lude.Maybe Lude.Timestamp)
ttjfSubmittedBeforeTime = Lens.lens (submittedBeforeTime :: TextTranslationJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submittedBeforeTime = a} :: TextTranslationJobFilter)
{-# DEPRECATED ttjfSubmittedBeforeTime "Use generic-lens or generic-optics with 'submittedBeforeTime' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing and returns only the jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submittedAfterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfSubmittedAfterTime :: Lens.Lens' TextTranslationJobFilter (Lude.Maybe Lude.Timestamp)
ttjfSubmittedAfterTime = Lens.lens (submittedAfterTime :: TextTranslationJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submittedAfterTime = a} :: TextTranslationJobFilter)
{-# DEPRECATED ttjfSubmittedAfterTime "Use generic-lens or generic-optics with 'submittedAfterTime' instead." #-}

-- | Filters the list of jobs by name.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfJobName :: Lens.Lens' TextTranslationJobFilter (Lude.Maybe Lude.Text)
ttjfJobName = Lens.lens (jobName :: TextTranslationJobFilter -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: TextTranslationJobFilter)
{-# DEPRECATED ttjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list of jobs based by job status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttjfJobStatus :: Lens.Lens' TextTranslationJobFilter (Lude.Maybe JobStatus)
ttjfJobStatus = Lens.lens (jobStatus :: TextTranslationJobFilter -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: TextTranslationJobFilter)
{-# DEPRECATED ttjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

instance Lude.ToJSON TextTranslationJobFilter where
  toJSON TextTranslationJobFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubmittedBeforeTime" Lude..=) Lude.<$> submittedBeforeTime,
            ("SubmittedAfterTime" Lude..=) Lude.<$> submittedAfterTime,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("JobStatus" Lude..=) Lude.<$> jobStatus
          ]
      )
