{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassificationJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassificationJobFilter
  ( DocumentClassificationJobFilter (..),

    -- * Smart constructor
    mkDocumentClassificationJobFilter,

    -- * Lenses
    dcjfSubmitTimeAfter,
    dcjfSubmitTimeBefore,
    dcjfJobName,
    dcjfJobStatus,
  )
where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering a list of document classification jobs. For more information, see the operation. You can provide only one filter parameter in each request.
--
-- /See:/ 'mkDocumentClassificationJobFilter' smart constructor.
data DocumentClassificationJobFilter = DocumentClassificationJobFilter'
  { -- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
    submitTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
    submitTimeBefore :: Lude.Maybe Lude.Timestamp,
    -- | Filters on the name of the job.
    jobName :: Lude.Maybe Lude.Text,
    -- | Filters the list based on job status. Returns only jobs with the specified status.
    jobStatus :: Lude.Maybe JobStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentClassificationJobFilter' with the minimum fields required to make a request.
--
-- * 'submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
-- * 'submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
-- * 'jobName' - Filters on the name of the job.
-- * 'jobStatus' - Filters the list based on job status. Returns only jobs with the specified status.
mkDocumentClassificationJobFilter ::
  DocumentClassificationJobFilter
mkDocumentClassificationJobFilter =
  DocumentClassificationJobFilter'
    { submitTimeAfter = Lude.Nothing,
      submitTimeBefore = Lude.Nothing,
      jobName = Lude.Nothing,
      jobStatus = Lude.Nothing
    }

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjfSubmitTimeAfter :: Lens.Lens' DocumentClassificationJobFilter (Lude.Maybe Lude.Timestamp)
dcjfSubmitTimeAfter = Lens.lens (submitTimeAfter :: DocumentClassificationJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: DocumentClassificationJobFilter)
{-# DEPRECATED dcjfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjfSubmitTimeBefore :: Lens.Lens' DocumentClassificationJobFilter (Lude.Maybe Lude.Timestamp)
dcjfSubmitTimeBefore = Lens.lens (submitTimeBefore :: DocumentClassificationJobFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: DocumentClassificationJobFilter)
{-# DEPRECATED dcjfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjfJobName :: Lens.Lens' DocumentClassificationJobFilter (Lude.Maybe Lude.Text)
dcjfJobName = Lens.lens (jobName :: DocumentClassificationJobFilter -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: DocumentClassificationJobFilter)
{-# DEPRECATED dcjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjfJobStatus :: Lens.Lens' DocumentClassificationJobFilter (Lude.Maybe JobStatus)
dcjfJobStatus = Lens.lens (jobStatus :: DocumentClassificationJobFilter -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: DocumentClassificationJobFilter)
{-# DEPRECATED dcjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

instance Lude.ToJSON DocumentClassificationJobFilter where
  toJSON DocumentClassificationJobFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore,
            ("JobName" Lude..=) Lude.<$> jobName,
            ("JobStatus" Lude..=) Lude.<$> jobStatus
          ]
      )
