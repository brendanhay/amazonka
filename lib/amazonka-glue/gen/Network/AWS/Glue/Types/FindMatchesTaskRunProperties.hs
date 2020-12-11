-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.FindMatchesTaskRunProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesTaskRunProperties
  ( FindMatchesTaskRunProperties (..),

    -- * Smart constructor
    mkFindMatchesTaskRunProperties,

    -- * Lenses
    fmtrpJobId,
    fmtrpJobName,
    fmtrpJobRunId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies configuration properties for a Find Matches task run.
--
-- /See:/ 'mkFindMatchesTaskRunProperties' smart constructor.
data FindMatchesTaskRunProperties = FindMatchesTaskRunProperties'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobName :: Lude.Maybe Lude.Text,
    jobRunId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FindMatchesTaskRunProperties' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID for the Find Matches task run.
-- * 'jobName' - The name assigned to the job for the Find Matches task run.
-- * 'jobRunId' - The job run ID for the Find Matches task run.
mkFindMatchesTaskRunProperties ::
  FindMatchesTaskRunProperties
mkFindMatchesTaskRunProperties =
  FindMatchesTaskRunProperties'
    { jobId = Lude.Nothing,
      jobName = Lude.Nothing,
      jobRunId = Lude.Nothing
    }

-- | The job ID for the Find Matches task run.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmtrpJobId :: Lens.Lens' FindMatchesTaskRunProperties (Lude.Maybe Lude.Text)
fmtrpJobId = Lens.lens (jobId :: FindMatchesTaskRunProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: FindMatchesTaskRunProperties)
{-# DEPRECATED fmtrpJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The name assigned to the job for the Find Matches task run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmtrpJobName :: Lens.Lens' FindMatchesTaskRunProperties (Lude.Maybe Lude.Text)
fmtrpJobName = Lens.lens (jobName :: FindMatchesTaskRunProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: FindMatchesTaskRunProperties)
{-# DEPRECATED fmtrpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The job run ID for the Find Matches task run.
--
-- /Note:/ Consider using 'jobRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmtrpJobRunId :: Lens.Lens' FindMatchesTaskRunProperties (Lude.Maybe Lude.Text)
fmtrpJobRunId = Lens.lens (jobRunId :: FindMatchesTaskRunProperties -> Lude.Maybe Lude.Text) (\s a -> s {jobRunId = a} :: FindMatchesTaskRunProperties)
{-# DEPRECATED fmtrpJobRunId "Use generic-lens or generic-optics with 'jobRunId' instead." #-}

instance Lude.FromJSON FindMatchesTaskRunProperties where
  parseJSON =
    Lude.withObject
      "FindMatchesTaskRunProperties"
      ( \x ->
          FindMatchesTaskRunProperties'
            Lude.<$> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "JobRunId")
      )
