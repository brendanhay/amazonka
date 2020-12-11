-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobBookmarkEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarkEntry
  ( JobBookmarkEntry (..),

    -- * Smart constructor
    mkJobBookmarkEntry,

    -- * Lenses
    jbeJobName,
    jbeRun,
    jbeRunId,
    jbeVersion,
    jbePreviousRunId,
    jbeAttempt,
    jbeJobBookmark,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a point that a job can resume processing.
--
-- /See:/ 'mkJobBookmarkEntry' smart constructor.
data JobBookmarkEntry = JobBookmarkEntry'
  { jobName ::
      Lude.Maybe Lude.Text,
    run :: Lude.Maybe Lude.Int,
    runId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    previousRunId :: Lude.Maybe Lude.Text,
    attempt :: Lude.Maybe Lude.Int,
    jobBookmark :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobBookmarkEntry' with the minimum fields required to make a request.
--
-- * 'attempt' - The attempt ID number.
-- * 'jobBookmark' - The bookmark itself.
-- * 'jobName' - The name of the job in question.
-- * 'previousRunId' - The unique run identifier associated with the previous job run.
-- * 'run' - The run ID number.
-- * 'runId' - The run ID number.
-- * 'version' - The version of the job.
mkJobBookmarkEntry ::
  JobBookmarkEntry
mkJobBookmarkEntry =
  JobBookmarkEntry'
    { jobName = Lude.Nothing,
      run = Lude.Nothing,
      runId = Lude.Nothing,
      version = Lude.Nothing,
      previousRunId = Lude.Nothing,
      attempt = Lude.Nothing,
      jobBookmark = Lude.Nothing
    }

-- | The name of the job in question.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeJobName :: Lens.Lens' JobBookmarkEntry (Lude.Maybe Lude.Text)
jbeJobName = Lens.lens (jobName :: JobBookmarkEntry -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: JobBookmarkEntry)
{-# DEPRECATED jbeJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The run ID number.
--
-- /Note:/ Consider using 'run' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeRun :: Lens.Lens' JobBookmarkEntry (Lude.Maybe Lude.Int)
jbeRun = Lens.lens (run :: JobBookmarkEntry -> Lude.Maybe Lude.Int) (\s a -> s {run = a} :: JobBookmarkEntry)
{-# DEPRECATED jbeRun "Use generic-lens or generic-optics with 'run' instead." #-}

-- | The run ID number.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeRunId :: Lens.Lens' JobBookmarkEntry (Lude.Maybe Lude.Text)
jbeRunId = Lens.lens (runId :: JobBookmarkEntry -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: JobBookmarkEntry)
{-# DEPRECATED jbeRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The version of the job.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeVersion :: Lens.Lens' JobBookmarkEntry (Lude.Maybe Lude.Int)
jbeVersion = Lens.lens (version :: JobBookmarkEntry -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: JobBookmarkEntry)
{-# DEPRECATED jbeVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The unique run identifier associated with the previous job run.
--
-- /Note:/ Consider using 'previousRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbePreviousRunId :: Lens.Lens' JobBookmarkEntry (Lude.Maybe Lude.Text)
jbePreviousRunId = Lens.lens (previousRunId :: JobBookmarkEntry -> Lude.Maybe Lude.Text) (\s a -> s {previousRunId = a} :: JobBookmarkEntry)
{-# DEPRECATED jbePreviousRunId "Use generic-lens or generic-optics with 'previousRunId' instead." #-}

-- | The attempt ID number.
--
-- /Note:/ Consider using 'attempt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeAttempt :: Lens.Lens' JobBookmarkEntry (Lude.Maybe Lude.Int)
jbeAttempt = Lens.lens (attempt :: JobBookmarkEntry -> Lude.Maybe Lude.Int) (\s a -> s {attempt = a} :: JobBookmarkEntry)
{-# DEPRECATED jbeAttempt "Use generic-lens or generic-optics with 'attempt' instead." #-}

-- | The bookmark itself.
--
-- /Note:/ Consider using 'jobBookmark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jbeJobBookmark :: Lens.Lens' JobBookmarkEntry (Lude.Maybe Lude.Text)
jbeJobBookmark = Lens.lens (jobBookmark :: JobBookmarkEntry -> Lude.Maybe Lude.Text) (\s a -> s {jobBookmark = a} :: JobBookmarkEntry)
{-# DEPRECATED jbeJobBookmark "Use generic-lens or generic-optics with 'jobBookmark' instead." #-}

instance Lude.FromJSON JobBookmarkEntry where
  parseJSON =
    Lude.withObject
      "JobBookmarkEntry"
      ( \x ->
          JobBookmarkEntry'
            Lude.<$> (x Lude..:? "JobName")
            Lude.<*> (x Lude..:? "Run")
            Lude.<*> (x Lude..:? "RunId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "PreviousRunId")
            Lude.<*> (x Lude..:? "Attempt")
            Lude.<*> (x Lude..:? "JobBookmark")
      )
