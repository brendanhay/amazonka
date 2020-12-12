{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Predecessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Predecessor
  ( Predecessor (..),

    -- * Smart constructor
    mkPredecessor,

    -- * Lenses
    pJobName,
    pRunId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A job run that was used in the predicate of a conditional trigger that triggered this job run.
--
-- /See:/ 'mkPredecessor' smart constructor.
data Predecessor = Predecessor'
  { jobName :: Lude.Maybe Lude.Text,
    runId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Predecessor' with the minimum fields required to make a request.
--
-- * 'jobName' - The name of the job definition used by the predecessor job run.
-- * 'runId' - The job-run ID of the predecessor job run.
mkPredecessor ::
  Predecessor
mkPredecessor =
  Predecessor' {jobName = Lude.Nothing, runId = Lude.Nothing}

-- | The name of the job definition used by the predecessor job run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pJobName :: Lens.Lens' Predecessor (Lude.Maybe Lude.Text)
pJobName = Lens.lens (jobName :: Predecessor -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: Predecessor)
{-# DEPRECATED pJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The job-run ID of the predecessor job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRunId :: Lens.Lens' Predecessor (Lude.Maybe Lude.Text)
pRunId = Lens.lens (runId :: Predecessor -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: Predecessor)
{-# DEPRECATED pRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.FromJSON Predecessor where
  parseJSON =
    Lude.withObject
      "Predecessor"
      ( \x ->
          Predecessor'
            Lude.<$> (x Lude..:? "JobName") Lude.<*> (x Lude..:? "RunId")
      )
