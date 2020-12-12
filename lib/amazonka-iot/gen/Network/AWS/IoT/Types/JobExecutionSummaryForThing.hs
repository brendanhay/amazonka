{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummaryForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionSummaryForThing
  ( JobExecutionSummaryForThing (..),

    -- * Smart constructor
    mkJobExecutionSummaryForThing,

    -- * Lenses
    jesftJobId,
    jesftJobExecutionSummary,
  )
where

import Network.AWS.IoT.Types.JobExecutionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The job execution summary for a thing.
--
-- /See:/ 'mkJobExecutionSummaryForThing' smart constructor.
data JobExecutionSummaryForThing = JobExecutionSummaryForThing'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobExecutionSummary ::
      Lude.Maybe JobExecutionSummary
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecutionSummaryForThing' with the minimum fields required to make a request.
--
-- * 'jobExecutionSummary' - Contains a subset of information about a job execution.
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
mkJobExecutionSummaryForThing ::
  JobExecutionSummaryForThing
mkJobExecutionSummaryForThing =
  JobExecutionSummaryForThing'
    { jobId = Lude.Nothing,
      jobExecutionSummary = Lude.Nothing
    }

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesftJobId :: Lens.Lens' JobExecutionSummaryForThing (Lude.Maybe Lude.Text)
jesftJobId = Lens.lens (jobId :: JobExecutionSummaryForThing -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobExecutionSummaryForThing)
{-# DEPRECATED jesftJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Contains a subset of information about a job execution.
--
-- /Note:/ Consider using 'jobExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesftJobExecutionSummary :: Lens.Lens' JobExecutionSummaryForThing (Lude.Maybe JobExecutionSummary)
jesftJobExecutionSummary = Lens.lens (jobExecutionSummary :: JobExecutionSummaryForThing -> Lude.Maybe JobExecutionSummary) (\s a -> s {jobExecutionSummary = a} :: JobExecutionSummaryForThing)
{-# DEPRECATED jesftJobExecutionSummary "Use generic-lens or generic-optics with 'jobExecutionSummary' instead." #-}

instance Lude.FromJSON JobExecutionSummaryForThing where
  parseJSON =
    Lude.withObject
      "JobExecutionSummaryForThing"
      ( \x ->
          JobExecutionSummaryForThing'
            Lude.<$> (x Lude..:? "jobId") Lude.<*> (x Lude..:? "jobExecutionSummary")
      )
