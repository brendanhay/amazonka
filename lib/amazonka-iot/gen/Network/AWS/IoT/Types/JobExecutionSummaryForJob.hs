-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummaryForJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionSummaryForJob
  ( JobExecutionSummaryForJob (..),

    -- * Smart constructor
    mkJobExecutionSummaryForJob,

    -- * Lenses
    jesfjJobExecutionSummary,
    jesfjThingARN,
  )
where

import Network.AWS.IoT.Types.JobExecutionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains a summary of information about job executions for a specific job.
--
-- /See:/ 'mkJobExecutionSummaryForJob' smart constructor.
data JobExecutionSummaryForJob = JobExecutionSummaryForJob'
  { jobExecutionSummary ::
      Lude.Maybe JobExecutionSummary,
    thingARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobExecutionSummaryForJob' with the minimum fields required to make a request.
--
-- * 'jobExecutionSummary' - Contains a subset of information about a job execution.
-- * 'thingARN' - The ARN of the thing on which the job execution is running.
mkJobExecutionSummaryForJob ::
  JobExecutionSummaryForJob
mkJobExecutionSummaryForJob =
  JobExecutionSummaryForJob'
    { jobExecutionSummary = Lude.Nothing,
      thingARN = Lude.Nothing
    }

-- | Contains a subset of information about a job execution.
--
-- /Note:/ Consider using 'jobExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesfjJobExecutionSummary :: Lens.Lens' JobExecutionSummaryForJob (Lude.Maybe JobExecutionSummary)
jesfjJobExecutionSummary = Lens.lens (jobExecutionSummary :: JobExecutionSummaryForJob -> Lude.Maybe JobExecutionSummary) (\s a -> s {jobExecutionSummary = a} :: JobExecutionSummaryForJob)
{-# DEPRECATED jesfjJobExecutionSummary "Use generic-lens or generic-optics with 'jobExecutionSummary' instead." #-}

-- | The ARN of the thing on which the job execution is running.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesfjThingARN :: Lens.Lens' JobExecutionSummaryForJob (Lude.Maybe Lude.Text)
jesfjThingARN = Lens.lens (thingARN :: JobExecutionSummaryForJob -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: JobExecutionSummaryForJob)
{-# DEPRECATED jesfjThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

instance Lude.FromJSON JobExecutionSummaryForJob where
  parseJSON =
    Lude.withObject
      "JobExecutionSummaryForJob"
      ( \x ->
          JobExecutionSummaryForJob'
            Lude.<$> (x Lude..:? "jobExecutionSummary")
            Lude.<*> (x Lude..:? "thingArn")
      )
