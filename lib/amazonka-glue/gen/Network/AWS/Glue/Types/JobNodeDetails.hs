-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobNodeDetails
  ( JobNodeDetails (..),

    -- * Smart constructor
    mkJobNodeDetails,

    -- * Lenses
    jndJobRuns,
  )
where

import Network.AWS.Glue.Types.JobRun
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of a Job node present in the workflow.
--
-- /See:/ 'mkJobNodeDetails' smart constructor.
newtype JobNodeDetails = JobNodeDetails'
  { jobRuns ::
      Lude.Maybe [JobRun]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobNodeDetails' with the minimum fields required to make a request.
--
-- * 'jobRuns' - The information for the job runs represented by the job node.
mkJobNodeDetails ::
  JobNodeDetails
mkJobNodeDetails = JobNodeDetails' {jobRuns = Lude.Nothing}

-- | The information for the job runs represented by the job node.
--
-- /Note:/ Consider using 'jobRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jndJobRuns :: Lens.Lens' JobNodeDetails (Lude.Maybe [JobRun])
jndJobRuns = Lens.lens (jobRuns :: JobNodeDetails -> Lude.Maybe [JobRun]) (\s a -> s {jobRuns = a} :: JobNodeDetails)
{-# DEPRECATED jndJobRuns "Use generic-lens or generic-optics with 'jobRuns' instead." #-}

instance Lude.FromJSON JobNodeDetails where
  parseJSON =
    Lude.withObject
      "JobNodeDetails"
      ( \x ->
          JobNodeDetails'
            Lude.<$> (x Lude..:? "JobRuns" Lude..!= Lude.mempty)
      )
