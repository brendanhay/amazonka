{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.JobNodeDetails
  ( JobNodeDetails (..)
  -- * Smart constructor
  , mkJobNodeDetails
  -- * Lenses
  , jndJobRuns
  ) where

import qualified Network.AWS.Glue.Types.JobRun as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a Job node present in the workflow.
--
-- /See:/ 'mkJobNodeDetails' smart constructor.
newtype JobNodeDetails = JobNodeDetails'
  { jobRuns :: Core.Maybe [Types.JobRun]
    -- ^ The information for the job runs represented by the job node.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype Core.NFData

-- | Creates a 'JobNodeDetails' value with any optional fields omitted.
mkJobNodeDetails
    :: JobNodeDetails
mkJobNodeDetails = JobNodeDetails'{jobRuns = Core.Nothing}

-- | The information for the job runs represented by the job node.
--
-- /Note:/ Consider using 'jobRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jndJobRuns :: Lens.Lens' JobNodeDetails (Core.Maybe [Types.JobRun])
jndJobRuns = Lens.field @"jobRuns"
{-# INLINEABLE jndJobRuns #-}
{-# DEPRECATED jobRuns "Use generic-lens or generic-optics with 'jobRuns' instead"  #-}

instance Core.FromJSON JobNodeDetails where
        parseJSON
          = Core.withObject "JobNodeDetails" Core.$
              \ x -> JobNodeDetails' Core.<$> (x Core..:? "JobRuns")
