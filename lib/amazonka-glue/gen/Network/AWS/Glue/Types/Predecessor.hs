{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Predecessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Predecessor
  ( Predecessor (..)
  -- * Smart constructor
  , mkPredecessor
  -- * Lenses
  , pJobName
  , pRunId
  ) where

import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.RunId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A job run that was used in the predicate of a conditional trigger that triggered this job run.
--
-- /See:/ 'mkPredecessor' smart constructor.
data Predecessor = Predecessor'
  { jobName :: Core.Maybe Types.NameString
    -- ^ The name of the job definition used by the predecessor job run.
  , runId :: Core.Maybe Types.RunId
    -- ^ The job-run ID of the predecessor job run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Predecessor' value with any optional fields omitted.
mkPredecessor
    :: Predecessor
mkPredecessor
  = Predecessor'{jobName = Core.Nothing, runId = Core.Nothing}

-- | The name of the job definition used by the predecessor job run.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pJobName :: Lens.Lens' Predecessor (Core.Maybe Types.NameString)
pJobName = Lens.field @"jobName"
{-# INLINEABLE pJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The job-run ID of the predecessor job run.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRunId :: Lens.Lens' Predecessor (Core.Maybe Types.RunId)
pRunId = Lens.field @"runId"
{-# INLINEABLE pRunId #-}
{-# DEPRECATED runId "Use generic-lens or generic-optics with 'runId' instead"  #-}

instance Core.FromJSON Predecessor where
        parseJSON
          = Core.withObject "Predecessor" Core.$
              \ x ->
                Predecessor' Core.<$>
                  (x Core..:? "JobName") Core.<*> x Core..:? "RunId"
