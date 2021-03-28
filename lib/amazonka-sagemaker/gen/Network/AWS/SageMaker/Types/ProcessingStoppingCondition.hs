{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingStoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingStoppingCondition
  ( ProcessingStoppingCondition (..)
  -- * Smart constructor
  , mkProcessingStoppingCondition
  -- * Lenses
  , pscMaxRuntimeInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a time limit for how long the processing job is allowed to run.
--
-- /See:/ 'mkProcessingStoppingCondition' smart constructor.
newtype ProcessingStoppingCondition = ProcessingStoppingCondition'
  { maxRuntimeInSeconds :: Core.Natural
    -- ^ Specifies the maximum runtime in seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessingStoppingCondition' value with any optional fields omitted.
mkProcessingStoppingCondition
    :: Core.Natural -- ^ 'maxRuntimeInSeconds'
    -> ProcessingStoppingCondition
mkProcessingStoppingCondition maxRuntimeInSeconds
  = ProcessingStoppingCondition'{maxRuntimeInSeconds}

-- | Specifies the maximum runtime in seconds.
--
-- /Note:/ Consider using 'maxRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pscMaxRuntimeInSeconds :: Lens.Lens' ProcessingStoppingCondition Core.Natural
pscMaxRuntimeInSeconds = Lens.field @"maxRuntimeInSeconds"
{-# INLINEABLE pscMaxRuntimeInSeconds #-}
{-# DEPRECATED maxRuntimeInSeconds "Use generic-lens or generic-optics with 'maxRuntimeInSeconds' instead"  #-}

instance Core.FromJSON ProcessingStoppingCondition where
        toJSON ProcessingStoppingCondition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MaxRuntimeInSeconds" Core..= maxRuntimeInSeconds)])

instance Core.FromJSON ProcessingStoppingCondition where
        parseJSON
          = Core.withObject "ProcessingStoppingCondition" Core.$
              \ x ->
                ProcessingStoppingCondition' Core.<$>
                  (x Core..: "MaxRuntimeInSeconds")
