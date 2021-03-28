{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringStoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.MonitoringStoppingCondition
  ( MonitoringStoppingCondition (..)
  -- * Smart constructor
  , mkMonitoringStoppingCondition
  -- * Lenses
  , mscMaxRuntimeInSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A time limit for how long the monitoring job is allowed to run before stopping.
--
-- /See:/ 'mkMonitoringStoppingCondition' smart constructor.
newtype MonitoringStoppingCondition = MonitoringStoppingCondition'
  { maxRuntimeInSeconds :: Core.Natural
    -- ^ The maximum runtime allowed in seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringStoppingCondition' value with any optional fields omitted.
mkMonitoringStoppingCondition
    :: Core.Natural -- ^ 'maxRuntimeInSeconds'
    -> MonitoringStoppingCondition
mkMonitoringStoppingCondition maxRuntimeInSeconds
  = MonitoringStoppingCondition'{maxRuntimeInSeconds}

-- | The maximum runtime allowed in seconds.
--
-- /Note:/ Consider using 'maxRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscMaxRuntimeInSeconds :: Lens.Lens' MonitoringStoppingCondition Core.Natural
mscMaxRuntimeInSeconds = Lens.field @"maxRuntimeInSeconds"
{-# INLINEABLE mscMaxRuntimeInSeconds #-}
{-# DEPRECATED maxRuntimeInSeconds "Use generic-lens or generic-optics with 'maxRuntimeInSeconds' instead"  #-}

instance Core.FromJSON MonitoringStoppingCondition where
        toJSON MonitoringStoppingCondition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MaxRuntimeInSeconds" Core..= maxRuntimeInSeconds)])

instance Core.FromJSON MonitoringStoppingCondition where
        parseJSON
          = Core.withObject "MonitoringStoppingCondition" Core.$
              \ x ->
                MonitoringStoppingCondition' Core.<$>
                  (x Core..: "MaxRuntimeInSeconds")
