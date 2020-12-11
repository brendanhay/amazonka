-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringStoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringStoppingCondition
  ( MonitoringStoppingCondition (..),

    -- * Smart constructor
    mkMonitoringStoppingCondition,

    -- * Lenses
    mscMaxRuntimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A time limit for how long the monitoring job is allowed to run before stopping.
--
-- /See:/ 'mkMonitoringStoppingCondition' smart constructor.
newtype MonitoringStoppingCondition = MonitoringStoppingCondition'
  { maxRuntimeInSeconds ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringStoppingCondition' with the minimum fields required to make a request.
--
-- * 'maxRuntimeInSeconds' - The maximum runtime allowed in seconds.
mkMonitoringStoppingCondition ::
  -- | 'maxRuntimeInSeconds'
  Lude.Natural ->
  MonitoringStoppingCondition
mkMonitoringStoppingCondition pMaxRuntimeInSeconds_ =
  MonitoringStoppingCondition'
    { maxRuntimeInSeconds =
        pMaxRuntimeInSeconds_
    }

-- | The maximum runtime allowed in seconds.
--
-- /Note:/ Consider using 'maxRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscMaxRuntimeInSeconds :: Lens.Lens' MonitoringStoppingCondition Lude.Natural
mscMaxRuntimeInSeconds = Lens.lens (maxRuntimeInSeconds :: MonitoringStoppingCondition -> Lude.Natural) (\s a -> s {maxRuntimeInSeconds = a} :: MonitoringStoppingCondition)
{-# DEPRECATED mscMaxRuntimeInSeconds "Use generic-lens or generic-optics with 'maxRuntimeInSeconds' instead." #-}

instance Lude.FromJSON MonitoringStoppingCondition where
  parseJSON =
    Lude.withObject
      "MonitoringStoppingCondition"
      ( \x ->
          MonitoringStoppingCondition'
            Lude.<$> (x Lude..: "MaxRuntimeInSeconds")
      )

instance Lude.ToJSON MonitoringStoppingCondition where
  toJSON MonitoringStoppingCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("MaxRuntimeInSeconds" Lude..= maxRuntimeInSeconds)]
      )
