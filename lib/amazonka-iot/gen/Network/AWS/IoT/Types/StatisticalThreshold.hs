{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StatisticalThreshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.StatisticalThreshold
  ( StatisticalThreshold (..)
  -- * Smart constructor
  , mkStatisticalThreshold
  -- * Lenses
  , stStatistic
  ) where

import qualified Network.AWS.IoT.Types.EvaluationStatistic as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
--
-- /See:/ 'mkStatisticalThreshold' smart constructor.
newtype StatisticalThreshold = StatisticalThreshold'
  { statistic :: Core.Maybe Types.EvaluationStatistic
    -- ^ The percentile which resolves to a threshold value by which compliance with a behavior is determined. Metrics are collected over the specified period (@durationSeconds@ ) from all reporting devices in your account and statistical ranks are calculated. Then, the measurements from a device are collected over the same period. If the accumulated measurements from the device fall above or below (@comparisonOperator@ ) the value associated with the percentile specified, then the device is considered to be in compliance with the behavior, otherwise a violation occurs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StatisticalThreshold' value with any optional fields omitted.
mkStatisticalThreshold
    :: StatisticalThreshold
mkStatisticalThreshold
  = StatisticalThreshold'{statistic = Core.Nothing}

-- | The percentile which resolves to a threshold value by which compliance with a behavior is determined. Metrics are collected over the specified period (@durationSeconds@ ) from all reporting devices in your account and statistical ranks are calculated. Then, the measurements from a device are collected over the same period. If the accumulated measurements from the device fall above or below (@comparisonOperator@ ) the value associated with the percentile specified, then the device is considered to be in compliance with the behavior, otherwise a violation occurs.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStatistic :: Lens.Lens' StatisticalThreshold (Core.Maybe Types.EvaluationStatistic)
stStatistic = Lens.field @"statistic"
{-# INLINEABLE stStatistic #-}
{-# DEPRECATED statistic "Use generic-lens or generic-optics with 'statistic' instead"  #-}

instance Core.FromJSON StatisticalThreshold where
        toJSON StatisticalThreshold{..}
          = Core.object
              (Core.catMaybes [("statistic" Core..=) Core.<$> statistic])

instance Core.FromJSON StatisticalThreshold where
        parseJSON
          = Core.withObject "StatisticalThreshold" Core.$
              \ x -> StatisticalThreshold' Core.<$> (x Core..:? "statistic")
