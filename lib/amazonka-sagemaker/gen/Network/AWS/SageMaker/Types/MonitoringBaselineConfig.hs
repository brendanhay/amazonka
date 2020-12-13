{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringBaselineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringBaselineConfig
  ( MonitoringBaselineConfig (..),

    -- * Smart constructor
    mkMonitoringBaselineConfig,

    -- * Lenses
    mbcConstraintsResource,
    mbcStatisticsResource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource
import Network.AWS.SageMaker.Types.MonitoringStatisticsResource

-- | Configuration for monitoring constraints and monitoring statistics. These baseline resources are compared against the results of the current job from the series of jobs scheduled to collect data periodically.
--
-- /See:/ 'mkMonitoringBaselineConfig' smart constructor.
data MonitoringBaselineConfig = MonitoringBaselineConfig'
  { -- | The baseline constraint file in Amazon S3 that the current monitoring job should validated against.
    constraintsResource :: Lude.Maybe MonitoringConstraintsResource,
    -- | The baseline statistics file in Amazon S3 that the current monitoring job should be validated against.
    statisticsResource :: Lude.Maybe MonitoringStatisticsResource
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringBaselineConfig' with the minimum fields required to make a request.
--
-- * 'constraintsResource' - The baseline constraint file in Amazon S3 that the current monitoring job should validated against.
-- * 'statisticsResource' - The baseline statistics file in Amazon S3 that the current monitoring job should be validated against.
mkMonitoringBaselineConfig ::
  MonitoringBaselineConfig
mkMonitoringBaselineConfig =
  MonitoringBaselineConfig'
    { constraintsResource = Lude.Nothing,
      statisticsResource = Lude.Nothing
    }

-- | The baseline constraint file in Amazon S3 that the current monitoring job should validated against.
--
-- /Note:/ Consider using 'constraintsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbcConstraintsResource :: Lens.Lens' MonitoringBaselineConfig (Lude.Maybe MonitoringConstraintsResource)
mbcConstraintsResource = Lens.lens (constraintsResource :: MonitoringBaselineConfig -> Lude.Maybe MonitoringConstraintsResource) (\s a -> s {constraintsResource = a} :: MonitoringBaselineConfig)
{-# DEPRECATED mbcConstraintsResource "Use generic-lens or generic-optics with 'constraintsResource' instead." #-}

-- | The baseline statistics file in Amazon S3 that the current monitoring job should be validated against.
--
-- /Note:/ Consider using 'statisticsResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbcStatisticsResource :: Lens.Lens' MonitoringBaselineConfig (Lude.Maybe MonitoringStatisticsResource)
mbcStatisticsResource = Lens.lens (statisticsResource :: MonitoringBaselineConfig -> Lude.Maybe MonitoringStatisticsResource) (\s a -> s {statisticsResource = a} :: MonitoringBaselineConfig)
{-# DEPRECATED mbcStatisticsResource "Use generic-lens or generic-optics with 'statisticsResource' instead." #-}

instance Lude.FromJSON MonitoringBaselineConfig where
  parseJSON =
    Lude.withObject
      "MonitoringBaselineConfig"
      ( \x ->
          MonitoringBaselineConfig'
            Lude.<$> (x Lude..:? "ConstraintsResource")
            Lude.<*> (x Lude..:? "StatisticsResource")
      )

instance Lude.ToJSON MonitoringBaselineConfig where
  toJSON MonitoringBaselineConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConstraintsResource" Lude..=) Lude.<$> constraintsResource,
            ("StatisticsResource" Lude..=) Lude.<$> statisticsResource
          ]
      )
