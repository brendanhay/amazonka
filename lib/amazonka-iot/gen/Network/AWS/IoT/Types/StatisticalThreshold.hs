{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StatisticalThreshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StatisticalThreshold
  ( StatisticalThreshold (..),

    -- * Smart constructor
    mkStatisticalThreshold,

    -- * Lenses
    stStatistic,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A statistical ranking (percentile) which indicates a threshold value by which a behavior is determined to be in compliance or in violation of the behavior.
--
-- /See:/ 'mkStatisticalThreshold' smart constructor.
newtype StatisticalThreshold = StatisticalThreshold'
  { -- | The percentile which resolves to a threshold value by which compliance with a behavior is determined. Metrics are collected over the specified period (@durationSeconds@ ) from all reporting devices in your account and statistical ranks are calculated. Then, the measurements from a device are collected over the same period. If the accumulated measurements from the device fall above or below (@comparisonOperator@ ) the value associated with the percentile specified, then the device is considered to be in compliance with the behavior, otherwise a violation occurs.
    statistic :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StatisticalThreshold' with the minimum fields required to make a request.
--
-- * 'statistic' - The percentile which resolves to a threshold value by which compliance with a behavior is determined. Metrics are collected over the specified period (@durationSeconds@ ) from all reporting devices in your account and statistical ranks are calculated. Then, the measurements from a device are collected over the same period. If the accumulated measurements from the device fall above or below (@comparisonOperator@ ) the value associated with the percentile specified, then the device is considered to be in compliance with the behavior, otherwise a violation occurs.
mkStatisticalThreshold ::
  StatisticalThreshold
mkStatisticalThreshold =
  StatisticalThreshold' {statistic = Lude.Nothing}

-- | The percentile which resolves to a threshold value by which compliance with a behavior is determined. Metrics are collected over the specified period (@durationSeconds@ ) from all reporting devices in your account and statistical ranks are calculated. Then, the measurements from a device are collected over the same period. If the accumulated measurements from the device fall above or below (@comparisonOperator@ ) the value associated with the percentile specified, then the device is considered to be in compliance with the behavior, otherwise a violation occurs.
--
-- /Note:/ Consider using 'statistic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stStatistic :: Lens.Lens' StatisticalThreshold (Lude.Maybe Lude.Text)
stStatistic = Lens.lens (statistic :: StatisticalThreshold -> Lude.Maybe Lude.Text) (\s a -> s {statistic = a} :: StatisticalThreshold)
{-# DEPRECATED stStatistic "Use generic-lens or generic-optics with 'statistic' instead." #-}

instance Lude.FromJSON StatisticalThreshold where
  parseJSON =
    Lude.withObject
      "StatisticalThreshold"
      (\x -> StatisticalThreshold' Lude.<$> (x Lude..:? "statistic"))

instance Lude.ToJSON StatisticalThreshold where
  toJSON StatisticalThreshold' {..} =
    Lude.object
      (Lude.catMaybes [("statistic" Lude..=) Lude.<$> statistic])
