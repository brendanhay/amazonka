{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MetricValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MetricValue
  ( MetricValue (..),

    -- * Smart constructor
    mkMetricValue,

    -- * Lenses
    mvAmount,
    mvUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The aggregated value for a metric.
--
-- /See:/ 'mkMetricValue' smart constructor.
data MetricValue = MetricValue'
  { -- | The actual number that represents the metric.
    amount :: Lude.Maybe Lude.Text,
    -- | The unit that the metric is given in.
    unit :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricValue' with the minimum fields required to make a request.
--
-- * 'amount' - The actual number that represents the metric.
-- * 'unit' - The unit that the metric is given in.
mkMetricValue ::
  MetricValue
mkMetricValue =
  MetricValue' {amount = Lude.Nothing, unit = Lude.Nothing}

-- | The actual number that represents the metric.
--
-- /Note:/ Consider using 'amount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvAmount :: Lens.Lens' MetricValue (Lude.Maybe Lude.Text)
mvAmount = Lens.lens (amount :: MetricValue -> Lude.Maybe Lude.Text) (\s a -> s {amount = a} :: MetricValue)
{-# DEPRECATED mvAmount "Use generic-lens or generic-optics with 'amount' instead." #-}

-- | The unit that the metric is given in.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvUnit :: Lens.Lens' MetricValue (Lude.Maybe Lude.Text)
mvUnit = Lens.lens (unit :: MetricValue -> Lude.Maybe Lude.Text) (\s a -> s {unit = a} :: MetricValue)
{-# DEPRECATED mvUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON MetricValue where
  parseJSON =
    Lude.withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            Lude.<$> (x Lude..:? "Amount") Lude.<*> (x Lude..:? "Unit")
      )
