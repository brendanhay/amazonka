-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetric
  ( CurrentMetric (..),

    -- * Smart constructor
    mkCurrentMetric,

    -- * Lenses
    cmName,
    cmUnit,
  )
where

import Network.AWS.Connect.Types.CurrentMetricName
import Network.AWS.Connect.Types.Unit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a real-time metric. For a description of each metric, see <https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html Real-time Metrics Definitions> in the /Amazon Connect Administrator Guide/ .
--
-- /See:/ 'mkCurrentMetric' smart constructor.
data CurrentMetric = CurrentMetric'
  { name ::
      Lude.Maybe CurrentMetricName,
    unit :: Lude.Maybe Unit
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CurrentMetric' with the minimum fields required to make a request.
--
-- * 'name' - The name of the metric.
-- * 'unit' - The unit for the metric.
mkCurrentMetric ::
  CurrentMetric
mkCurrentMetric =
  CurrentMetric' {name = Lude.Nothing, unit = Lude.Nothing}

-- | The name of the metric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' CurrentMetric (Lude.Maybe CurrentMetricName)
cmName = Lens.lens (name :: CurrentMetric -> Lude.Maybe CurrentMetricName) (\s a -> s {name = a} :: CurrentMetric)
{-# DEPRECATED cmName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unit for the metric.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmUnit :: Lens.Lens' CurrentMetric (Lude.Maybe Unit)
cmUnit = Lens.lens (unit :: CurrentMetric -> Lude.Maybe Unit) (\s a -> s {unit = a} :: CurrentMetric)
{-# DEPRECATED cmUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON CurrentMetric where
  parseJSON =
    Lude.withObject
      "CurrentMetric"
      ( \x ->
          CurrentMetric'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Unit")
      )

instance Lude.ToJSON CurrentMetric where
  toJSON CurrentMetric' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Name" Lude..=) Lude.<$> name, ("Unit" Lude..=) Lude.<$> unit]
      )
