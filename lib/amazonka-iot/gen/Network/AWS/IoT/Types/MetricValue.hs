{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricValue
  ( MetricValue (..),

    -- * Smart constructor
    mkMetricValue,

    -- * Lenses
    mvCidrs,
    mvCount,
    mvPorts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The value to be compared with the @metric@ .
--
-- /See:/ 'mkMetricValue' smart constructor.
data MetricValue = MetricValue'
  { cidrs :: Lude.Maybe [Lude.Text],
    count :: Lude.Maybe Lude.Natural,
    ports :: Lude.Maybe [Lude.Natural]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricValue' with the minimum fields required to make a request.
--
-- * 'cidrs' - If the @comparisonOperator@ calls for a set of CIDRs, use this to specify that set to be compared with the @metric@ .
-- * 'count' - If the @comparisonOperator@ calls for a numeric value, use this to specify that numeric value to be compared with the @metric@ .
-- * 'ports' - If the @comparisonOperator@ calls for a set of ports, use this to specify that set to be compared with the @metric@ .
mkMetricValue ::
  MetricValue
mkMetricValue =
  MetricValue'
    { cidrs = Lude.Nothing,
      count = Lude.Nothing,
      ports = Lude.Nothing
    }

-- | If the @comparisonOperator@ calls for a set of CIDRs, use this to specify that set to be compared with the @metric@ .
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvCidrs :: Lens.Lens' MetricValue (Lude.Maybe [Lude.Text])
mvCidrs = Lens.lens (cidrs :: MetricValue -> Lude.Maybe [Lude.Text]) (\s a -> s {cidrs = a} :: MetricValue)
{-# DEPRECATED mvCidrs "Use generic-lens or generic-optics with 'cidrs' instead." #-}

-- | If the @comparisonOperator@ calls for a numeric value, use this to specify that numeric value to be compared with the @metric@ .
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvCount :: Lens.Lens' MetricValue (Lude.Maybe Lude.Natural)
mvCount = Lens.lens (count :: MetricValue -> Lude.Maybe Lude.Natural) (\s a -> s {count = a} :: MetricValue)
{-# DEPRECATED mvCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | If the @comparisonOperator@ calls for a set of ports, use this to specify that set to be compared with the @metric@ .
--
-- /Note:/ Consider using 'ports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mvPorts :: Lens.Lens' MetricValue (Lude.Maybe [Lude.Natural])
mvPorts = Lens.lens (ports :: MetricValue -> Lude.Maybe [Lude.Natural]) (\s a -> s {ports = a} :: MetricValue)
{-# DEPRECATED mvPorts "Use generic-lens or generic-optics with 'ports' instead." #-}

instance Lude.FromJSON MetricValue where
  parseJSON =
    Lude.withObject
      "MetricValue"
      ( \x ->
          MetricValue'
            Lude.<$> (x Lude..:? "cidrs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "count")
            Lude.<*> (x Lude..:? "ports" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON MetricValue where
  toJSON MetricValue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cidrs" Lude..=) Lude.<$> cidrs,
            ("count" Lude..=) Lude.<$> count,
            ("ports" Lude..=) Lude.<$> ports
          ]
      )
