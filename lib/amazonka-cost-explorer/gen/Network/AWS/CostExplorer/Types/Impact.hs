{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Impact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Impact
  ( Impact (..),

    -- * Smart constructor
    mkImpact,

    -- * Lenses
    iTotalImpact,
    iMaxImpact,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The anomaly's dollar value.
--
-- /See:/ 'mkImpact' smart constructor.
data Impact = Impact'
  { -- | The cumulative dollar value observed for an anomaly.
    totalImpact :: Lude.Maybe Lude.Double,
    -- | The maximum dollar value observed for an anomaly.
    maxImpact :: Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Impact' with the minimum fields required to make a request.
--
-- * 'totalImpact' - The cumulative dollar value observed for an anomaly.
-- * 'maxImpact' - The maximum dollar value observed for an anomaly.
mkImpact ::
  -- | 'maxImpact'
  Lude.Double ->
  Impact
mkImpact pMaxImpact_ =
  Impact' {totalImpact = Lude.Nothing, maxImpact = pMaxImpact_}

-- | The cumulative dollar value observed for an anomaly.
--
-- /Note:/ Consider using 'totalImpact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTotalImpact :: Lens.Lens' Impact (Lude.Maybe Lude.Double)
iTotalImpact = Lens.lens (totalImpact :: Impact -> Lude.Maybe Lude.Double) (\s a -> s {totalImpact = a} :: Impact)
{-# DEPRECATED iTotalImpact "Use generic-lens or generic-optics with 'totalImpact' instead." #-}

-- | The maximum dollar value observed for an anomaly.
--
-- /Note:/ Consider using 'maxImpact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMaxImpact :: Lens.Lens' Impact Lude.Double
iMaxImpact = Lens.lens (maxImpact :: Impact -> Lude.Double) (\s a -> s {maxImpact = a} :: Impact)
{-# DEPRECATED iMaxImpact "Use generic-lens or generic-optics with 'maxImpact' instead." #-}

instance Lude.FromJSON Impact where
  parseJSON =
    Lude.withObject
      "Impact"
      ( \x ->
          Impact'
            Lude.<$> (x Lude..:? "TotalImpact") Lude.<*> (x Lude..: "MaxImpact")
      )
