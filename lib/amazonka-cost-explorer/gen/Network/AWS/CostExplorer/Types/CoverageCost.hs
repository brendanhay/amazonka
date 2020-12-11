-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageCost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageCost
  ( CoverageCost (..),

    -- * Smart constructor
    mkCoverageCost,

    -- * Lenses
    ccOnDemandCost,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | How much it costs to run an instance.
--
-- /See:/ 'mkCoverageCost' smart constructor.
newtype CoverageCost = CoverageCost'
  { onDemandCost ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CoverageCost' with the minimum fields required to make a request.
--
-- * 'onDemandCost' - How much an On-Demand Instance costs.
mkCoverageCost ::
  CoverageCost
mkCoverageCost = CoverageCost' {onDemandCost = Lude.Nothing}

-- | How much an On-Demand Instance costs.
--
-- /Note:/ Consider using 'onDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccOnDemandCost :: Lens.Lens' CoverageCost (Lude.Maybe Lude.Text)
ccOnDemandCost = Lens.lens (onDemandCost :: CoverageCost -> Lude.Maybe Lude.Text) (\s a -> s {onDemandCost = a} :: CoverageCost)
{-# DEPRECATED ccOnDemandCost "Use generic-lens or generic-optics with 'onDemandCost' instead." #-}

instance Lude.FromJSON CoverageCost where
  parseJSON =
    Lude.withObject
      "CoverageCost"
      (\x -> CoverageCost' Lude.<$> (x Lude..:? "OnDemandCost"))
