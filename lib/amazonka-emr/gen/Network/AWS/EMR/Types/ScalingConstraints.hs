{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingConstraints
  ( ScalingConstraints (..),

    -- * Smart constructor
    mkScalingConstraints,

    -- * Lenses
    scMinCapacity,
    scMaxCapacity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activities triggered by automatic scaling rules will not cause an instance group to grow above or below these limits.
--
-- /See:/ 'mkScalingConstraints' smart constructor.
data ScalingConstraints = ScalingConstraints'
  { minCapacity ::
      Lude.Int,
    maxCapacity :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScalingConstraints' with the minimum fields required to make a request.
--
-- * 'maxCapacity' - The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
-- * 'minCapacity' - The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
mkScalingConstraints ::
  -- | 'minCapacity'
  Lude.Int ->
  -- | 'maxCapacity'
  Lude.Int ->
  ScalingConstraints
mkScalingConstraints pMinCapacity_ pMaxCapacity_ =
  ScalingConstraints'
    { minCapacity = pMinCapacity_,
      maxCapacity = pMaxCapacity_
    }

-- | The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
--
-- /Note:/ Consider using 'minCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMinCapacity :: Lens.Lens' ScalingConstraints Lude.Int
scMinCapacity = Lens.lens (minCapacity :: ScalingConstraints -> Lude.Int) (\s a -> s {minCapacity = a} :: ScalingConstraints)
{-# DEPRECATED scMinCapacity "Use generic-lens or generic-optics with 'minCapacity' instead." #-}

-- | The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
--
-- /Note:/ Consider using 'maxCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMaxCapacity :: Lens.Lens' ScalingConstraints Lude.Int
scMaxCapacity = Lens.lens (maxCapacity :: ScalingConstraints -> Lude.Int) (\s a -> s {maxCapacity = a} :: ScalingConstraints)
{-# DEPRECATED scMaxCapacity "Use generic-lens or generic-optics with 'maxCapacity' instead." #-}

instance Lude.FromJSON ScalingConstraints where
  parseJSON =
    Lude.withObject
      "ScalingConstraints"
      ( \x ->
          ScalingConstraints'
            Lude.<$> (x Lude..: "MinCapacity") Lude.<*> (x Lude..: "MaxCapacity")
      )

instance Lude.ToJSON ScalingConstraints where
  toJSON ScalingConstraints' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MinCapacity" Lude..= minCapacity),
            Lude.Just ("MaxCapacity" Lude..= maxCapacity)
          ]
      )
