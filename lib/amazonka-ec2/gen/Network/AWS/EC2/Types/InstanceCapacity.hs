-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCapacity
  ( InstanceCapacity (..),

    -- * Smart constructor
    mkInstanceCapacity,

    -- * Lenses
    icAvailableCapacity,
    icInstanceType,
    icTotalCapacity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the number of instances that can be launched onto the Dedicated Host.
--
-- /See:/ 'mkInstanceCapacity' smart constructor.
data InstanceCapacity = InstanceCapacity'
  { availableCapacity ::
      Lude.Maybe Lude.Int,
    instanceType :: Lude.Maybe Lude.Text,
    totalCapacity :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceCapacity' with the minimum fields required to make a request.
--
-- * 'availableCapacity' - The number of instances that can be launched onto the Dedicated Host based on the host's available capacity.
-- * 'instanceType' - The instance type supported by the Dedicated Host.
-- * 'totalCapacity' - The total number of instances that can be launched onto the Dedicated Host if there are no instances running on it.
mkInstanceCapacity ::
  InstanceCapacity
mkInstanceCapacity =
  InstanceCapacity'
    { availableCapacity = Lude.Nothing,
      instanceType = Lude.Nothing,
      totalCapacity = Lude.Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host based on the host's available capacity.
--
-- /Note:/ Consider using 'availableCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icAvailableCapacity :: Lens.Lens' InstanceCapacity (Lude.Maybe Lude.Int)
icAvailableCapacity = Lens.lens (availableCapacity :: InstanceCapacity -> Lude.Maybe Lude.Int) (\s a -> s {availableCapacity = a} :: InstanceCapacity)
{-# DEPRECATED icAvailableCapacity "Use generic-lens or generic-optics with 'availableCapacity' instead." #-}

-- | The instance type supported by the Dedicated Host.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icInstanceType :: Lens.Lens' InstanceCapacity (Lude.Maybe Lude.Text)
icInstanceType = Lens.lens (instanceType :: InstanceCapacity -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: InstanceCapacity)
{-# DEPRECATED icInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The total number of instances that can be launched onto the Dedicated Host if there are no instances running on it.
--
-- /Note:/ Consider using 'totalCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTotalCapacity :: Lens.Lens' InstanceCapacity (Lude.Maybe Lude.Int)
icTotalCapacity = Lens.lens (totalCapacity :: InstanceCapacity -> Lude.Maybe Lude.Int) (\s a -> s {totalCapacity = a} :: InstanceCapacity)
{-# DEPRECATED icTotalCapacity "Use generic-lens or generic-optics with 'totalCapacity' instead." #-}

instance Lude.FromXML InstanceCapacity where
  parseXML x =
    InstanceCapacity'
      Lude.<$> (x Lude..@? "availableCapacity")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "totalCapacity")
