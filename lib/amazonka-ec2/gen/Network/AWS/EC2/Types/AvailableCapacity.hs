{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailableCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailableCapacity
  ( AvailableCapacity (..),

    -- * Smart constructor
    mkAvailableCapacity,

    -- * Lenses
    acAvailableInstanceCapacity,
    acAvailableVCPUs,
  )
where

import Network.AWS.EC2.Types.InstanceCapacity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The capacity information for instances that can be launched onto the Dedicated Host.
--
-- /See:/ 'mkAvailableCapacity' smart constructor.
data AvailableCapacity = AvailableCapacity'
  { -- | The number of instances that can be launched onto the Dedicated Host depending on the host's available capacity. For Dedicated Hosts that support multiple instance types, this parameter represents the number of instances for each instance size that is supported on the host.
    availableInstanceCapacity :: Lude.Maybe [InstanceCapacity],
    -- | The number of vCPUs available for launching instances onto the Dedicated Host.
    availableVCPUs :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailableCapacity' with the minimum fields required to make a request.
--
-- * 'availableInstanceCapacity' - The number of instances that can be launched onto the Dedicated Host depending on the host's available capacity. For Dedicated Hosts that support multiple instance types, this parameter represents the number of instances for each instance size that is supported on the host.
-- * 'availableVCPUs' - The number of vCPUs available for launching instances onto the Dedicated Host.
mkAvailableCapacity ::
  AvailableCapacity
mkAvailableCapacity =
  AvailableCapacity'
    { availableInstanceCapacity = Lude.Nothing,
      availableVCPUs = Lude.Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host depending on the host's available capacity. For Dedicated Hosts that support multiple instance types, this parameter represents the number of instances for each instance size that is supported on the host.
--
-- /Note:/ Consider using 'availableInstanceCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAvailableInstanceCapacity :: Lens.Lens' AvailableCapacity (Lude.Maybe [InstanceCapacity])
acAvailableInstanceCapacity = Lens.lens (availableInstanceCapacity :: AvailableCapacity -> Lude.Maybe [InstanceCapacity]) (\s a -> s {availableInstanceCapacity = a} :: AvailableCapacity)
{-# DEPRECATED acAvailableInstanceCapacity "Use generic-lens or generic-optics with 'availableInstanceCapacity' instead." #-}

-- | The number of vCPUs available for launching instances onto the Dedicated Host.
--
-- /Note:/ Consider using 'availableVCPUs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAvailableVCPUs :: Lens.Lens' AvailableCapacity (Lude.Maybe Lude.Int)
acAvailableVCPUs = Lens.lens (availableVCPUs :: AvailableCapacity -> Lude.Maybe Lude.Int) (\s a -> s {availableVCPUs = a} :: AvailableCapacity)
{-# DEPRECATED acAvailableVCPUs "Use generic-lens or generic-optics with 'availableVCPUs' instead." #-}

instance Lude.FromXML AvailableCapacity where
  parseXML x =
    AvailableCapacity'
      Lude.<$> ( x Lude..@? "availableInstanceCapacity" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "availableVCpus")
