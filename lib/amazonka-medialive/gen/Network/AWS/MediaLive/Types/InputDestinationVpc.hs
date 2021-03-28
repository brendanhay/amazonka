{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDestinationVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputDestinationVpc
  ( InputDestinationVpc (..)
  -- * Smart constructor
  , mkInputDestinationVpc
  -- * Lenses
  , idvAvailabilityZone
  , idvNetworkInterfaceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The properties for a VPC type input destination.
--
-- /See:/ 'mkInputDestinationVpc' smart constructor.
data InputDestinationVpc = InputDestinationVpc'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The availability zone of the Input destination.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The network interface ID of the Input destination in the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputDestinationVpc' value with any optional fields omitted.
mkInputDestinationVpc
    :: InputDestinationVpc
mkInputDestinationVpc
  = InputDestinationVpc'{availabilityZone = Core.Nothing,
                         networkInterfaceId = Core.Nothing}

-- | The availability zone of the Input destination.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvAvailabilityZone :: Lens.Lens' InputDestinationVpc (Core.Maybe Core.Text)
idvAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE idvAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The network interface ID of the Input destination in the VPC.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvNetworkInterfaceId :: Lens.Lens' InputDestinationVpc (Core.Maybe Core.Text)
idvNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE idvNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

instance Core.FromJSON InputDestinationVpc where
        parseJSON
          = Core.withObject "InputDestinationVpc" Core.$
              \ x ->
                InputDestinationVpc' Core.<$>
                  (x Core..:? "availabilityZone") Core.<*>
                    x Core..:? "networkInterfaceId"
