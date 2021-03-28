{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.NetworkInterface
  ( NetworkInterface (..)
  -- * Smart constructor
  , mkNetworkInterface
  -- * Lenses
  , niNetworkInterfaceId
  , niSubnetId
  ) where

import qualified Network.AWS.CodeBuild.Types.NetworkInterfaceId as Types
import qualified Network.AWS.CodeBuild.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The ID of the subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterface' value with any optional fields omitted.
mkNetworkInterface
    :: NetworkInterface
mkNetworkInterface
  = NetworkInterface'{networkInterfaceId = Core.Nothing,
                      subnetId = Core.Nothing}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Core.Maybe Types.NetworkInterfaceId)
niNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE niNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Core.Maybe Types.SubnetId)
niSubnetId = Lens.field @"subnetId"
{-# INLINEABLE niSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

instance Core.FromJSON NetworkInterface where
        parseJSON
          = Core.withObject "NetworkInterface" Core.$
              \ x ->
                NetworkInterface' Core.<$>
                  (x Core..:? "networkInterfaceId") Core.<*> x Core..:? "subnetId"
