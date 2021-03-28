{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
  ( NetworkInterfacePrivateIpAddress (..)
  -- * Smart constructor
  , mkNetworkInterfacePrivateIpAddress
  -- * Lenses
  , nipiaAssociation
  , nipiaPrimary
  , nipiaPrivateDnsName
  , nipiaPrivateIpAddress
  ) where

import qualified Network.AWS.EC2.Types.NetworkInterfaceAssociation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the private IPv4 address of a network interface.
--
-- /See:/ 'mkNetworkInterfacePrivateIpAddress' smart constructor.
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress'
  { association :: Core.Maybe Types.NetworkInterfaceAssociation
    -- ^ The association information for an Elastic IP address (IPv4) associated with the network interface.
  , primary :: Core.Maybe Core.Bool
    -- ^ Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ The private DNS name.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IPv4 address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterfacePrivateIpAddress' value with any optional fields omitted.
mkNetworkInterfacePrivateIpAddress
    :: NetworkInterfacePrivateIpAddress
mkNetworkInterfacePrivateIpAddress
  = NetworkInterfacePrivateIpAddress'{association = Core.Nothing,
                                      primary = Core.Nothing, privateDnsName = Core.Nothing,
                                      privateIpAddress = Core.Nothing}

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaAssociation :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe Types.NetworkInterfaceAssociation)
nipiaAssociation = Lens.field @"association"
{-# INLINEABLE nipiaAssociation #-}
{-# DEPRECATED association "Use generic-lens or generic-optics with 'association' instead"  #-}

-- | Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaPrimary :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe Core.Bool)
nipiaPrimary = Lens.field @"primary"
{-# INLINEABLE nipiaPrimary #-}
{-# DEPRECATED primary "Use generic-lens or generic-optics with 'primary' instead"  #-}

-- | The private DNS name.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaPrivateDnsName :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe Core.Text)
nipiaPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE nipiaPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The private IPv4 address.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaPrivateIpAddress :: Lens.Lens' NetworkInterfacePrivateIpAddress (Core.Maybe Core.Text)
nipiaPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE nipiaPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

instance Core.FromXML NetworkInterfacePrivateIpAddress where
        parseXML x
          = NetworkInterfacePrivateIpAddress' Core.<$>
              (x Core..@? "association") Core.<*> x Core..@? "primary" Core.<*>
                x Core..@? "privateDnsName"
                Core.<*> x Core..@? "privateIpAddress"
