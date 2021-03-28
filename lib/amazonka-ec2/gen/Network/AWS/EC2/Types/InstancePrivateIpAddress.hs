{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstancePrivateIpAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstancePrivateIpAddress
  ( InstancePrivateIpAddress (..)
  -- * Smart constructor
  , mkInstancePrivateIpAddress
  -- * Lenses
  , ipiaAssociation
  , ipiaPrimary
  , ipiaPrivateDnsName
  , ipiaPrivateIpAddress
  ) where

import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a private IPv4 address.
--
-- /See:/ 'mkInstancePrivateIpAddress' smart constructor.
data InstancePrivateIpAddress = InstancePrivateIpAddress'
  { association :: Core.Maybe Types.InstanceNetworkInterfaceAssociation
    -- ^ The association information for an Elastic IP address for the network interface.
  , primary :: Core.Maybe Core.Bool
    -- ^ Indicates whether this IPv4 address is the primary private IP address of the network interface.
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ The private IPv4 DNS name.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IPv4 address of the network interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstancePrivateIpAddress' value with any optional fields omitted.
mkInstancePrivateIpAddress
    :: InstancePrivateIpAddress
mkInstancePrivateIpAddress
  = InstancePrivateIpAddress'{association = Core.Nothing,
                              primary = Core.Nothing, privateDnsName = Core.Nothing,
                              privateIpAddress = Core.Nothing}

-- | The association information for an Elastic IP address for the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaAssociation :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe Types.InstanceNetworkInterfaceAssociation)
ipiaAssociation = Lens.field @"association"
{-# INLINEABLE ipiaAssociation #-}
{-# DEPRECATED association "Use generic-lens or generic-optics with 'association' instead"  #-}

-- | Indicates whether this IPv4 address is the primary private IP address of the network interface.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaPrimary :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe Core.Bool)
ipiaPrimary = Lens.field @"primary"
{-# INLINEABLE ipiaPrimary #-}
{-# DEPRECATED primary "Use generic-lens or generic-optics with 'primary' instead"  #-}

-- | The private IPv4 DNS name.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaPrivateDnsName :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe Core.Text)
ipiaPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE ipiaPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaPrivateIpAddress :: Lens.Lens' InstancePrivateIpAddress (Core.Maybe Core.Text)
ipiaPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE ipiaPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

instance Core.FromXML InstancePrivateIpAddress where
        parseXML x
          = InstancePrivateIpAddress' Core.<$>
              (x Core..@? "association") Core.<*> x Core..@? "primary" Core.<*>
                x Core..@? "privateDnsName"
                Core.<*> x Core..@? "privateIpAddress"
