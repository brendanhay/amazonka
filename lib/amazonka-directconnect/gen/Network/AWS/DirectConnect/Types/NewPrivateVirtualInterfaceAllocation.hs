{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
  ( NewPrivateVirtualInterfaceAllocation (..)
  -- * Smart constructor
  , mkNewPrivateVirtualInterfaceAllocation
  -- * Lenses
  , npviaVirtualInterfaceName
  , npviaVlan
  , npviaAsn
  , npviaAddressFamily
  , npviaAmazonAddress
  , npviaAuthKey
  , npviaCustomerAddress
  , npviaMtu
  , npviaTags
  ) where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.BGPAuthKey as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a private virtual interface to be provisioned on a connection.
--
-- /See:/ 'mkNewPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
  { virtualInterfaceName :: Types.VirtualInterfaceName
    -- ^ The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
  , vlan :: Core.Int
    -- ^ The ID of the VLAN.
  , asn :: Core.Int
    -- ^ The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
  , addressFamily :: Core.Maybe Types.AddressFamily
    -- ^ The address family for the BGP peer.
  , amazonAddress :: Core.Maybe Types.AmazonAddress
    -- ^ The IP address assigned to the Amazon interface.
  , authKey :: Core.Maybe Types.BGPAuthKey
    -- ^ The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
  , customerAddress :: Core.Maybe Types.CustomerAddress
    -- ^ The IP address assigned to the customer interface.
  , mtu :: Core.Maybe Core.Int
    -- ^ The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags associated with the private virtual interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewPrivateVirtualInterfaceAllocation' value with any optional fields omitted.
mkNewPrivateVirtualInterfaceAllocation
    :: Types.VirtualInterfaceName -- ^ 'virtualInterfaceName'
    -> Core.Int -- ^ 'vlan'
    -> Core.Int -- ^ 'asn'
    -> NewPrivateVirtualInterfaceAllocation
mkNewPrivateVirtualInterfaceAllocation virtualInterfaceName vlan
  asn
  = NewPrivateVirtualInterfaceAllocation'{virtualInterfaceName, vlan,
                                          asn, addressFamily = Core.Nothing,
                                          amazonAddress = Core.Nothing, authKey = Core.Nothing,
                                          customerAddress = Core.Nothing, mtu = Core.Nothing,
                                          tags = Core.Nothing}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaVirtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Types.VirtualInterfaceName
npviaVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# INLINEABLE npviaVirtualInterfaceName #-}
{-# DEPRECATED virtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead"  #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaVlan :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Core.Int
npviaVlan = Lens.field @"vlan"
{-# INLINEABLE npviaVlan #-}
{-# DEPRECATED vlan "Use generic-lens or generic-optics with 'vlan' instead"  #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAsn :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Core.Int
npviaAsn = Lens.field @"asn"
{-# INLINEABLE npviaAsn #-}
{-# DEPRECATED asn "Use generic-lens or generic-optics with 'asn' instead"  #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAddressFamily :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Types.AddressFamily)
npviaAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE npviaAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAmazonAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Types.AmazonAddress)
npviaAmazonAddress = Lens.field @"amazonAddress"
{-# INLINEABLE npviaAmazonAddress #-}
{-# DEPRECATED amazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead"  #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAuthKey :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Types.BGPAuthKey)
npviaAuthKey = Lens.field @"authKey"
{-# INLINEABLE npviaAuthKey #-}
{-# DEPRECATED authKey "Use generic-lens or generic-optics with 'authKey' instead"  #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaCustomerAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Types.CustomerAddress)
npviaCustomerAddress = Lens.field @"customerAddress"
{-# INLINEABLE npviaCustomerAddress #-}
{-# DEPRECATED customerAddress "Use generic-lens or generic-optics with 'customerAddress' instead"  #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaMtu :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe Core.Int)
npviaMtu = Lens.field @"mtu"
{-# INLINEABLE npviaMtu #-}
{-# DEPRECATED mtu "Use generic-lens or generic-optics with 'mtu' instead"  #-}

-- | The tags associated with the private virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaTags :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Core.Maybe (Core.NonEmpty Types.Tag))
npviaTags = Lens.field @"tags"
{-# INLINEABLE npviaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON NewPrivateVirtualInterfaceAllocation where
        toJSON NewPrivateVirtualInterfaceAllocation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("virtualInterfaceName" Core..= virtualInterfaceName),
                  Core.Just ("vlan" Core..= vlan), Core.Just ("asn" Core..= asn),
                  ("addressFamily" Core..=) Core.<$> addressFamily,
                  ("amazonAddress" Core..=) Core.<$> amazonAddress,
                  ("authKey" Core..=) Core.<$> authKey,
                  ("customerAddress" Core..=) Core.<$> customerAddress,
                  ("mtu" Core..=) Core.<$> mtu, ("tags" Core..=) Core.<$> tags])
