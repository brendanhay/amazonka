{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterface
  ( NewPrivateVirtualInterface (..),

    -- * Smart constructor
    mkNewPrivateVirtualInterface,

    -- * Lenses
    nVirtualInterfaceName,
    nVlan,
    nAsn,
    nAddressFamily,
    nAmazonAddress,
    nAuthKey,
    nCustomerAddress,
    nDirectConnectGatewayId,
    nMtu,
    nTags,
    nVirtualGatewayId,
  )
where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.BGPAuthKey as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.DirectConnect.Types.VirtualGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a private virtual interface.
--
-- /See:/ 'mkNewPrivateVirtualInterface' smart constructor.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
  { -- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Types.VirtualInterfaceName,
    -- | The ID of the VLAN.
    vlan :: Core.Int,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Core.Int,
    -- | The address family for the BGP peer.
    addressFamily :: Core.Maybe Types.AddressFamily,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Core.Maybe Types.AmazonAddress,
    -- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Core.Maybe Types.BGPAuthKey,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Core.Maybe Types.CustomerAddress,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int,
    -- | The tags associated with the private virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag),
    -- | The ID of the virtual private gateway.
    virtualGatewayId :: Core.Maybe Types.VirtualGatewayId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewPrivateVirtualInterface' value with any optional fields omitted.
mkNewPrivateVirtualInterface ::
  -- | 'virtualInterfaceName'
  Types.VirtualInterfaceName ->
  -- | 'vlan'
  Core.Int ->
  -- | 'asn'
  Core.Int ->
  NewPrivateVirtualInterface
mkNewPrivateVirtualInterface virtualInterfaceName vlan asn =
  NewPrivateVirtualInterface'
    { virtualInterfaceName,
      vlan,
      asn,
      addressFamily = Core.Nothing,
      amazonAddress = Core.Nothing,
      authKey = Core.Nothing,
      customerAddress = Core.Nothing,
      directConnectGatewayId = Core.Nothing,
      mtu = Core.Nothing,
      tags = Core.Nothing,
      virtualGatewayId = Core.Nothing
    }

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVirtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterface Types.VirtualInterfaceName
nVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# DEPRECATED nVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVlan :: Lens.Lens' NewPrivateVirtualInterface Core.Int
nVlan = Lens.field @"vlan"
{-# DEPRECATED nVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAsn :: Lens.Lens' NewPrivateVirtualInterface Core.Int
nAsn = Lens.field @"asn"
{-# DEPRECATED nAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAddressFamily :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Types.AddressFamily)
nAddressFamily = Lens.field @"addressFamily"
{-# DEPRECATED nAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAmazonAddress :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Types.AmazonAddress)
nAmazonAddress = Lens.field @"amazonAddress"
{-# DEPRECATED nAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAuthKey :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Types.BGPAuthKey)
nAuthKey = Lens.field @"authKey"
{-# DEPRECATED nAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCustomerAddress :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Types.CustomerAddress)
nCustomerAddress = Lens.field @"customerAddress"
{-# DEPRECATED nCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nDirectConnectGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Types.DirectConnectGatewayId)
nDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# DEPRECATED nDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nMtu :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Core.Int)
nMtu = Lens.field @"mtu"
{-# DEPRECATED nMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The tags associated with the private virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nTags :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe (Core.NonEmpty Types.Tag))
nTags = Lens.field @"tags"
{-# DEPRECATED nTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVirtualGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Core.Maybe Types.VirtualGatewayId)
nVirtualGatewayId = Lens.field @"virtualGatewayId"
{-# DEPRECATED nVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

instance Core.FromJSON NewPrivateVirtualInterface where
  toJSON NewPrivateVirtualInterface {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("virtualInterfaceName" Core..= virtualInterfaceName),
            Core.Just ("vlan" Core..= vlan),
            Core.Just ("asn" Core..= asn),
            ("addressFamily" Core..=) Core.<$> addressFamily,
            ("amazonAddress" Core..=) Core.<$> amazonAddress,
            ("authKey" Core..=) Core.<$> authKey,
            ("customerAddress" Core..=) Core.<$> customerAddress,
            ("directConnectGatewayId" Core..=) Core.<$> directConnectGatewayId,
            ("mtu" Core..=) Core.<$> mtu,
            ("tags" Core..=) Core.<$> tags,
            ("virtualGatewayId" Core..=) Core.<$> virtualGatewayId
          ]
      )
