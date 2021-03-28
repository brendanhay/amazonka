{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
  ( NewTransitVirtualInterface (..)
  -- * Smart constructor
  , mkNewTransitVirtualInterface
  -- * Lenses
  , ntviAddressFamily
  , ntviAmazonAddress
  , ntviAsn
  , ntviAuthKey
  , ntviCustomerAddress
  , ntviDirectConnectGatewayId
  , ntviMtu
  , ntviTags
  , ntviVirtualInterfaceName
  , ntviVlan
  ) where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.AuthKey as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.DirectConnect.Types.DirectConnectGatewayId as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a transit virtual interface.
--
-- /See:/ 'mkNewTransitVirtualInterface' smart constructor.
data NewTransitVirtualInterface = NewTransitVirtualInterface'
  { addressFamily :: Core.Maybe Types.AddressFamily
    -- ^ The address family for the BGP peer.
  , amazonAddress :: Core.Maybe Types.AmazonAddress
    -- ^ The IP address assigned to the Amazon interface.
  , asn :: Core.Maybe Core.Int
    -- ^ The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
  , authKey :: Core.Maybe Types.AuthKey
    -- ^ The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
  , customerAddress :: Core.Maybe Types.CustomerAddress
    -- ^ The IP address assigned to the customer interface.
  , directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , mtu :: Core.Maybe Core.Int
    -- ^ The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags associated with the transitive virtual interface.
  , virtualInterfaceName :: Core.Maybe Types.VirtualInterfaceName
    -- ^ The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
  , vlan :: Core.Maybe Core.Int
    -- ^ The ID of the VLAN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewTransitVirtualInterface' value with any optional fields omitted.
mkNewTransitVirtualInterface
    :: NewTransitVirtualInterface
mkNewTransitVirtualInterface
  = NewTransitVirtualInterface'{addressFamily = Core.Nothing,
                                amazonAddress = Core.Nothing, asn = Core.Nothing,
                                authKey = Core.Nothing, customerAddress = Core.Nothing,
                                directConnectGatewayId = Core.Nothing, mtu = Core.Nothing,
                                tags = Core.Nothing, virtualInterfaceName = Core.Nothing,
                                vlan = Core.Nothing}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAddressFamily :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Types.AddressFamily)
ntviAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE ntviAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAmazonAddress :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Types.AmazonAddress)
ntviAmazonAddress = Lens.field @"amazonAddress"
{-# INLINEABLE ntviAmazonAddress #-}
{-# DEPRECATED amazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead"  #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAsn :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Core.Int)
ntviAsn = Lens.field @"asn"
{-# INLINEABLE ntviAsn #-}
{-# DEPRECATED asn "Use generic-lens or generic-optics with 'asn' instead"  #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAuthKey :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Types.AuthKey)
ntviAuthKey = Lens.field @"authKey"
{-# INLINEABLE ntviAuthKey #-}
{-# DEPRECATED authKey "Use generic-lens or generic-optics with 'authKey' instead"  #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviCustomerAddress :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Types.CustomerAddress)
ntviCustomerAddress = Lens.field @"customerAddress"
{-# INLINEABLE ntviCustomerAddress #-}
{-# DEPRECATED customerAddress "Use generic-lens or generic-optics with 'customerAddress' instead"  #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviDirectConnectGatewayId :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Types.DirectConnectGatewayId)
ntviDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE ntviDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviMtu :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Core.Int)
ntviMtu = Lens.field @"mtu"
{-# INLINEABLE ntviMtu #-}
{-# DEPRECATED mtu "Use generic-lens or generic-optics with 'mtu' instead"  #-}

-- | The tags associated with the transitive virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviTags :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe (Core.NonEmpty Types.Tag))
ntviTags = Lens.field @"tags"
{-# INLINEABLE ntviTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviVirtualInterfaceName :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Types.VirtualInterfaceName)
ntviVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# INLINEABLE ntviVirtualInterfaceName #-}
{-# DEPRECATED virtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead"  #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviVlan :: Lens.Lens' NewTransitVirtualInterface (Core.Maybe Core.Int)
ntviVlan = Lens.field @"vlan"
{-# INLINEABLE ntviVlan #-}
{-# DEPRECATED vlan "Use generic-lens or generic-optics with 'vlan' instead"  #-}

instance Core.FromJSON NewTransitVirtualInterface where
        toJSON NewTransitVirtualInterface{..}
          = Core.object
              (Core.catMaybes
                 [("addressFamily" Core..=) Core.<$> addressFamily,
                  ("amazonAddress" Core..=) Core.<$> amazonAddress,
                  ("asn" Core..=) Core.<$> asn, ("authKey" Core..=) Core.<$> authKey,
                  ("customerAddress" Core..=) Core.<$> customerAddress,
                  ("directConnectGatewayId" Core..=) Core.<$> directConnectGatewayId,
                  ("mtu" Core..=) Core.<$> mtu, ("tags" Core..=) Core.<$> tags,
                  ("virtualInterfaceName" Core..=) Core.<$> virtualInterfaceName,
                  ("vlan" Core..=) Core.<$> vlan])
