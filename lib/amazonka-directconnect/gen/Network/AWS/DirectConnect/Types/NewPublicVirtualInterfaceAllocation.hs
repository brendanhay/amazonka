{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
  ( NewPublicVirtualInterfaceAllocation (..)
  -- * Smart constructor
  , mkNewPublicVirtualInterfaceAllocation
  -- * Lenses
  , npviafVirtualInterfaceName
  , npviafVlan
  , npviafAsn
  , npviafAddressFamily
  , npviafAmazonAddress
  , npviafAuthKey
  , npviafCustomerAddress
  , npviafRouteFilterPrefixes
  , npviafTags
  ) where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.BGPAuthKey as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.DirectConnect.Types.RouteFilterPrefix as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a public virtual interface to be provisioned on a connection.
--
-- /See:/ 'mkNewPublicVirtualInterfaceAllocation' smart constructor.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
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
  , routeFilterPrefixes :: Core.Maybe [Types.RouteFilterPrefix]
    -- ^ The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags associated with the public virtual interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewPublicVirtualInterfaceAllocation' value with any optional fields omitted.
mkNewPublicVirtualInterfaceAllocation
    :: Types.VirtualInterfaceName -- ^ 'virtualInterfaceName'
    -> Core.Int -- ^ 'vlan'
    -> Core.Int -- ^ 'asn'
    -> NewPublicVirtualInterfaceAllocation
mkNewPublicVirtualInterfaceAllocation virtualInterfaceName vlan asn
  = NewPublicVirtualInterfaceAllocation'{virtualInterfaceName, vlan,
                                         asn, addressFamily = Core.Nothing,
                                         amazonAddress = Core.Nothing, authKey = Core.Nothing,
                                         customerAddress = Core.Nothing,
                                         routeFilterPrefixes = Core.Nothing, tags = Core.Nothing}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafVirtualInterfaceName :: Lens.Lens' NewPublicVirtualInterfaceAllocation Types.VirtualInterfaceName
npviafVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# INLINEABLE npviafVirtualInterfaceName #-}
{-# DEPRECATED virtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead"  #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafVlan :: Lens.Lens' NewPublicVirtualInterfaceAllocation Core.Int
npviafVlan = Lens.field @"vlan"
{-# INLINEABLE npviafVlan #-}
{-# DEPRECATED vlan "Use generic-lens or generic-optics with 'vlan' instead"  #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafAsn :: Lens.Lens' NewPublicVirtualInterfaceAllocation Core.Int
npviafAsn = Lens.field @"asn"
{-# INLINEABLE npviafAsn #-}
{-# DEPRECATED asn "Use generic-lens or generic-optics with 'asn' instead"  #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafAddressFamily :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Core.Maybe Types.AddressFamily)
npviafAddressFamily = Lens.field @"addressFamily"
{-# INLINEABLE npviafAddressFamily #-}
{-# DEPRECATED addressFamily "Use generic-lens or generic-optics with 'addressFamily' instead"  #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafAmazonAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Core.Maybe Types.AmazonAddress)
npviafAmazonAddress = Lens.field @"amazonAddress"
{-# INLINEABLE npviafAmazonAddress #-}
{-# DEPRECATED amazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead"  #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafAuthKey :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Core.Maybe Types.BGPAuthKey)
npviafAuthKey = Lens.field @"authKey"
{-# INLINEABLE npviafAuthKey #-}
{-# DEPRECATED authKey "Use generic-lens or generic-optics with 'authKey' instead"  #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafCustomerAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Core.Maybe Types.CustomerAddress)
npviafCustomerAddress = Lens.field @"customerAddress"
{-# INLINEABLE npviafCustomerAddress #-}
{-# DEPRECATED customerAddress "Use generic-lens or generic-optics with 'customerAddress' instead"  #-}

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- /Note:/ Consider using 'routeFilterPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafRouteFilterPrefixes :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Core.Maybe [Types.RouteFilterPrefix])
npviafRouteFilterPrefixes = Lens.field @"routeFilterPrefixes"
{-# INLINEABLE npviafRouteFilterPrefixes #-}
{-# DEPRECATED routeFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead"  #-}

-- | The tags associated with the public virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviafTags :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Core.Maybe (Core.NonEmpty Types.Tag))
npviafTags = Lens.field @"tags"
{-# INLINEABLE npviafTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON NewPublicVirtualInterfaceAllocation where
        toJSON NewPublicVirtualInterfaceAllocation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("virtualInterfaceName" Core..= virtualInterfaceName),
                  Core.Just ("vlan" Core..= vlan), Core.Just ("asn" Core..= asn),
                  ("addressFamily" Core..=) Core.<$> addressFamily,
                  ("amazonAddress" Core..=) Core.<$> amazonAddress,
                  ("authKey" Core..=) Core.<$> authKey,
                  ("customerAddress" Core..=) Core.<$> customerAddress,
                  ("routeFilterPrefixes" Core..=) Core.<$> routeFilterPrefixes,
                  ("tags" Core..=) Core.<$> tags])
