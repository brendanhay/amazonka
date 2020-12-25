{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPublicVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPublicVirtualInterface
  ( NewPublicVirtualInterface (..),

    -- * Smart constructor
    mkNewPublicVirtualInterface,

    -- * Lenses
    npviVirtualInterfaceName,
    npviVlan,
    npviAsn,
    npviAddressFamily,
    npviAmazonAddress,
    npviAuthKey,
    npviCustomerAddress,
    npviRouteFilterPrefixes,
    npviTags,
  )
where

import qualified Network.AWS.DirectConnect.Types.AddressFamily as Types
import qualified Network.AWS.DirectConnect.Types.AmazonAddress as Types
import qualified Network.AWS.DirectConnect.Types.BGPAuthKey as Types
import qualified Network.AWS.DirectConnect.Types.CustomerAddress as Types
import qualified Network.AWS.DirectConnect.Types.RouteFilterPrefix as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a public virtual interface.
--
-- /See:/ 'mkNewPublicVirtualInterface' smart constructor.
data NewPublicVirtualInterface = NewPublicVirtualInterface'
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
    -- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
    routeFilterPrefixes :: Core.Maybe [Types.RouteFilterPrefix],
    -- | The tags associated with the public virtual interface.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NewPublicVirtualInterface' value with any optional fields omitted.
mkNewPublicVirtualInterface ::
  -- | 'virtualInterfaceName'
  Types.VirtualInterfaceName ->
  -- | 'vlan'
  Core.Int ->
  -- | 'asn'
  Core.Int ->
  NewPublicVirtualInterface
mkNewPublicVirtualInterface virtualInterfaceName vlan asn =
  NewPublicVirtualInterface'
    { virtualInterfaceName,
      vlan,
      asn,
      addressFamily = Core.Nothing,
      amazonAddress = Core.Nothing,
      authKey = Core.Nothing,
      customerAddress = Core.Nothing,
      routeFilterPrefixes = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviVirtualInterfaceName :: Lens.Lens' NewPublicVirtualInterface Types.VirtualInterfaceName
npviVirtualInterfaceName = Lens.field @"virtualInterfaceName"
{-# DEPRECATED npviVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviVlan :: Lens.Lens' NewPublicVirtualInterface Core.Int
npviVlan = Lens.field @"vlan"
{-# DEPRECATED npviVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAsn :: Lens.Lens' NewPublicVirtualInterface Core.Int
npviAsn = Lens.field @"asn"
{-# DEPRECATED npviAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAddressFamily :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe Types.AddressFamily)
npviAddressFamily = Lens.field @"addressFamily"
{-# DEPRECATED npviAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAmazonAddress :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe Types.AmazonAddress)
npviAmazonAddress = Lens.field @"amazonAddress"
{-# DEPRECATED npviAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAuthKey :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe Types.BGPAuthKey)
npviAuthKey = Lens.field @"authKey"
{-# DEPRECATED npviAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviCustomerAddress :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe Types.CustomerAddress)
npviCustomerAddress = Lens.field @"customerAddress"
{-# DEPRECATED npviCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- /Note:/ Consider using 'routeFilterPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviRouteFilterPrefixes :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe [Types.RouteFilterPrefix])
npviRouteFilterPrefixes = Lens.field @"routeFilterPrefixes"
{-# DEPRECATED npviRouteFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead." #-}

-- | The tags associated with the public virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviTags :: Lens.Lens' NewPublicVirtualInterface (Core.Maybe (Core.NonEmpty Types.Tag))
npviTags = Lens.field @"tags"
{-# DEPRECATED npviTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON NewPublicVirtualInterface where
  toJSON NewPublicVirtualInterface {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("virtualInterfaceName" Core..= virtualInterfaceName),
            Core.Just ("vlan" Core..= vlan),
            Core.Just ("asn" Core..= asn),
            ("addressFamily" Core..=) Core.<$> addressFamily,
            ("amazonAddress" Core..=) Core.<$> amazonAddress,
            ("authKey" Core..=) Core.<$> authKey,
            ("customerAddress" Core..=) Core.<$> customerAddress,
            ("routeFilterPrefixes" Core..=) Core.<$> routeFilterPrefixes,
            ("tags" Core..=) Core.<$> tags
          ]
      )
