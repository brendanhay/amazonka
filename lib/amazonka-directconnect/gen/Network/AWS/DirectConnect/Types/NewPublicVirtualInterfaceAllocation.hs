{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPublicVirtualInterfaceAllocation
  ( NewPublicVirtualInterfaceAllocation (..),

    -- * Smart constructor
    mkNewPublicVirtualInterfaceAllocation,

    -- * Lenses
    nRouteFilterPrefixes,
    nCustomerAddress,
    nVlan,
    nAmazonAddress,
    nAddressFamily,
    nAsn,
    nAuthKey,
    nVirtualInterfaceName,
    nTags,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a public virtual interface to be provisioned on a connection.
--
-- /See:/ 'mkNewPublicVirtualInterfaceAllocation' smart constructor.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation'
  { -- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
    routeFilterPrefixes :: Lude.Maybe [RouteFilterPrefix],
    -- | The IP address assigned to the customer interface.
    customerAddress :: Lude.Maybe Lude.Text,
    -- | The ID of the VLAN.
    vlan :: Lude.Int,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Lude.Maybe Lude.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Lude.Maybe AddressFamily,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Lude.Int,
    -- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Lude.Maybe Lude.Text,
    -- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Lude.Text,
    -- | The tags associated with the public virtual interface.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewPublicVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- * 'routeFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
-- * 'customerAddress' - The IP address assigned to the customer interface.
-- * 'vlan' - The ID of the VLAN.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
-- * 'tags' - The tags associated with the public virtual interface.
mkNewPublicVirtualInterfaceAllocation ::
  -- | 'vlan'
  Lude.Int ->
  -- | 'asn'
  Lude.Int ->
  -- | 'virtualInterfaceName'
  Lude.Text ->
  NewPublicVirtualInterfaceAllocation
mkNewPublicVirtualInterfaceAllocation
  pVlan_
  pAsn_
  pVirtualInterfaceName_ =
    NewPublicVirtualInterfaceAllocation'
      { routeFilterPrefixes =
          Lude.Nothing,
        customerAddress = Lude.Nothing,
        vlan = pVlan_,
        amazonAddress = Lude.Nothing,
        addressFamily = Lude.Nothing,
        asn = pAsn_,
        authKey = Lude.Nothing,
        virtualInterfaceName = pVirtualInterfaceName_,
        tags = Lude.Nothing
      }

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- /Note:/ Consider using 'routeFilterPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nRouteFilterPrefixes :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe [RouteFilterPrefix])
nRouteFilterPrefixes = Lens.lens (routeFilterPrefixes :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {routeFilterPrefixes = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nRouteFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCustomerAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
nCustomerAddress = Lens.lens (customerAddress :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVlan :: Lens.Lens' NewPublicVirtualInterfaceAllocation Lude.Int
nVlan = Lens.lens (vlan :: NewPublicVirtualInterfaceAllocation -> Lude.Int) (\s a -> s {vlan = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAmazonAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
nAmazonAddress = Lens.lens (amazonAddress :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAddressFamily :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe AddressFamily)
nAddressFamily = Lens.lens (addressFamily :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAsn :: Lens.Lens' NewPublicVirtualInterfaceAllocation Lude.Int
nAsn = Lens.lens (asn :: NewPublicVirtualInterfaceAllocation -> Lude.Int) (\s a -> s {asn = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAuthKey :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
nAuthKey = Lens.lens (authKey :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVirtualInterfaceName :: Lens.Lens' NewPublicVirtualInterfaceAllocation Lude.Text
nVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewPublicVirtualInterfaceAllocation -> Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The tags associated with the public virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nTags :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe (Lude.NonEmpty Tag))
nTags = Lens.lens (tags :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED nTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON NewPublicVirtualInterfaceAllocation where
  toJSON NewPublicVirtualInterfaceAllocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("routeFilterPrefixes" Lude..=) Lude.<$> routeFilterPrefixes,
            ("customerAddress" Lude..=) Lude.<$> customerAddress,
            Lude.Just ("vlan" Lude..= vlan),
            ("amazonAddress" Lude..=) Lude.<$> amazonAddress,
            ("addressFamily" Lude..=) Lude.<$> addressFamily,
            Lude.Just ("asn" Lude..= asn),
            ("authKey" Lude..=) Lude.<$> authKey,
            Lude.Just ("virtualInterfaceName" Lude..= virtualInterfaceName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )
