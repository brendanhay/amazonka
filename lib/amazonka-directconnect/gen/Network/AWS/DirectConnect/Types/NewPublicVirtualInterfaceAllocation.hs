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
    newRouteFilterPrefixes,
    newCustomerAddress,
    newAmazonAddress,
    newAddressFamily,
    newAuthKey,
    newTags,
    newVirtualInterfaceName,
    newVlan,
    newAsn,
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
  { routeFilterPrefixes ::
      Lude.Maybe
        [RouteFilterPrefix],
    customerAddress ::
      Lude.Maybe
        Lude.Text,
    amazonAddress ::
      Lude.Maybe
        Lude.Text,
    addressFamily ::
      Lude.Maybe
        AddressFamily,
    authKey ::
      Lude.Maybe
        Lude.Text,
    tags ::
      Lude.Maybe
        ( Lude.NonEmpty
            Tag
        ),
    virtualInterfaceName ::
      Lude.Text,
    vlan :: Lude.Int,
    asn :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewPublicVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'customerAddress' - The IP address assigned to the customer interface.
-- * 'routeFilterPrefixes' - The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
-- * 'tags' - The tags associated with the public virtual interface.
-- * 'virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
-- * 'vlan' - The ID of the VLAN.
mkNewPublicVirtualInterfaceAllocation ::
  -- | 'virtualInterfaceName'
  Lude.Text ->
  -- | 'vlan'
  Lude.Int ->
  -- | 'asn'
  Lude.Int ->
  NewPublicVirtualInterfaceAllocation
mkNewPublicVirtualInterfaceAllocation
  pVirtualInterfaceName_
  pVlan_
  pAsn_ =
    NewPublicVirtualInterfaceAllocation'
      { routeFilterPrefixes =
          Lude.Nothing,
        customerAddress = Lude.Nothing,
        amazonAddress = Lude.Nothing,
        addressFamily = Lude.Nothing,
        authKey = Lude.Nothing,
        tags = Lude.Nothing,
        virtualInterfaceName = pVirtualInterfaceName_,
        vlan = pVlan_,
        asn = pAsn_
      }

-- | The routes to be advertised to the AWS network in this Region. Applies to public virtual interfaces.
--
-- /Note:/ Consider using 'routeFilterPrefixes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newRouteFilterPrefixes :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe [RouteFilterPrefix])
newRouteFilterPrefixes = Lens.lens (routeFilterPrefixes :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {routeFilterPrefixes = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newRouteFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newCustomerAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
newCustomerAddress = Lens.lens (customerAddress :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newAmazonAddress :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
newAmazonAddress = Lens.lens (amazonAddress :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newAddressFamily :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe AddressFamily)
newAddressFamily = Lens.lens (addressFamily :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newAuthKey :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
newAuthKey = Lens.lens (authKey :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The tags associated with the public virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newTags :: Lens.Lens' NewPublicVirtualInterfaceAllocation (Lude.Maybe (Lude.NonEmpty Tag))
newTags = Lens.lens (tags :: NewPublicVirtualInterfaceAllocation -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newVirtualInterfaceName :: Lens.Lens' NewPublicVirtualInterfaceAllocation Lude.Text
newVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewPublicVirtualInterfaceAllocation -> Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newVlan :: Lens.Lens' NewPublicVirtualInterfaceAllocation Lude.Int
newVlan = Lens.lens (vlan :: NewPublicVirtualInterfaceAllocation -> Lude.Int) (\s a -> s {vlan = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
newAsn :: Lens.Lens' NewPublicVirtualInterfaceAllocation Lude.Int
newAsn = Lens.lens (asn :: NewPublicVirtualInterfaceAllocation -> Lude.Int) (\s a -> s {asn = a} :: NewPublicVirtualInterfaceAllocation)
{-# DEPRECATED newAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

instance Lude.ToJSON NewPublicVirtualInterfaceAllocation where
  toJSON NewPublicVirtualInterfaceAllocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("routeFilterPrefixes" Lude..=) Lude.<$> routeFilterPrefixes,
            ("customerAddress" Lude..=) Lude.<$> customerAddress,
            ("amazonAddress" Lude..=) Lude.<$> amazonAddress,
            ("addressFamily" Lude..=) Lude.<$> addressFamily,
            ("authKey" Lude..=) Lude.<$> authKey,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("virtualInterfaceName" Lude..= virtualInterfaceName),
            Lude.Just ("vlan" Lude..= vlan),
            Lude.Just ("asn" Lude..= asn)
          ]
      )
