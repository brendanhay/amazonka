{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterfaceAllocation
  ( NewTransitVirtualInterfaceAllocation (..),

    -- * Smart constructor
    mkNewTransitVirtualInterfaceAllocation,

    -- * Lenses
    ntviaMtu,
    ntviaCustomerAddress,
    ntviaVlan,
    ntviaAmazonAddress,
    ntviaAddressFamily,
    ntviaAsn,
    ntviaAuthKey,
    ntviaVirtualInterfaceName,
    ntviaTags,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a transit virtual interface to be provisioned on a connection.
--
-- /See:/ 'mkNewTransitVirtualInterfaceAllocation' smart constructor.
data NewTransitVirtualInterfaceAllocation = NewTransitVirtualInterfaceAllocation'
  { -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Lude.Maybe Lude.Int,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Lude.Maybe Lude.Text,
    -- | The ID of the VLAN.
    vlan :: Lude.Maybe Lude.Int,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Lude.Maybe Lude.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Lude.Maybe AddressFamily,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Lude.Maybe Lude.Int,
    -- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Lude.Maybe Lude.Text,
    -- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Lude.Maybe Lude.Text,
    -- | The tags associated with the transitive virtual interface.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewTransitVirtualInterfaceAllocation' with the minimum fields required to make a request.
--
-- * 'mtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
-- * 'customerAddress' - The IP address assigned to the customer interface.
-- * 'vlan' - The ID of the VLAN.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
-- * 'tags' - The tags associated with the transitive virtual interface.
mkNewTransitVirtualInterfaceAllocation ::
  NewTransitVirtualInterfaceAllocation
mkNewTransitVirtualInterfaceAllocation =
  NewTransitVirtualInterfaceAllocation'
    { mtu = Lude.Nothing,
      customerAddress = Lude.Nothing,
      vlan = Lude.Nothing,
      amazonAddress = Lude.Nothing,
      addressFamily = Lude.Nothing,
      asn = Lude.Nothing,
      authKey = Lude.Nothing,
      virtualInterfaceName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaMtu :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe Lude.Int)
ntviaMtu = Lens.lens (mtu :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe Lude.Int) (\s a -> s {mtu = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaCustomerAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
ntviaCustomerAddress = Lens.lens (customerAddress :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaVlan :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe Lude.Int)
ntviaVlan = Lens.lens (vlan :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe Lude.Int) (\s a -> s {vlan = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAmazonAddress :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
ntviaAmazonAddress = Lens.lens (amazonAddress :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAddressFamily :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe AddressFamily)
ntviaAddressFamily = Lens.lens (addressFamily :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAsn :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe Lude.Int)
ntviaAsn = Lens.lens (asn :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe Lude.Int) (\s a -> s {asn = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaAuthKey :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
ntviaAuthKey = Lens.lens (authKey :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaVirtualInterfaceName :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
ntviaVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The tags associated with the transitive virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviaTags :: Lens.Lens' NewTransitVirtualInterfaceAllocation (Lude.Maybe (Lude.NonEmpty Tag))
ntviaTags = Lens.lens (tags :: NewTransitVirtualInterfaceAllocation -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewTransitVirtualInterfaceAllocation)
{-# DEPRECATED ntviaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON NewTransitVirtualInterfaceAllocation where
  toJSON NewTransitVirtualInterfaceAllocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("mtu" Lude..=) Lude.<$> mtu,
            ("customerAddress" Lude..=) Lude.<$> customerAddress,
            ("vlan" Lude..=) Lude.<$> vlan,
            ("amazonAddress" Lude..=) Lude.<$> amazonAddress,
            ("addressFamily" Lude..=) Lude.<$> addressFamily,
            ("asn" Lude..=) Lude.<$> asn,
            ("authKey" Lude..=) Lude.<$> authKey,
            ("virtualInterfaceName" Lude..=) Lude.<$> virtualInterfaceName,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )
