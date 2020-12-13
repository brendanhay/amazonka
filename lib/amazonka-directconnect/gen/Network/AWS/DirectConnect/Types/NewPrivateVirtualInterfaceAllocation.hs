{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewPrivateVirtualInterfaceAllocation
  ( NewPrivateVirtualInterfaceAllocation (..),

    -- * Smart constructor
    mkNewPrivateVirtualInterfaceAllocation,

    -- * Lenses
    npviaMtu,
    npviaCustomerAddress,
    npviaVlan,
    npviaAmazonAddress,
    npviaAddressFamily,
    npviaAsn,
    npviaAuthKey,
    npviaVirtualInterfaceName,
    npviaTags,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a private virtual interface to be provisioned on a connection.
--
-- /See:/ 'mkNewPrivateVirtualInterfaceAllocation' smart constructor.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation'
  { -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Lude.Maybe Lude.Int,
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
    -- | The tags associated with the private virtual interface.
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewPrivateVirtualInterfaceAllocation' with the minimum fields required to make a request.
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
-- * 'tags' - The tags associated with the private virtual interface.
mkNewPrivateVirtualInterfaceAllocation ::
  -- | 'vlan'
  Lude.Int ->
  -- | 'asn'
  Lude.Int ->
  -- | 'virtualInterfaceName'
  Lude.Text ->
  NewPrivateVirtualInterfaceAllocation
mkNewPrivateVirtualInterfaceAllocation
  pVlan_
  pAsn_
  pVirtualInterfaceName_ =
    NewPrivateVirtualInterfaceAllocation'
      { mtu = Lude.Nothing,
        customerAddress = Lude.Nothing,
        vlan = pVlan_,
        amazonAddress = Lude.Nothing,
        addressFamily = Lude.Nothing,
        asn = pAsn_,
        authKey = Lude.Nothing,
        virtualInterfaceName = pVirtualInterfaceName_,
        tags = Lude.Nothing
      }

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaMtu :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Lude.Maybe Lude.Int)
npviaMtu = Lens.lens (mtu :: NewPrivateVirtualInterfaceAllocation -> Lude.Maybe Lude.Int) (\s a -> s {mtu = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaCustomerAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
npviaCustomerAddress = Lens.lens (customerAddress :: NewPrivateVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaVlan :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Lude.Int
npviaVlan = Lens.lens (vlan :: NewPrivateVirtualInterfaceAllocation -> Lude.Int) (\s a -> s {vlan = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAmazonAddress :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
npviaAmazonAddress = Lens.lens (amazonAddress :: NewPrivateVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAddressFamily :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Lude.Maybe AddressFamily)
npviaAddressFamily = Lens.lens (addressFamily :: NewPrivateVirtualInterfaceAllocation -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAsn :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Lude.Int
npviaAsn = Lens.lens (asn :: NewPrivateVirtualInterfaceAllocation -> Lude.Int) (\s a -> s {asn = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaAuthKey :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Lude.Maybe Lude.Text)
npviaAuthKey = Lens.lens (authKey :: NewPrivateVirtualInterfaceAllocation -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaVirtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterfaceAllocation Lude.Text
npviaVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewPrivateVirtualInterfaceAllocation -> Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The tags associated with the private virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviaTags :: Lens.Lens' NewPrivateVirtualInterfaceAllocation (Lude.Maybe (Lude.NonEmpty Tag))
npviaTags = Lens.lens (tags :: NewPrivateVirtualInterfaceAllocation -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewPrivateVirtualInterfaceAllocation)
{-# DEPRECATED npviaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON NewPrivateVirtualInterfaceAllocation where
  toJSON NewPrivateVirtualInterfaceAllocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("mtu" Lude..=) Lude.<$> mtu,
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
