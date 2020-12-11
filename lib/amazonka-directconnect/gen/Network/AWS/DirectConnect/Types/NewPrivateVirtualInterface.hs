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
    nVirtualGatewayId,
    nMtu,
    nCustomerAddress,
    nAmazonAddress,
    nAddressFamily,
    nDirectConnectGatewayId,
    nAuthKey,
    nTags,
    nVirtualInterfaceName,
    nVlan,
    nAsn,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a private virtual interface.
--
-- /See:/ 'mkNewPrivateVirtualInterface' smart constructor.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface'
  { virtualGatewayId ::
      Lude.Maybe Lude.Text,
    mtu :: Lude.Maybe Lude.Int,
    customerAddress ::
      Lude.Maybe Lude.Text,
    amazonAddress :: Lude.Maybe Lude.Text,
    addressFamily ::
      Lude.Maybe AddressFamily,
    directConnectGatewayId ::
      Lude.Maybe Lude.Text,
    authKey :: Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe (Lude.NonEmpty Tag),
    virtualInterfaceName :: Lude.Text,
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

-- | Creates a value of 'NewPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'customerAddress' - The IP address assigned to the customer interface.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'mtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
-- * 'tags' - The tags associated with the private virtual interface.
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
-- * 'virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
-- * 'vlan' - The ID of the VLAN.
mkNewPrivateVirtualInterface ::
  -- | 'virtualInterfaceName'
  Lude.Text ->
  -- | 'vlan'
  Lude.Int ->
  -- | 'asn'
  Lude.Int ->
  NewPrivateVirtualInterface
mkNewPrivateVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPrivateVirtualInterface'
    { virtualGatewayId = Lude.Nothing,
      mtu = Lude.Nothing,
      customerAddress = Lude.Nothing,
      amazonAddress = Lude.Nothing,
      addressFamily = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      authKey = Lude.Nothing,
      tags = Lude.Nothing,
      virtualInterfaceName = pVirtualInterfaceName_,
      vlan = pVlan_,
      asn = pAsn_
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVirtualGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
nVirtualGatewayId = Lens.lens (virtualGatewayId :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nMtu :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Int)
nMtu = Lens.lens (mtu :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {mtu = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCustomerAddress :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
nCustomerAddress = Lens.lens (customerAddress :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAmazonAddress :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
nAmazonAddress = Lens.lens (amazonAddress :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAddressFamily :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe AddressFamily)
nAddressFamily = Lens.lens (addressFamily :: NewPrivateVirtualInterface -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nDirectConnectGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
nDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAuthKey :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
nAuthKey = Lens.lens (authKey :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The tags associated with the private virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nTags :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe (Lude.NonEmpty Tag))
nTags = Lens.lens (tags :: NewPrivateVirtualInterface -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVirtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterface Lude.Text
nVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewPrivateVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nVlan :: Lens.Lens' NewPrivateVirtualInterface Lude.Int
nVlan = Lens.lens (vlan :: NewPrivateVirtualInterface -> Lude.Int) (\s a -> s {vlan = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAsn :: Lens.Lens' NewPrivateVirtualInterface Lude.Int
nAsn = Lens.lens (asn :: NewPrivateVirtualInterface -> Lude.Int) (\s a -> s {asn = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED nAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

instance Lude.ToJSON NewPrivateVirtualInterface where
  toJSON NewPrivateVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("virtualGatewayId" Lude..=) Lude.<$> virtualGatewayId,
            ("mtu" Lude..=) Lude.<$> mtu,
            ("customerAddress" Lude..=) Lude.<$> customerAddress,
            ("amazonAddress" Lude..=) Lude.<$> amazonAddress,
            ("addressFamily" Lude..=) Lude.<$> addressFamily,
            ("directConnectGatewayId" Lude..=) Lude.<$> directConnectGatewayId,
            ("authKey" Lude..=) Lude.<$> authKey,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("virtualInterfaceName" Lude..= virtualInterfaceName),
            Lude.Just ("vlan" Lude..= vlan),
            Lude.Just ("asn" Lude..= asn)
          ]
      )
