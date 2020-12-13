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
    npvifVirtualGatewayId,
    npvifMtu,
    npvifCustomerAddress,
    npvifVlan,
    npvifAmazonAddress,
    npvifAddressFamily,
    npvifDirectConnectGatewayId,
    npvifAsn,
    npvifAuthKey,
    npvifVirtualInterfaceName,
    npvifTags,
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
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Lude.Maybe Lude.Text,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Lude.Maybe Lude.Int,
    -- | The IP address assigned to the customer interface.
    customerAddress :: Lude.Maybe Lude.Text,
    -- | The ID of the VLAN.
    vlan :: Lude.Int,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Lude.Maybe Lude.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Lude.Maybe AddressFamily,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'NewPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
-- * 'mtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
-- * 'customerAddress' - The IP address assigned to the customer interface.
-- * 'vlan' - The ID of the VLAN.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
-- * 'tags' - The tags associated with the private virtual interface.
mkNewPrivateVirtualInterface ::
  -- | 'vlan'
  Lude.Int ->
  -- | 'asn'
  Lude.Int ->
  -- | 'virtualInterfaceName'
  Lude.Text ->
  NewPrivateVirtualInterface
mkNewPrivateVirtualInterface pVlan_ pAsn_ pVirtualInterfaceName_ =
  NewPrivateVirtualInterface'
    { virtualGatewayId = Lude.Nothing,
      mtu = Lude.Nothing,
      customerAddress = Lude.Nothing,
      vlan = pVlan_,
      amazonAddress = Lude.Nothing,
      addressFamily = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      asn = pAsn_,
      authKey = Lude.Nothing,
      virtualInterfaceName = pVirtualInterfaceName_,
      tags = Lude.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifVirtualGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
npvifVirtualGatewayId = Lens.lens (virtualGatewayId :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifMtu :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Int)
npvifMtu = Lens.lens (mtu :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {mtu = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifCustomerAddress :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
npvifCustomerAddress = Lens.lens (customerAddress :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifVlan :: Lens.Lens' NewPrivateVirtualInterface Lude.Int
npvifVlan = Lens.lens (vlan :: NewPrivateVirtualInterface -> Lude.Int) (\s a -> s {vlan = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifAmazonAddress :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
npvifAmazonAddress = Lens.lens (amazonAddress :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifAddressFamily :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe AddressFamily)
npvifAddressFamily = Lens.lens (addressFamily :: NewPrivateVirtualInterface -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifDirectConnectGatewayId :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
npvifDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifAsn :: Lens.Lens' NewPrivateVirtualInterface Lude.Int
npvifAsn = Lens.lens (asn :: NewPrivateVirtualInterface -> Lude.Int) (\s a -> s {asn = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifAuthKey :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe Lude.Text)
npvifAuthKey = Lens.lens (authKey :: NewPrivateVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifVirtualInterfaceName :: Lens.Lens' NewPrivateVirtualInterface Lude.Text
npvifVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewPrivateVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The tags associated with the private virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npvifTags :: Lens.Lens' NewPrivateVirtualInterface (Lude.Maybe (Lude.NonEmpty Tag))
npvifTags = Lens.lens (tags :: NewPrivateVirtualInterface -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewPrivateVirtualInterface)
{-# DEPRECATED npvifTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON NewPrivateVirtualInterface where
  toJSON NewPrivateVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("virtualGatewayId" Lude..=) Lude.<$> virtualGatewayId,
            ("mtu" Lude..=) Lude.<$> mtu,
            ("customerAddress" Lude..=) Lude.<$> customerAddress,
            Lude.Just ("vlan" Lude..= vlan),
            ("amazonAddress" Lude..=) Lude.<$> amazonAddress,
            ("addressFamily" Lude..=) Lude.<$> addressFamily,
            ("directConnectGatewayId" Lude..=) Lude.<$> directConnectGatewayId,
            Lude.Just ("asn" Lude..= asn),
            ("authKey" Lude..=) Lude.<$> authKey,
            Lude.Just ("virtualInterfaceName" Lude..= virtualInterfaceName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )
