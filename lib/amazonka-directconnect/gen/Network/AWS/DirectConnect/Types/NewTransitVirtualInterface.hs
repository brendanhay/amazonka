{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewTransitVirtualInterface
  ( NewTransitVirtualInterface (..),

    -- * Smart constructor
    mkNewTransitVirtualInterface,

    -- * Lenses
    ntviMtu,
    ntviCustomerAddress,
    ntviVlan,
    ntviAmazonAddress,
    ntviAddressFamily,
    ntviDirectConnectGatewayId,
    ntviAsn,
    ntviAuthKey,
    ntviVirtualInterfaceName,
    ntviTags,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a transit virtual interface.
--
-- /See:/ 'mkNewTransitVirtualInterface' smart constructor.
data NewTransitVirtualInterface = NewTransitVirtualInterface'
  { mtu ::
      Lude.Maybe Lude.Int,
    customerAddress ::
      Lude.Maybe Lude.Text,
    vlan :: Lude.Maybe Lude.Int,
    amazonAddress :: Lude.Maybe Lude.Text,
    addressFamily ::
      Lude.Maybe AddressFamily,
    directConnectGatewayId ::
      Lude.Maybe Lude.Text,
    asn :: Lude.Maybe Lude.Int,
    authKey :: Lude.Maybe Lude.Text,
    virtualInterfaceName ::
      Lude.Maybe Lude.Text,
    tags ::
      Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewTransitVirtualInterface' with the minimum fields required to make a request.
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
-- * 'tags' - The tags associated with the transitive virtual interface.
-- * 'virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
-- * 'vlan' - The ID of the VLAN.
mkNewTransitVirtualInterface ::
  NewTransitVirtualInterface
mkNewTransitVirtualInterface =
  NewTransitVirtualInterface'
    { mtu = Lude.Nothing,
      customerAddress = Lude.Nothing,
      vlan = Lude.Nothing,
      amazonAddress = Lude.Nothing,
      addressFamily = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      asn = Lude.Nothing,
      authKey = Lude.Nothing,
      virtualInterfaceName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviMtu :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Int)
ntviMtu = Lens.lens (mtu :: NewTransitVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {mtu = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviCustomerAddress :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Text)
ntviCustomerAddress = Lens.lens (customerAddress :: NewTransitVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviVlan :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Int)
ntviVlan = Lens.lens (vlan :: NewTransitVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {vlan = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAmazonAddress :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Text)
ntviAmazonAddress = Lens.lens (amazonAddress :: NewTransitVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAddressFamily :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe AddressFamily)
ntviAddressFamily = Lens.lens (addressFamily :: NewTransitVirtualInterface -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviDirectConnectGatewayId :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Text)
ntviDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: NewTransitVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAsn :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Int)
ntviAsn = Lens.lens (asn :: NewTransitVirtualInterface -> Lude.Maybe Lude.Int) (\s a -> s {asn = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviAuthKey :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Text)
ntviAuthKey = Lens.lens (authKey :: NewTransitVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviVirtualInterfaceName :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe Lude.Text)
ntviVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewTransitVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The tags associated with the transitive virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntviTags :: Lens.Lens' NewTransitVirtualInterface (Lude.Maybe (Lude.NonEmpty Tag))
ntviTags = Lens.lens (tags :: NewTransitVirtualInterface -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewTransitVirtualInterface)
{-# DEPRECATED ntviTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON NewTransitVirtualInterface where
  toJSON NewTransitVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("mtu" Lude..=) Lude.<$> mtu,
            ("customerAddress" Lude..=) Lude.<$> customerAddress,
            ("vlan" Lude..=) Lude.<$> vlan,
            ("amazonAddress" Lude..=) Lude.<$> amazonAddress,
            ("addressFamily" Lude..=) Lude.<$> addressFamily,
            ("directConnectGatewayId" Lude..=) Lude.<$> directConnectGatewayId,
            ("asn" Lude..=) Lude.<$> asn,
            ("authKey" Lude..=) Lude.<$> authKey,
            ("virtualInterfaceName" Lude..=) Lude.<$> virtualInterfaceName,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )
