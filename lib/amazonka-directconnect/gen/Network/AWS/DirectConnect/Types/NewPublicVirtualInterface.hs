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
    npviRouteFilterPrefixes,
    npviCustomerAddress,
    npviAmazonAddress,
    npviAddressFamily,
    npviAuthKey,
    npviTags,
    npviVirtualInterfaceName,
    npviVlan,
    npviAsn,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a public virtual interface.
--
-- /See:/ 'mkNewPublicVirtualInterface' smart constructor.
data NewPublicVirtualInterface = NewPublicVirtualInterface'
  { routeFilterPrefixes ::
      Lude.Maybe [RouteFilterPrefix],
    customerAddress :: Lude.Maybe Lude.Text,
    amazonAddress :: Lude.Maybe Lude.Text,
    addressFamily ::
      Lude.Maybe AddressFamily,
    authKey :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
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

-- | Creates a value of 'NewPublicVirtualInterface' with the minimum fields required to make a request.
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
mkNewPublicVirtualInterface ::
  -- | 'virtualInterfaceName'
  Lude.Text ->
  -- | 'vlan'
  Lude.Int ->
  -- | 'asn'
  Lude.Int ->
  NewPublicVirtualInterface
mkNewPublicVirtualInterface pVirtualInterfaceName_ pVlan_ pAsn_ =
  NewPublicVirtualInterface'
    { routeFilterPrefixes = Lude.Nothing,
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
npviRouteFilterPrefixes :: Lens.Lens' NewPublicVirtualInterface (Lude.Maybe [RouteFilterPrefix])
npviRouteFilterPrefixes = Lens.lens (routeFilterPrefixes :: NewPublicVirtualInterface -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {routeFilterPrefixes = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviRouteFilterPrefixes "Use generic-lens or generic-optics with 'routeFilterPrefixes' instead." #-}

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviCustomerAddress :: Lens.Lens' NewPublicVirtualInterface (Lude.Maybe Lude.Text)
npviCustomerAddress = Lens.lens (customerAddress :: NewPublicVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAmazonAddress :: Lens.Lens' NewPublicVirtualInterface (Lude.Maybe Lude.Text)
npviAmazonAddress = Lens.lens (amazonAddress :: NewPublicVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAddressFamily :: Lens.Lens' NewPublicVirtualInterface (Lude.Maybe AddressFamily)
npviAddressFamily = Lens.lens (addressFamily :: NewPublicVirtualInterface -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAuthKey :: Lens.Lens' NewPublicVirtualInterface (Lude.Maybe Lude.Text)
npviAuthKey = Lens.lens (authKey :: NewPublicVirtualInterface -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

-- | The tags associated with the public virtual interface.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviTags :: Lens.Lens' NewPublicVirtualInterface (Lude.Maybe (Lude.NonEmpty Tag))
npviTags = Lens.lens (tags :: NewPublicVirtualInterface -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the virtual interface assigned by the customer network. The name has a maximum of 100 characters. The following are valid characters: a-z, 0-9 and a hyphen (-).
--
-- /Note:/ Consider using 'virtualInterfaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviVirtualInterfaceName :: Lens.Lens' NewPublicVirtualInterface Lude.Text
npviVirtualInterfaceName = Lens.lens (virtualInterfaceName :: NewPublicVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceName = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviVirtualInterfaceName "Use generic-lens or generic-optics with 'virtualInterfaceName' instead." #-}

-- | The ID of the VLAN.
--
-- /Note:/ Consider using 'vlan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviVlan :: Lens.Lens' NewPublicVirtualInterface Lude.Int
npviVlan = Lens.lens (vlan :: NewPublicVirtualInterface -> Lude.Int) (\s a -> s {vlan = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviVlan "Use generic-lens or generic-optics with 'vlan' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- The valid values are 1-2147483647.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npviAsn :: Lens.Lens' NewPublicVirtualInterface Lude.Int
npviAsn = Lens.lens (asn :: NewPublicVirtualInterface -> Lude.Int) (\s a -> s {asn = a} :: NewPublicVirtualInterface)
{-# DEPRECATED npviAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

instance Lude.ToJSON NewPublicVirtualInterface where
  toJSON NewPublicVirtualInterface' {..} =
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
