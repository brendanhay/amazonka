{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePrivateIPAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePrivateIPAddress
  ( NetworkInterfacePrivateIPAddress (..),

    -- * Smart constructor
    mkNetworkInterfacePrivateIPAddress,

    -- * Lenses
    nipiaPrimary,
    nipiaPrivateIPAddress,
    nipiaPrivateDNSName,
    nipiaAssociation,
  )
where

import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the private IPv4 address of a network interface.
--
-- /See:/ 'mkNetworkInterfacePrivateIPAddress' smart constructor.
data NetworkInterfacePrivateIPAddress = NetworkInterfacePrivateIPAddress'
  { -- | Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
    primary :: Lude.Maybe Lude.Bool,
    -- | The private IPv4 address.
    privateIPAddress :: Lude.Maybe Lude.Text,
    -- | The private DNS name.
    privateDNSName :: Lude.Maybe Lude.Text,
    -- | The association information for an Elastic IP address (IPv4) associated with the network interface.
    association :: Lude.Maybe NetworkInterfaceAssociation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterfacePrivateIPAddress' with the minimum fields required to make a request.
--
-- * 'primary' - Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
-- * 'privateIPAddress' - The private IPv4 address.
-- * 'privateDNSName' - The private DNS name.
-- * 'association' - The association information for an Elastic IP address (IPv4) associated with the network interface.
mkNetworkInterfacePrivateIPAddress ::
  NetworkInterfacePrivateIPAddress
mkNetworkInterfacePrivateIPAddress =
  NetworkInterfacePrivateIPAddress'
    { primary = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      association = Lude.Nothing
    }

-- | Indicates whether this IPv4 address is the primary private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaPrimary :: Lens.Lens' NetworkInterfacePrivateIPAddress (Lude.Maybe Lude.Bool)
nipiaPrimary = Lens.lens (primary :: NetworkInterfacePrivateIPAddress -> Lude.Maybe Lude.Bool) (\s a -> s {primary = a} :: NetworkInterfacePrivateIPAddress)
{-# DEPRECATED nipiaPrimary "Use generic-lens or generic-optics with 'primary' instead." #-}

-- | The private IPv4 address.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaPrivateIPAddress :: Lens.Lens' NetworkInterfacePrivateIPAddress (Lude.Maybe Lude.Text)
nipiaPrivateIPAddress = Lens.lens (privateIPAddress :: NetworkInterfacePrivateIPAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: NetworkInterfacePrivateIPAddress)
{-# DEPRECATED nipiaPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The private DNS name.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaPrivateDNSName :: Lens.Lens' NetworkInterfacePrivateIPAddress (Lude.Maybe Lude.Text)
nipiaPrivateDNSName = Lens.lens (privateDNSName :: NetworkInterfacePrivateIPAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: NetworkInterfacePrivateIPAddress)
{-# DEPRECATED nipiaPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipiaAssociation :: Lens.Lens' NetworkInterfacePrivateIPAddress (Lude.Maybe NetworkInterfaceAssociation)
nipiaAssociation = Lens.lens (association :: NetworkInterfacePrivateIPAddress -> Lude.Maybe NetworkInterfaceAssociation) (\s a -> s {association = a} :: NetworkInterfacePrivateIPAddress)
{-# DEPRECATED nipiaAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

instance Lude.FromXML NetworkInterfacePrivateIPAddress where
  parseXML x =
    NetworkInterfacePrivateIPAddress'
      Lude.<$> (x Lude..@? "primary")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "privateDnsName")
      Lude.<*> (x Lude..@? "association")
