{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstancePrivateIPAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstancePrivateIPAddress
  ( InstancePrivateIPAddress (..),

    -- * Smart constructor
    mkInstancePrivateIPAddress,

    -- * Lenses
    ipiaPrimary,
    ipiaPrivateIPAddress,
    ipiaPrivateDNSName,
    ipiaAssociation,
  )
where

import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a private IPv4 address.
--
-- /See:/ 'mkInstancePrivateIPAddress' smart constructor.
data InstancePrivateIPAddress = InstancePrivateIPAddress'
  { primary ::
      Lude.Maybe Lude.Bool,
    privateIPAddress :: Lude.Maybe Lude.Text,
    privateDNSName :: Lude.Maybe Lude.Text,
    association ::
      Lude.Maybe
        InstanceNetworkInterfaceAssociation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstancePrivateIPAddress' with the minimum fields required to make a request.
--
-- * 'association' - The association information for an Elastic IP address for the network interface.
-- * 'primary' - Indicates whether this IPv4 address is the primary private IP address of the network interface.
-- * 'privateDNSName' - The private IPv4 DNS name.
-- * 'privateIPAddress' - The private IPv4 address of the network interface.
mkInstancePrivateIPAddress ::
  InstancePrivateIPAddress
mkInstancePrivateIPAddress =
  InstancePrivateIPAddress'
    { primary = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      association = Lude.Nothing
    }

-- | Indicates whether this IPv4 address is the primary private IP address of the network interface.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaPrimary :: Lens.Lens' InstancePrivateIPAddress (Lude.Maybe Lude.Bool)
ipiaPrimary = Lens.lens (primary :: InstancePrivateIPAddress -> Lude.Maybe Lude.Bool) (\s a -> s {primary = a} :: InstancePrivateIPAddress)
{-# DEPRECATED ipiaPrimary "Use generic-lens or generic-optics with 'primary' instead." #-}

-- | The private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaPrivateIPAddress :: Lens.Lens' InstancePrivateIPAddress (Lude.Maybe Lude.Text)
ipiaPrivateIPAddress = Lens.lens (privateIPAddress :: InstancePrivateIPAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: InstancePrivateIPAddress)
{-# DEPRECATED ipiaPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The private IPv4 DNS name.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaPrivateDNSName :: Lens.Lens' InstancePrivateIPAddress (Lude.Maybe Lude.Text)
ipiaPrivateDNSName = Lens.lens (privateDNSName :: InstancePrivateIPAddress -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: InstancePrivateIPAddress)
{-# DEPRECATED ipiaPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The association information for an Elastic IP address for the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipiaAssociation :: Lens.Lens' InstancePrivateIPAddress (Lude.Maybe InstanceNetworkInterfaceAssociation)
ipiaAssociation = Lens.lens (association :: InstancePrivateIPAddress -> Lude.Maybe InstanceNetworkInterfaceAssociation) (\s a -> s {association = a} :: InstancePrivateIPAddress)
{-# DEPRECATED ipiaAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

instance Lude.FromXML InstancePrivateIPAddress where
  parseXML x =
    InstancePrivateIPAddress'
      Lude.<$> (x Lude..@? "primary")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "privateDnsName")
      Lude.<*> (x Lude..@? "association")
