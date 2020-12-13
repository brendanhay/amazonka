{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
  ( InstanceNetworkInterfaceAssociation (..),

    -- * Smart constructor
    mkInstanceNetworkInterfaceAssociation,

    -- * Lenses
    iniaPublicDNSName,
    iniaCarrierIP,
    iniaIPOwnerId,
    iniaPublicIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes association information for an Elastic IP address (IPv4).
--
-- /See:/ 'mkInstanceNetworkInterfaceAssociation' smart constructor.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'
  { -- | The public DNS name.
    publicDNSName :: Lude.Maybe Lude.Text,
    -- | The carrier IP address associated with the network interface.
    carrierIP :: Lude.Maybe Lude.Text,
    -- | The ID of the owner of the Elastic IP address.
    ipOwnerId :: Lude.Maybe Lude.Text,
    -- | The public IP address or Elastic IP address bound to the network interface.
    publicIP :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceNetworkInterfaceAssociation' with the minimum fields required to make a request.
--
-- * 'publicDNSName' - The public DNS name.
-- * 'carrierIP' - The carrier IP address associated with the network interface.
-- * 'ipOwnerId' - The ID of the owner of the Elastic IP address.
-- * 'publicIP' - The public IP address or Elastic IP address bound to the network interface.
mkInstanceNetworkInterfaceAssociation ::
  InstanceNetworkInterfaceAssociation
mkInstanceNetworkInterfaceAssociation =
  InstanceNetworkInterfaceAssociation'
    { publicDNSName =
        Lude.Nothing,
      carrierIP = Lude.Nothing,
      ipOwnerId = Lude.Nothing,
      publicIP = Lude.Nothing
    }

-- | The public DNS name.
--
-- /Note:/ Consider using 'publicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaPublicDNSName :: Lens.Lens' InstanceNetworkInterfaceAssociation (Lude.Maybe Lude.Text)
iniaPublicDNSName = Lens.lens (publicDNSName :: InstanceNetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {publicDNSName = a} :: InstanceNetworkInterfaceAssociation)
{-# DEPRECATED iniaPublicDNSName "Use generic-lens or generic-optics with 'publicDNSName' instead." #-}

-- | The carrier IP address associated with the network interface.
--
-- /Note:/ Consider using 'carrierIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaCarrierIP :: Lens.Lens' InstanceNetworkInterfaceAssociation (Lude.Maybe Lude.Text)
iniaCarrierIP = Lens.lens (carrierIP :: InstanceNetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {carrierIP = a} :: InstanceNetworkInterfaceAssociation)
{-# DEPRECATED iniaCarrierIP "Use generic-lens or generic-optics with 'carrierIP' instead." #-}

-- | The ID of the owner of the Elastic IP address.
--
-- /Note:/ Consider using 'ipOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaIPOwnerId :: Lens.Lens' InstanceNetworkInterfaceAssociation (Lude.Maybe Lude.Text)
iniaIPOwnerId = Lens.lens (ipOwnerId :: InstanceNetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ipOwnerId = a} :: InstanceNetworkInterfaceAssociation)
{-# DEPRECATED iniaIPOwnerId "Use generic-lens or generic-optics with 'ipOwnerId' instead." #-}

-- | The public IP address or Elastic IP address bound to the network interface.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaPublicIP :: Lens.Lens' InstanceNetworkInterfaceAssociation (Lude.Maybe Lude.Text)
iniaPublicIP = Lens.lens (publicIP :: InstanceNetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: InstanceNetworkInterfaceAssociation)
{-# DEPRECATED iniaPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

instance Lude.FromXML InstanceNetworkInterfaceAssociation where
  parseXML x =
    InstanceNetworkInterfaceAssociation'
      Lude.<$> (x Lude..@? "publicDnsName")
      Lude.<*> (x Lude..@? "carrierIp")
      Lude.<*> (x Lude..@? "ipOwnerId")
      Lude.<*> (x Lude..@? "publicIp")
