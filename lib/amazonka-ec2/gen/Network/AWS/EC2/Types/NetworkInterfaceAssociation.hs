-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceAssociation
  ( NetworkInterfaceAssociation (..),

    -- * Smart constructor
    mkNetworkInterfaceAssociation,

    -- * Lenses
    niaAssociationId,
    niaPublicDNSName,
    niaAllocationId,
    niaCarrierIP,
    niaIPOwnerId,
    niaCustomerOwnedIP,
    niaPublicIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes association information for an Elastic IP address (IPv4 only), or a Carrier IP address (for a network interface which resides in a subnet in a Wavelength Zone).
--
-- /See:/ 'mkNetworkInterfaceAssociation' smart constructor.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'
  { associationId ::
      Lude.Maybe Lude.Text,
    publicDNSName ::
      Lude.Maybe Lude.Text,
    allocationId ::
      Lude.Maybe Lude.Text,
    carrierIP :: Lude.Maybe Lude.Text,
    ipOwnerId :: Lude.Maybe Lude.Text,
    customerOwnedIP ::
      Lude.Maybe Lude.Text,
    publicIP :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterfaceAssociation' with the minimum fields required to make a request.
--
-- * 'allocationId' - The allocation ID.
-- * 'associationId' - The association ID.
-- * 'carrierIP' - The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet which is associated with a Wavelength Zone.
-- * 'customerOwnedIP' - The customer-owned IP address associated with the network interface.
-- * 'ipOwnerId' - The ID of the Elastic IP address owner.
-- * 'publicDNSName' - The public DNS name.
-- * 'publicIP' - The address of the Elastic IP address bound to the network interface.
mkNetworkInterfaceAssociation ::
  NetworkInterfaceAssociation
mkNetworkInterfaceAssociation =
  NetworkInterfaceAssociation'
    { associationId = Lude.Nothing,
      publicDNSName = Lude.Nothing,
      allocationId = Lude.Nothing,
      carrierIP = Lude.Nothing,
      ipOwnerId = Lude.Nothing,
      customerOwnedIP = Lude.Nothing,
      publicIP = Lude.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAssociationId :: Lens.Lens' NetworkInterfaceAssociation (Lude.Maybe Lude.Text)
niaAssociationId = Lens.lens (associationId :: NetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: NetworkInterfaceAssociation)
{-# DEPRECATED niaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The public DNS name.
--
-- /Note:/ Consider using 'publicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaPublicDNSName :: Lens.Lens' NetworkInterfaceAssociation (Lude.Maybe Lude.Text)
niaPublicDNSName = Lens.lens (publicDNSName :: NetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {publicDNSName = a} :: NetworkInterfaceAssociation)
{-# DEPRECATED niaPublicDNSName "Use generic-lens or generic-optics with 'publicDNSName' instead." #-}

-- | The allocation ID.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAllocationId :: Lens.Lens' NetworkInterfaceAssociation (Lude.Maybe Lude.Text)
niaAllocationId = Lens.lens (allocationId :: NetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: NetworkInterfaceAssociation)
{-# DEPRECATED niaAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet which is associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'carrierIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaCarrierIP :: Lens.Lens' NetworkInterfaceAssociation (Lude.Maybe Lude.Text)
niaCarrierIP = Lens.lens (carrierIP :: NetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {carrierIP = a} :: NetworkInterfaceAssociation)
{-# DEPRECATED niaCarrierIP "Use generic-lens or generic-optics with 'carrierIP' instead." #-}

-- | The ID of the Elastic IP address owner.
--
-- /Note:/ Consider using 'ipOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaIPOwnerId :: Lens.Lens' NetworkInterfaceAssociation (Lude.Maybe Lude.Text)
niaIPOwnerId = Lens.lens (ipOwnerId :: NetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {ipOwnerId = a} :: NetworkInterfaceAssociation)
{-# DEPRECATED niaIPOwnerId "Use generic-lens or generic-optics with 'ipOwnerId' instead." #-}

-- | The customer-owned IP address associated with the network interface.
--
-- /Note:/ Consider using 'customerOwnedIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaCustomerOwnedIP :: Lens.Lens' NetworkInterfaceAssociation (Lude.Maybe Lude.Text)
niaCustomerOwnedIP = Lens.lens (customerOwnedIP :: NetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIP = a} :: NetworkInterfaceAssociation)
{-# DEPRECATED niaCustomerOwnedIP "Use generic-lens or generic-optics with 'customerOwnedIP' instead." #-}

-- | The address of the Elastic IP address bound to the network interface.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaPublicIP :: Lens.Lens' NetworkInterfaceAssociation (Lude.Maybe Lude.Text)
niaPublicIP = Lens.lens (publicIP :: NetworkInterfaceAssociation -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: NetworkInterfaceAssociation)
{-# DEPRECATED niaPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

instance Lude.FromXML NetworkInterfaceAssociation where
  parseXML x =
    NetworkInterfaceAssociation'
      Lude.<$> (x Lude..@? "associationId")
      Lude.<*> (x Lude..@? "publicDnsName")
      Lude.<*> (x Lude..@? "allocationId")
      Lude.<*> (x Lude..@? "carrierIp")
      Lude.<*> (x Lude..@? "ipOwnerId")
      Lude.<*> (x Lude..@? "customerOwnedIp")
      Lude.<*> (x Lude..@? "publicIp")
