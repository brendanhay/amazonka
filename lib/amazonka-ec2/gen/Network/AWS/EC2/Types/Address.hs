-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Address
  ( Address (..),

    -- * Smart constructor
    mkAddress,

    -- * Lenses
    aAssociationId,
    aInstanceId,
    aNetworkInterfaceOwnerId,
    aAllocationId,
    aCarrierIP,
    aNetworkBorderGroup,
    aDomain,
    aNetworkInterfaceId,
    aPublicIPv4Pool,
    aCustomerOwnedIPv4Pool,
    aCustomerOwnedIP,
    aPrivateIPAddress,
    aPublicIP,
    aTags,
  )
where

import Network.AWS.EC2.Types.DomainType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Elastic IP address, or a carrier IP address.
--
-- /See:/ 'mkAddress' smart constructor.
data Address = Address'
  { associationId :: Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    networkInterfaceOwnerId :: Lude.Maybe Lude.Text,
    allocationId :: Lude.Maybe Lude.Text,
    carrierIP :: Lude.Maybe Lude.Text,
    networkBorderGroup :: Lude.Maybe Lude.Text,
    domain :: Lude.Maybe DomainType,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    publicIPv4Pool :: Lude.Maybe Lude.Text,
    customerOwnedIPv4Pool :: Lude.Maybe Lude.Text,
    customerOwnedIP :: Lude.Maybe Lude.Text,
    privateIPAddress :: Lude.Maybe Lude.Text,
    publicIP :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Address' with the minimum fields required to make a request.
--
-- * 'allocationId' - The ID representing the allocation of the address for use with EC2-VPC.
-- * 'associationId' - The ID representing the association of the address with an instance in a VPC.
-- * 'carrierIP' - The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
-- * 'customerOwnedIP' - The customer-owned IP address.
-- * 'customerOwnedIPv4Pool' - The ID of the customer-owned address pool.
-- * 'domain' - Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
-- * 'instanceId' - The ID of the instance that the address is associated with (if any).
-- * 'networkBorderGroup' - The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'networkInterfaceOwnerId' - The ID of the AWS account that owns the network interface.
-- * 'privateIPAddress' - The private IP address associated with the Elastic IP address.
-- * 'publicIP' - The Elastic IP address.
-- * 'publicIPv4Pool' - The ID of an address pool.
-- * 'tags' - Any tags assigned to the Elastic IP address.
mkAddress ::
  Address
mkAddress =
  Address'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      networkInterfaceOwnerId = Lude.Nothing,
      allocationId = Lude.Nothing,
      carrierIP = Lude.Nothing,
      networkBorderGroup = Lude.Nothing,
      domain = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      publicIPv4Pool = Lude.Nothing,
      customerOwnedIPv4Pool = Lude.Nothing,
      customerOwnedIP = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      publicIP = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID representing the association of the address with an instance in a VPC.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssociationId :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aAssociationId = Lens.lens (associationId :: Address -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: Address)
{-# DEPRECATED aAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the instance that the address is associated with (if any).
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInstanceId :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aInstanceId = Lens.lens (instanceId :: Address -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Address)
{-# DEPRECATED aInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the AWS account that owns the network interface.
--
-- /Note:/ Consider using 'networkInterfaceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkInterfaceOwnerId :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aNetworkInterfaceOwnerId = Lens.lens (networkInterfaceOwnerId :: Address -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceOwnerId = a} :: Address)
{-# DEPRECATED aNetworkInterfaceOwnerId "Use generic-lens or generic-optics with 'networkInterfaceOwnerId' instead." #-}

-- | The ID representing the allocation of the address for use with EC2-VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAllocationId :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aAllocationId = Lens.lens (allocationId :: Address -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: Address)
{-# DEPRECATED aAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
--
-- /Note:/ Consider using 'carrierIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCarrierIP :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aCarrierIP = Lens.lens (carrierIP :: Address -> Lude.Maybe Lude.Text) (\s a -> s {carrierIP = a} :: Address)
{-# DEPRECATED aCarrierIP "Use generic-lens or generic-optics with 'carrierIP' instead." #-}

-- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkBorderGroup :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aNetworkBorderGroup = Lens.lens (networkBorderGroup :: Address -> Lude.Maybe Lude.Text) (\s a -> s {networkBorderGroup = a} :: Address)
{-# DEPRECATED aNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDomain :: Lens.Lens' Address (Lude.Maybe DomainType)
aDomain = Lens.lens (domain :: Address -> Lude.Maybe DomainType) (\s a -> s {domain = a} :: Address)
{-# DEPRECATED aDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkInterfaceId :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aNetworkInterfaceId = Lens.lens (networkInterfaceId :: Address -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: Address)
{-# DEPRECATED aNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of an address pool.
--
-- /Note:/ Consider using 'publicIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPublicIPv4Pool :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aPublicIPv4Pool = Lens.lens (publicIPv4Pool :: Address -> Lude.Maybe Lude.Text) (\s a -> s {publicIPv4Pool = a} :: Address)
{-# DEPRECATED aPublicIPv4Pool "Use generic-lens or generic-optics with 'publicIPv4Pool' instead." #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCustomerOwnedIPv4Pool :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: Address -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: Address)
{-# DEPRECATED aCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | The customer-owned IP address.
--
-- /Note:/ Consider using 'customerOwnedIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCustomerOwnedIP :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aCustomerOwnedIP = Lens.lens (customerOwnedIP :: Address -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIP = a} :: Address)
{-# DEPRECATED aCustomerOwnedIP "Use generic-lens or generic-optics with 'customerOwnedIP' instead." #-}

-- | The private IP address associated with the Elastic IP address.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrivateIPAddress :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aPrivateIPAddress = Lens.lens (privateIPAddress :: Address -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: Address)
{-# DEPRECATED aPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPublicIP :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aPublicIP = Lens.lens (publicIP :: Address -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: Address)
{-# DEPRECATED aPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | Any tags assigned to the Elastic IP address.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTags :: Lens.Lens' Address (Lude.Maybe [Tag])
aTags = Lens.lens (tags :: Address -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Address)
{-# DEPRECATED aTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML Address where
  parseXML x =
    Address'
      Lude.<$> (x Lude..@? "associationId")
      Lude.<*> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "networkInterfaceOwnerId")
      Lude.<*> (x Lude..@? "allocationId")
      Lude.<*> (x Lude..@? "carrierIp")
      Lude.<*> (x Lude..@? "networkBorderGroup")
      Lude.<*> (x Lude..@? "domain")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "publicIpv4Pool")
      Lude.<*> (x Lude..@? "customerOwnedIpv4Pool")
      Lude.<*> (x Lude..@? "customerOwnedIp")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "publicIp")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
