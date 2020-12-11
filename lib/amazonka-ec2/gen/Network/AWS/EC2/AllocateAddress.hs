{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AllocateAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates an Elastic IP address to your AWS account. After you allocate the Elastic IP address you can associate it with an instance or network interface. After you release an Elastic IP address, it is released to the IP address pool and can be allocated to a different AWS account.
--
-- You can allocate an Elastic IP address from an address pool owned by AWS or from an address pool created from a public IPv4 address range that you have brought to AWS for use with your AWS resources using bring your own IP addresses (BYOIP). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html Bring Your Own IP Addresses (BYOIP)> in the /Amazon Elastic Compute Cloud User Guide/ .
-- [EC2-VPC] If you release an Elastic IP address, you might be able to recover it. You cannot recover an Elastic IP address that you released after it is allocated to another AWS account. You cannot recover an Elastic IP address for EC2-Classic. To attempt to recover an Elastic IP address that you released, specify it in this operation.
-- An Elastic IP address is for use either in the EC2-Classic platform or in a VPC. By default, you can allocate 5 Elastic IP addresses for EC2-Classic per Region and 5 Elastic IP addresses for EC2-VPC per Region.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon Elastic Compute Cloud User Guide/ .
-- You can allocate a carrier IP address which is a public IP address from a telecommunication carrier, to a network interface which resides in a subnet in a Wavelength Zone (for example an EC2 instance).
module Network.AWS.EC2.AllocateAddress
  ( -- * Creating a request
    AllocateAddress (..),
    mkAllocateAddress,

    -- ** Request lenses
    aaNetworkBorderGroup,
    aaDomain,
    aaAddress,
    aaPublicIPv4Pool,
    aaCustomerOwnedIPv4Pool,
    aaDryRun,

    -- * Destructuring the response
    AllocateAddressResponse (..),
    mkAllocateAddressResponse,

    -- ** Response lenses
    aarsAllocationId,
    aarsCarrierIP,
    aarsNetworkBorderGroup,
    aarsDomain,
    aarsPublicIPv4Pool,
    aarsCustomerOwnedIPv4Pool,
    aarsCustomerOwnedIP,
    aarsPublicIP,
    aarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAllocateAddress' smart constructor.
data AllocateAddress = AllocateAddress'
  { networkBorderGroup ::
      Lude.Maybe Lude.Text,
    domain :: Lude.Maybe DomainType,
    address :: Lude.Maybe Lude.Text,
    publicIPv4Pool :: Lude.Maybe Lude.Text,
    customerOwnedIPv4Pool :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateAddress' with the minimum fields required to make a request.
--
-- * 'address' - [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
-- * 'customerOwnedIPv4Pool' - The ID of a customer-owned address pool. Use this parameter to let Amazon EC2 select an address from the address pool. Alternatively, specify a specific address from the address pool.
-- * 'domain' - Indicates whether the Elastic IP address is for use with instances in a VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@ . Otherwise, the default is @vpc@ .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkBorderGroup' - A unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. Use this parameter to limit the IP address to this location. IP addresses cannot move between network border groups.
--
-- Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the network border groups.
-- * 'publicIPv4Pool' - The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
mkAllocateAddress ::
  AllocateAddress
mkAllocateAddress =
  AllocateAddress'
    { networkBorderGroup = Lude.Nothing,
      domain = Lude.Nothing,
      address = Lude.Nothing,
      publicIPv4Pool = Lude.Nothing,
      customerOwnedIPv4Pool = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | A unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. Use this parameter to limit the IP address to this location. IP addresses cannot move between network border groups.
--
-- Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the network border groups.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaNetworkBorderGroup :: Lens.Lens' AllocateAddress (Lude.Maybe Lude.Text)
aaNetworkBorderGroup = Lens.lens (networkBorderGroup :: AllocateAddress -> Lude.Maybe Lude.Text) (\s a -> s {networkBorderGroup = a} :: AllocateAddress)
{-# DEPRECATED aaNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | Indicates whether the Elastic IP address is for use with instances in a VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@ . Otherwise, the default is @vpc@ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaDomain :: Lens.Lens' AllocateAddress (Lude.Maybe DomainType)
aaDomain = Lens.lens (domain :: AllocateAddress -> Lude.Maybe DomainType) (\s a -> s {domain = a} :: AllocateAddress)
{-# DEPRECATED aaDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAddress :: Lens.Lens' AllocateAddress (Lude.Maybe Lude.Text)
aaAddress = Lens.lens (address :: AllocateAddress -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: AllocateAddress)
{-# DEPRECATED aaAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
--
-- /Note:/ Consider using 'publicIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaPublicIPv4Pool :: Lens.Lens' AllocateAddress (Lude.Maybe Lude.Text)
aaPublicIPv4Pool = Lens.lens (publicIPv4Pool :: AllocateAddress -> Lude.Maybe Lude.Text) (\s a -> s {publicIPv4Pool = a} :: AllocateAddress)
{-# DEPRECATED aaPublicIPv4Pool "Use generic-lens or generic-optics with 'publicIPv4Pool' instead." #-}

-- | The ID of a customer-owned address pool. Use this parameter to let Amazon EC2 select an address from the address pool. Alternatively, specify a specific address from the address pool.
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaCustomerOwnedIPv4Pool :: Lens.Lens' AllocateAddress (Lude.Maybe Lude.Text)
aaCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: AllocateAddress -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: AllocateAddress)
{-# DEPRECATED aaCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaDryRun :: Lens.Lens' AllocateAddress (Lude.Maybe Lude.Bool)
aaDryRun = Lens.lens (dryRun :: AllocateAddress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AllocateAddress)
{-# DEPRECATED aaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AllocateAddress where
  type Rs AllocateAddress = AllocateAddressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AllocateAddressResponse'
            Lude.<$> (x Lude..@? "allocationId")
            Lude.<*> (x Lude..@? "carrierIp")
            Lude.<*> (x Lude..@? "networkBorderGroup")
            Lude.<*> (x Lude..@? "domain")
            Lude.<*> (x Lude..@? "publicIpv4Pool")
            Lude.<*> (x Lude..@? "customerOwnedIpv4Pool")
            Lude.<*> (x Lude..@? "customerOwnedIp")
            Lude.<*> (x Lude..@? "publicIp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AllocateAddress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AllocateAddress where
  toPath = Lude.const "/"

instance Lude.ToQuery AllocateAddress where
  toQuery AllocateAddress' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AllocateAddress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NetworkBorderGroup" Lude.=: networkBorderGroup,
        "Domain" Lude.=: domain,
        "Address" Lude.=: address,
        "PublicIpv4Pool" Lude.=: publicIPv4Pool,
        "CustomerOwnedIpv4Pool" Lude.=: customerOwnedIPv4Pool,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAllocateAddressResponse' smart constructor.
data AllocateAddressResponse = AllocateAddressResponse'
  { allocationId ::
      Lude.Maybe Lude.Text,
    carrierIP :: Lude.Maybe Lude.Text,
    networkBorderGroup :: Lude.Maybe Lude.Text,
    domain :: Lude.Maybe DomainType,
    publicIPv4Pool :: Lude.Maybe Lude.Text,
    customerOwnedIPv4Pool ::
      Lude.Maybe Lude.Text,
    customerOwnedIP :: Lude.Maybe Lude.Text,
    publicIP :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocateAddressResponse' with the minimum fields required to make a request.
--
-- * 'allocationId' - [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
-- * 'carrierIP' - The carrier IP address. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
-- * 'customerOwnedIP' - The customer-owned IP address.
-- * 'customerOwnedIPv4Pool' - The ID of the customer-owned address pool.
-- * 'domain' - Indicates whether the Elastic IP address is for use with instances in a VPC (@vpc@ ) or instances in EC2-Classic (@standard@ ).
-- * 'networkBorderGroup' - The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
-- * 'publicIP' - The Elastic IP address.
-- * 'publicIPv4Pool' - The ID of an address pool.
-- * 'responseStatus' - The response status code.
mkAllocateAddressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AllocateAddressResponse
mkAllocateAddressResponse pResponseStatus_ =
  AllocateAddressResponse'
    { allocationId = Lude.Nothing,
      carrierIP = Lude.Nothing,
      networkBorderGroup = Lude.Nothing,
      domain = Lude.Nothing,
      publicIPv4Pool = Lude.Nothing,
      customerOwnedIPv4Pool = Lude.Nothing,
      customerOwnedIP = Lude.Nothing,
      publicIP = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsAllocationId :: Lens.Lens' AllocateAddressResponse (Lude.Maybe Lude.Text)
aarsAllocationId = Lens.lens (allocationId :: AllocateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {allocationId = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The carrier IP address. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
--
-- /Note:/ Consider using 'carrierIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsCarrierIP :: Lens.Lens' AllocateAddressResponse (Lude.Maybe Lude.Text)
aarsCarrierIP = Lens.lens (carrierIP :: AllocateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {carrierIP = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsCarrierIP "Use generic-lens or generic-optics with 'carrierIP' instead." #-}

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsNetworkBorderGroup :: Lens.Lens' AllocateAddressResponse (Lude.Maybe Lude.Text)
aarsNetworkBorderGroup = Lens.lens (networkBorderGroup :: AllocateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {networkBorderGroup = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | Indicates whether the Elastic IP address is for use with instances in a VPC (@vpc@ ) or instances in EC2-Classic (@standard@ ).
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsDomain :: Lens.Lens' AllocateAddressResponse (Lude.Maybe DomainType)
aarsDomain = Lens.lens (domain :: AllocateAddressResponse -> Lude.Maybe DomainType) (\s a -> s {domain = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The ID of an address pool.
--
-- /Note:/ Consider using 'publicIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsPublicIPv4Pool :: Lens.Lens' AllocateAddressResponse (Lude.Maybe Lude.Text)
aarsPublicIPv4Pool = Lens.lens (publicIPv4Pool :: AllocateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {publicIPv4Pool = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsPublicIPv4Pool "Use generic-lens or generic-optics with 'publicIPv4Pool' instead." #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsCustomerOwnedIPv4Pool :: Lens.Lens' AllocateAddressResponse (Lude.Maybe Lude.Text)
aarsCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: AllocateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | The customer-owned IP address.
--
-- /Note:/ Consider using 'customerOwnedIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsCustomerOwnedIP :: Lens.Lens' AllocateAddressResponse (Lude.Maybe Lude.Text)
aarsCustomerOwnedIP = Lens.lens (customerOwnedIP :: AllocateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIP = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsCustomerOwnedIP "Use generic-lens or generic-optics with 'customerOwnedIP' instead." #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsPublicIP :: Lens.Lens' AllocateAddressResponse (Lude.Maybe Lude.Text)
aarsPublicIP = Lens.lens (publicIP :: AllocateAddressResponse -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarsResponseStatus :: Lens.Lens' AllocateAddressResponse Lude.Int
aarsResponseStatus = Lens.lens (responseStatus :: AllocateAddressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AllocateAddressResponse)
{-# DEPRECATED aarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
