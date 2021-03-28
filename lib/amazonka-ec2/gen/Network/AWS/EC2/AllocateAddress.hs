{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AllocateAddress (..)
    , mkAllocateAddress
    -- ** Request lenses
    , aaAddress
    , aaCustomerOwnedIpv4Pool
    , aaDomain
    , aaDryRun
    , aaNetworkBorderGroup
    , aaPublicIpv4Pool

    -- * Destructuring the response
    , AllocateAddressResponse (..)
    , mkAllocateAddressResponse
    -- ** Response lenses
    , aarrsAllocationId
    , aarrsCarrierIp
    , aarrsCustomerOwnedIp
    , aarrsCustomerOwnedIpv4Pool
    , aarrsDomain
    , aarrsNetworkBorderGroup
    , aarrsPublicIp
    , aarrsPublicIpv4Pool
    , aarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAllocateAddress' smart constructor.
data AllocateAddress = AllocateAddress'
  { address :: Core.Maybe Types.PublicIpAddress
    -- ^ [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
  , customerOwnedIpv4Pool :: Core.Maybe Core.Text
    -- ^ The ID of a customer-owned address pool. Use this parameter to let Amazon EC2 select an address from the address pool. Alternatively, specify a specific address from the address pool.
  , domain :: Core.Maybe Types.DomainType
    -- ^ Indicates whether the Elastic IP address is for use with instances in a VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@ . Otherwise, the default is @vpc@ .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , networkBorderGroup :: Core.Maybe Core.Text
    -- ^ A unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. Use this parameter to limit the IP address to this location. IP addresses cannot move between network border groups.
--
-- Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the network border groups.
  , publicIpv4Pool :: Core.Maybe Types.Ipv4PoolEc2Id
    -- ^ The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateAddress' value with any optional fields omitted.
mkAllocateAddress
    :: AllocateAddress
mkAllocateAddress
  = AllocateAddress'{address = Core.Nothing,
                     customerOwnedIpv4Pool = Core.Nothing, domain = Core.Nothing,
                     dryRun = Core.Nothing, networkBorderGroup = Core.Nothing,
                     publicIpv4Pool = Core.Nothing}

-- | [EC2-VPC] The Elastic IP address to recover or an IPv4 address from an address pool.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAddress :: Lens.Lens' AllocateAddress (Core.Maybe Types.PublicIpAddress)
aaAddress = Lens.field @"address"
{-# INLINEABLE aaAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | The ID of a customer-owned address pool. Use this parameter to let Amazon EC2 select an address from the address pool. Alternatively, specify a specific address from the address pool.
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaCustomerOwnedIpv4Pool :: Lens.Lens' AllocateAddress (Core.Maybe Core.Text)
aaCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# INLINEABLE aaCustomerOwnedIpv4Pool #-}
{-# DEPRECATED customerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead"  #-}

-- | Indicates whether the Elastic IP address is for use with instances in a VPC or instances in EC2-Classic.
--
-- Default: If the Region supports EC2-Classic, the default is @standard@ . Otherwise, the default is @vpc@ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaDomain :: Lens.Lens' AllocateAddress (Core.Maybe Types.DomainType)
aaDomain = Lens.field @"domain"
{-# INLINEABLE aaDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaDryRun :: Lens.Lens' AllocateAddress (Core.Maybe Core.Bool)
aaDryRun = Lens.field @"dryRun"
{-# INLINEABLE aaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | A unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. Use this parameter to limit the IP address to this location. IP addresses cannot move between network border groups.
--
-- Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the network border groups.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaNetworkBorderGroup :: Lens.Lens' AllocateAddress (Core.Maybe Core.Text)
aaNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# INLINEABLE aaNetworkBorderGroup #-}
{-# DEPRECATED networkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead"  #-}

-- | The ID of an address pool that you own. Use this parameter to let Amazon EC2 select an address from the address pool. To specify a specific address from the address pool, use the @Address@ parameter instead.
--
-- /Note:/ Consider using 'publicIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaPublicIpv4Pool :: Lens.Lens' AllocateAddress (Core.Maybe Types.Ipv4PoolEc2Id)
aaPublicIpv4Pool = Lens.field @"publicIpv4Pool"
{-# INLINEABLE aaPublicIpv4Pool #-}
{-# DEPRECATED publicIpv4Pool "Use generic-lens or generic-optics with 'publicIpv4Pool' instead"  #-}

instance Core.ToQuery AllocateAddress where
        toQuery AllocateAddress{..}
          = Core.toQueryPair "Action" ("AllocateAddress" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Address") address
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CustomerOwnedIpv4Pool")
                customerOwnedIpv4Pool
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Domain") domain
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NetworkBorderGroup")
                networkBorderGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PublicIpv4Pool")
                publicIpv4Pool

instance Core.ToHeaders AllocateAddress where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AllocateAddress where
        type Rs AllocateAddress = AllocateAddressResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 AllocateAddressResponse' Core.<$>
                   (x Core..@? "allocationId") Core.<*> x Core..@? "carrierIp"
                     Core.<*> x Core..@? "customerOwnedIp"
                     Core.<*> x Core..@? "customerOwnedIpv4Pool"
                     Core.<*> x Core..@? "domain"
                     Core.<*> x Core..@? "networkBorderGroup"
                     Core.<*> x Core..@? "publicIp"
                     Core.<*> x Core..@? "publicIpv4Pool"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAllocateAddressResponse' smart constructor.
data AllocateAddressResponse = AllocateAddressResponse'
  { allocationId :: Core.Maybe Core.Text
    -- ^ [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
  , carrierIp :: Core.Maybe Core.Text
    -- ^ The carrier IP address. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance). 
  , customerOwnedIp :: Core.Maybe Core.Text
    -- ^ The customer-owned IP address.
  , customerOwnedIpv4Pool :: Core.Maybe Core.Text
    -- ^ The ID of the customer-owned address pool.
  , domain :: Core.Maybe Types.DomainType
    -- ^ Indicates whether the Elastic IP address is for use with instances in a VPC (@vpc@ ) or instances in EC2-Classic (@standard@ ).
  , networkBorderGroup :: Core.Maybe Core.Text
    -- ^ The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
  , publicIp :: Core.Maybe Core.Text
    -- ^ The Elastic IP address.
  , publicIpv4Pool :: Core.Maybe Core.Text
    -- ^ The ID of an address pool.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateAddressResponse' value with any optional fields omitted.
mkAllocateAddressResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AllocateAddressResponse
mkAllocateAddressResponse responseStatus
  = AllocateAddressResponse'{allocationId = Core.Nothing,
                             carrierIp = Core.Nothing, customerOwnedIp = Core.Nothing,
                             customerOwnedIpv4Pool = Core.Nothing, domain = Core.Nothing,
                             networkBorderGroup = Core.Nothing, publicIp = Core.Nothing,
                             publicIpv4Pool = Core.Nothing, responseStatus}

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic IP address for use with instances in a VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsAllocationId :: Lens.Lens' AllocateAddressResponse (Core.Maybe Core.Text)
aarrsAllocationId = Lens.field @"allocationId"
{-# INLINEABLE aarrsAllocationId #-}
{-# DEPRECATED allocationId "Use generic-lens or generic-optics with 'allocationId' instead"  #-}

-- | The carrier IP address. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance). 
--
-- /Note:/ Consider using 'carrierIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsCarrierIp :: Lens.Lens' AllocateAddressResponse (Core.Maybe Core.Text)
aarrsCarrierIp = Lens.field @"carrierIp"
{-# INLINEABLE aarrsCarrierIp #-}
{-# DEPRECATED carrierIp "Use generic-lens or generic-optics with 'carrierIp' instead"  #-}

-- | The customer-owned IP address.
--
-- /Note:/ Consider using 'customerOwnedIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsCustomerOwnedIp :: Lens.Lens' AllocateAddressResponse (Core.Maybe Core.Text)
aarrsCustomerOwnedIp = Lens.field @"customerOwnedIp"
{-# INLINEABLE aarrsCustomerOwnedIp #-}
{-# DEPRECATED customerOwnedIp "Use generic-lens or generic-optics with 'customerOwnedIp' instead"  #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsCustomerOwnedIpv4Pool :: Lens.Lens' AllocateAddressResponse (Core.Maybe Core.Text)
aarrsCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# INLINEABLE aarrsCustomerOwnedIpv4Pool #-}
{-# DEPRECATED customerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead"  #-}

-- | Indicates whether the Elastic IP address is for use with instances in a VPC (@vpc@ ) or instances in EC2-Classic (@standard@ ).
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsDomain :: Lens.Lens' AllocateAddressResponse (Core.Maybe Types.DomainType)
aarrsDomain = Lens.field @"domain"
{-# INLINEABLE aarrsDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsNetworkBorderGroup :: Lens.Lens' AllocateAddressResponse (Core.Maybe Core.Text)
aarrsNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# INLINEABLE aarrsNetworkBorderGroup #-}
{-# DEPRECATED networkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead"  #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsPublicIp :: Lens.Lens' AllocateAddressResponse (Core.Maybe Core.Text)
aarrsPublicIp = Lens.field @"publicIp"
{-# INLINEABLE aarrsPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

-- | The ID of an address pool.
--
-- /Note:/ Consider using 'publicIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsPublicIpv4Pool :: Lens.Lens' AllocateAddressResponse (Core.Maybe Core.Text)
aarrsPublicIpv4Pool = Lens.field @"publicIpv4Pool"
{-# INLINEABLE aarrsPublicIpv4Pool #-}
{-# DEPRECATED publicIpv4Pool "Use generic-lens or generic-optics with 'publicIpv4Pool' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsResponseStatus :: Lens.Lens' AllocateAddressResponse Core.Int
aarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
