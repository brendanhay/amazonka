{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC with the specified IPv4 CIDR block. The smallest VPC you can create uses a /28 netmask (16 IPv4 addresses), and the largest uses a /16 netmask (65,536 IPv4 addresses). For more information about how large to make your VPC, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- You can optionally request an IPv6 CIDR block for the VPC. You can request an Amazon-provided IPv6 CIDR block from Amazon's pool of IPv6 addresses, or an IPv6 CIDR block from an IPv6 address pool that you provisioned through bring your own IP addresses (<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html BYOIP> ).
-- By default, each instance you launch in the VPC has the default DHCP options, which include only a default DNS server that we provide (AmazonProvidedDNS). For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
-- You can specify the instance tenancy value for the VPC when you create it. You can't change this value for the VPC after you create it. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/dedicated-instance.html Dedicated Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateVpc
    (
    -- * Creating a request
      CreateVpc (..)
    , mkCreateVpc
    -- ** Request lenses
    , cvCidrBlock
    , cvAmazonProvidedIpv6CidrBlock
    , cvDryRun
    , cvInstanceTenancy
    , cvIpv6CidrBlock
    , cvIpv6CidrBlockNetworkBorderGroup
    , cvIpv6Pool
    , cvTagSpecifications

    -- * Destructuring the response
    , CreateVpcResponse (..)
    , mkCreateVpcResponse
    -- ** Response lenses
    , cvrrsVpc
    , cvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateVpc' smart constructor.
data CreateVpc = CreateVpc'
  { cidrBlock :: Core.Text
    -- ^ The IPv4 network range for the VPC, in CIDR notation. For example, @10.0.0.0/16@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
  , amazonProvidedIpv6CidrBlock :: Core.Maybe Core.Bool
    -- ^ Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IP addresses, or the size of the CIDR block.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , instanceTenancy :: Core.Maybe Types.Tenancy
    -- ^ The tenancy options for instances launched into the VPC. For @default@ , instances are launched with shared tenancy by default. You can launch instances with any tenancy into a shared tenancy VPC. For @dedicated@ , instances are launched as dedicated tenancy instances by default. You can only launch instances with a tenancy of @dedicated@ or @host@ into a dedicated tenancy VPC. 
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only.
-- Default: @default@ 
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
  , ipv6CidrBlockNetworkBorderGroup :: Core.Maybe Core.Text
    -- ^ The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the address to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
  , ipv6Pool :: Core.Maybe Types.Ipv6PoolEc2Id
    -- ^ The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpc' value with any optional fields omitted.
mkCreateVpc
    :: Core.Text -- ^ 'cidrBlock'
    -> CreateVpc
mkCreateVpc cidrBlock
  = CreateVpc'{cidrBlock, amazonProvidedIpv6CidrBlock = Core.Nothing,
               dryRun = Core.Nothing, instanceTenancy = Core.Nothing,
               ipv6CidrBlock = Core.Nothing,
               ipv6CidrBlockNetworkBorderGroup = Core.Nothing,
               ipv6Pool = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The IPv4 network range for the VPC, in CIDR notation. For example, @10.0.0.0/16@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvCidrBlock :: Lens.Lens' CreateVpc Core.Text
cvCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE cvCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IP addresses, or the size of the CIDR block.
--
-- /Note:/ Consider using 'amazonProvidedIpv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvAmazonProvidedIpv6CidrBlock :: Lens.Lens' CreateVpc (Core.Maybe Core.Bool)
cvAmazonProvidedIpv6CidrBlock = Lens.field @"amazonProvidedIpv6CidrBlock"
{-# INLINEABLE cvAmazonProvidedIpv6CidrBlock #-}
{-# DEPRECATED amazonProvidedIpv6CidrBlock "Use generic-lens or generic-optics with 'amazonProvidedIpv6CidrBlock' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvDryRun :: Lens.Lens' CreateVpc (Core.Maybe Core.Bool)
cvDryRun = Lens.field @"dryRun"
{-# INLINEABLE cvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tenancy options for instances launched into the VPC. For @default@ , instances are launched with shared tenancy by default. You can launch instances with any tenancy into a shared tenancy VPC. For @dedicated@ , instances are launched as dedicated tenancy instances by default. You can only launch instances with a tenancy of @dedicated@ or @host@ into a dedicated tenancy VPC. 
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only.
-- Default: @default@ 
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvInstanceTenancy :: Lens.Lens' CreateVpc (Core.Maybe Types.Tenancy)
cvInstanceTenancy = Lens.field @"instanceTenancy"
{-# INLINEABLE cvInstanceTenancy #-}
{-# DEPRECATED instanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead"  #-}

-- | The IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvIpv6CidrBlock :: Lens.Lens' CreateVpc (Core.Maybe Core.Text)
cvIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE cvIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the address to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
--
-- /Note:/ Consider using 'ipv6CidrBlockNetworkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvIpv6CidrBlockNetworkBorderGroup :: Lens.Lens' CreateVpc (Core.Maybe Core.Text)
cvIpv6CidrBlockNetworkBorderGroup = Lens.field @"ipv6CidrBlockNetworkBorderGroup"
{-# INLINEABLE cvIpv6CidrBlockNetworkBorderGroup #-}
{-# DEPRECATED ipv6CidrBlockNetworkBorderGroup "Use generic-lens or generic-optics with 'ipv6CidrBlockNetworkBorderGroup' instead"  #-}

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvIpv6Pool :: Lens.Lens' CreateVpc (Core.Maybe Types.Ipv6PoolEc2Id)
cvIpv6Pool = Lens.field @"ipv6Pool"
{-# INLINEABLE cvIpv6Pool #-}
{-# DEPRECATED ipv6Pool "Use generic-lens or generic-optics with 'ipv6Pool' instead"  #-}

-- | The tags to assign to the VPC.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvTagSpecifications :: Lens.Lens' CreateVpc (Core.Maybe [Types.TagSpecification])
cvTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cvTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateVpc where
        toQuery CreateVpc{..}
          = Core.toQueryPair "Action" ("CreateVpc" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "CidrBlock" cidrBlock
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AmazonProvidedIpv6CidrBlock")
                amazonProvidedIpv6CidrBlock
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceTenancy")
                instanceTenancy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6CidrBlock")
                ipv6CidrBlock
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "Ipv6CidrBlockNetworkBorderGroup")
                ipv6CidrBlockNetworkBorderGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6Pool") ipv6Pool
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateVpc where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateVpc where
        type Rs CreateVpc = CreateVpcResponse
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
                 CreateVpcResponse' Core.<$>
                   (x Core..@? "vpc") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateVpcResponse' smart constructor.
data CreateVpcResponse = CreateVpcResponse'
  { vpc :: Core.Maybe Types.Vpc
    -- ^ Information about the VPC.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcResponse' value with any optional fields omitted.
mkCreateVpcResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpcResponse
mkCreateVpcResponse responseStatus
  = CreateVpcResponse'{vpc = Core.Nothing, responseStatus}

-- | Information about the VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsVpc :: Lens.Lens' CreateVpcResponse (Core.Maybe Types.Vpc)
cvrrsVpc = Lens.field @"vpc"
{-# INLINEABLE cvrrsVpc #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrrsResponseStatus :: Lens.Lens' CreateVpcResponse Core.Int
cvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
