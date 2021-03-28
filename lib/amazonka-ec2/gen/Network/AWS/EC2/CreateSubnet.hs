{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subnet in a specified VPC.
--
-- You must specify an IPv4 CIDR block for the subnet. After you create a subnet, you can't change its CIDR block. The allowed block size is between a /16 netmask (65,536 IP addresses) and /28 netmask (16 IP addresses). The CIDR block must not overlap with the CIDR block of an existing subnet in the VPC.
-- If you've associated an IPv6 CIDR block with your VPC, you can create a subnet with an IPv6 CIDR block that uses a /64 prefix length. 
-- /Important:/ AWS reserves both the first four and the last IPv4 address in each subnet's CIDR block. They're not available for use.
-- If you add more than one subnet to a VPC, they're set up in a star topology with a logical router in the middle.
-- When you stop an instance in a subnet, it retains its private IPv4 address. It's therefore possible to have a subnet with no running instances (they're all stopped), but no remaining IP addresses available.
-- For more information about subnets, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and Subnets> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateSubnet
    (
    -- * Creating a request
      CreateSubnet (..)
    , mkCreateSubnet
    -- ** Request lenses
    , csgCidrBlock
    , csgVpcId
    , csgAvailabilityZone
    , csgAvailabilityZoneId
    , csgDryRun
    , csgIpv6CidrBlock
    , csgOutpostArn
    , csgTagSpecifications

    -- * Destructuring the response
    , CreateSubnetResponse (..)
    , mkCreateSubnetResponse
    -- ** Response lenses
    , csrgrsSubnet
    , csrgrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSubnet' smart constructor.
data CreateSubnet = CreateSubnet'
  { cidrBlock :: Core.Text
    -- ^ The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone or Local Zone for the subnet.
--
-- Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet.
-- To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
-- To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
  , availabilityZoneId :: Core.Maybe Core.Text
    -- ^ The AZ ID or the Local Zone ID of the subnet.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , ipv6CidrBlock :: Core.Maybe Core.Text
    -- ^ The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
  , outpostArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubnet' value with any optional fields omitted.
mkCreateSubnet
    :: Core.Text -- ^ 'cidrBlock'
    -> Types.VpcId -- ^ 'vpcId'
    -> CreateSubnet
mkCreateSubnet cidrBlock vpcId
  = CreateSubnet'{cidrBlock, vpcId, availabilityZone = Core.Nothing,
                  availabilityZoneId = Core.Nothing, dryRun = Core.Nothing,
                  ipv6CidrBlock = Core.Nothing, outpostArn = Core.Nothing,
                  tagSpecifications = Core.Nothing}

-- | The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgCidrBlock :: Lens.Lens' CreateSubnet Core.Text
csgCidrBlock = Lens.field @"cidrBlock"
{-# INLINEABLE csgCidrBlock #-}
{-# DEPRECATED cidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgVpcId :: Lens.Lens' CreateSubnet Types.VpcId
csgVpcId = Lens.field @"vpcId"
{-# INLINEABLE csgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The Availability Zone or Local Zone for the subnet.
--
-- Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet.
-- To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
-- To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgAvailabilityZone :: Lens.Lens' CreateSubnet (Core.Maybe Core.Text)
csgAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE csgAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The AZ ID or the Local Zone ID of the subnet.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgAvailabilityZoneId :: Lens.Lens' CreateSubnet (Core.Maybe Core.Text)
csgAvailabilityZoneId = Lens.field @"availabilityZoneId"
{-# INLINEABLE csgAvailabilityZoneId #-}
{-# DEPRECATED availabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDryRun :: Lens.Lens' CreateSubnet (Core.Maybe Core.Bool)
csgDryRun = Lens.field @"dryRun"
{-# INLINEABLE csgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgIpv6CidrBlock :: Lens.Lens' CreateSubnet (Core.Maybe Core.Text)
csgIpv6CidrBlock = Lens.field @"ipv6CidrBlock"
{-# INLINEABLE csgIpv6CidrBlock #-}
{-# DEPRECATED ipv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgOutpostArn :: Lens.Lens' CreateSubnet (Core.Maybe Core.Text)
csgOutpostArn = Lens.field @"outpostArn"
{-# INLINEABLE csgOutpostArn #-}
{-# DEPRECATED outpostArn "Use generic-lens or generic-optics with 'outpostArn' instead"  #-}

-- | The tags to assign to the subnet.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgTagSpecifications :: Lens.Lens' CreateSubnet (Core.Maybe [Types.TagSpecification])
csgTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE csgTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateSubnet where
        toQuery CreateSubnet{..}
          = Core.toQueryPair "Action" ("CreateSubnet" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "CidrBlock" cidrBlock
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZoneId")
                availabilityZoneId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6CidrBlock")
                ipv6CidrBlock
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OutpostArn") outpostArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateSubnet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateSubnet where
        type Rs CreateSubnet = CreateSubnetResponse
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
                 CreateSubnetResponse' Core.<$>
                   (x Core..@? "subnet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSubnetResponse' smart constructor.
data CreateSubnetResponse = CreateSubnetResponse'
  { subnet :: Core.Maybe Types.Subnet
    -- ^ Information about the subnet.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubnetResponse' value with any optional fields omitted.
mkCreateSubnetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSubnetResponse
mkCreateSubnetResponse responseStatus
  = CreateSubnetResponse'{subnet = Core.Nothing, responseStatus}

-- | Information about the subnet.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrgrsSubnet :: Lens.Lens' CreateSubnetResponse (Core.Maybe Types.Subnet)
csrgrsSubnet = Lens.field @"subnet"
{-# INLINEABLE csrgrsSubnet #-}
{-# DEPRECATED subnet "Use generic-lens or generic-optics with 'subnet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrgrsResponseStatus :: Lens.Lens' CreateSubnetResponse Core.Int
csrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
