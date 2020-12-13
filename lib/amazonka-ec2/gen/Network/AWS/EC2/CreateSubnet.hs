{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateSubnet (..),
    mkCreateSubnet,

    -- ** Request lenses
    csgIPv6CidrBlock,
    csgAvailabilityZoneId,
    csgVPCId,
    csgOutpostARN,
    csgTagSpecifications,
    csgAvailabilityZone,
    csgCidrBlock,
    csgDryRun,

    -- * Destructuring the response
    CreateSubnetResponse (..),
    mkCreateSubnetResponse,

    -- ** Response lenses
    csfrsSubnet,
    csfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSubnet' smart constructor.
data CreateSubnet = CreateSubnet'
  { -- | The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
    ipv6CidrBlock :: Lude.Maybe Lude.Text,
    -- | The AZ ID or the Local Zone ID of the subnet.
    availabilityZoneId :: Lude.Maybe Lude.Text,
    -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | The tags to assign to the subnet.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The Availability Zone or Local Zone for the subnet.
    --
    -- Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet.
    -- To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
    -- To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
    cidrBlock :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubnet' with the minimum fields required to make a request.
--
-- * 'ipv6CidrBlock' - The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
-- * 'availabilityZoneId' - The AZ ID or the Local Zone ID of the subnet.
-- * 'vpcId' - The ID of the VPC.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
-- * 'tagSpecifications' - The tags to assign to the subnet.
-- * 'availabilityZone' - The Availability Zone or Local Zone for the subnet.
--
-- Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet.
-- To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
-- To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
-- * 'cidrBlock' - The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateSubnet ::
  -- | 'vpcId'
  Lude.Text ->
  -- | 'cidrBlock'
  Lude.Text ->
  CreateSubnet
mkCreateSubnet pVPCId_ pCidrBlock_ =
  CreateSubnet'
    { ipv6CidrBlock = Lude.Nothing,
      availabilityZoneId = Lude.Nothing,
      vpcId = pVPCId_,
      outpostARN = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      cidrBlock = pCidrBlock_,
      dryRun = Lude.Nothing
    }

-- | The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgIPv6CidrBlock :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
csgIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: CreateSubnet)
{-# DEPRECATED csgIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | The AZ ID or the Local Zone ID of the subnet.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgAvailabilityZoneId :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
csgAvailabilityZoneId = Lens.lens (availabilityZoneId :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: CreateSubnet)
{-# DEPRECATED csgAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgVPCId :: Lens.Lens' CreateSubnet Lude.Text
csgVPCId = Lens.lens (vpcId :: CreateSubnet -> Lude.Text) (\s a -> s {vpcId = a} :: CreateSubnet)
{-# DEPRECATED csgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgOutpostARN :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
csgOutpostARN = Lens.lens (outpostARN :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: CreateSubnet)
{-# DEPRECATED csgOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | The tags to assign to the subnet.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgTagSpecifications :: Lens.Lens' CreateSubnet (Lude.Maybe [TagSpecification])
csgTagSpecifications = Lens.lens (tagSpecifications :: CreateSubnet -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateSubnet)
{-# DEPRECATED csgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Availability Zone or Local Zone for the subnet.
--
-- Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet.
-- To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
-- To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgAvailabilityZone :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
csgAvailabilityZone = Lens.lens (availabilityZone :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateSubnet)
{-# DEPRECATED csgAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgCidrBlock :: Lens.Lens' CreateSubnet Lude.Text
csgCidrBlock = Lens.lens (cidrBlock :: CreateSubnet -> Lude.Text) (\s a -> s {cidrBlock = a} :: CreateSubnet)
{-# DEPRECATED csgCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDryRun :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Bool)
csgDryRun = Lens.lens (dryRun :: CreateSubnet -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSubnet)
{-# DEPRECATED csgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateSubnet where
  type Rs CreateSubnet = CreateSubnetResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateSubnetResponse'
            Lude.<$> (x Lude..@? "subnet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSubnet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSubnet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSubnet where
  toQuery CreateSubnet' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateSubnet" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Ipv6CidrBlock" Lude.=: ipv6CidrBlock,
        "AvailabilityZoneId" Lude.=: availabilityZoneId,
        "VpcId" Lude.=: vpcId,
        "OutpostArn" Lude.=: outpostARN,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "AvailabilityZone" Lude.=: availabilityZone,
        "CidrBlock" Lude.=: cidrBlock,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateSubnetResponse' smart constructor.
data CreateSubnetResponse = CreateSubnetResponse'
  { -- | Information about the subnet.
    subnet :: Lude.Maybe Subnet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubnetResponse' with the minimum fields required to make a request.
--
-- * 'subnet' - Information about the subnet.
-- * 'responseStatus' - The response status code.
mkCreateSubnetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSubnetResponse
mkCreateSubnetResponse pResponseStatus_ =
  CreateSubnetResponse'
    { subnet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the subnet.
--
-- /Note:/ Consider using 'subnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsSubnet :: Lens.Lens' CreateSubnetResponse (Lude.Maybe Subnet)
csfrsSubnet = Lens.lens (subnet :: CreateSubnetResponse -> Lude.Maybe Subnet) (\s a -> s {subnet = a} :: CreateSubnetResponse)
{-# DEPRECATED csfrsSubnet "Use generic-lens or generic-optics with 'subnet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsResponseStatus :: Lens.Lens' CreateSubnetResponse Lude.Int
csfrsResponseStatus = Lens.lens (responseStatus :: CreateSubnetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSubnetResponse)
{-# DEPRECATED csfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
