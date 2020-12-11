{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cssIPv6CidrBlock,
    cssAvailabilityZoneId,
    cssOutpostARN,
    cssTagSpecifications,
    cssAvailabilityZone,
    cssDryRun,
    cssCidrBlock,
    cssVPCId,

    -- * Destructuring the response
    CreateSubnetResponse (..),
    mkCreateSubnetResponse,

    -- ** Response lenses
    crersSubnet,
    crersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSubnet' smart constructor.
data CreateSubnet = CreateSubnet'
  { ipv6CidrBlock ::
      Lude.Maybe Lude.Text,
    availabilityZoneId :: Lude.Maybe Lude.Text,
    outpostARN :: Lude.Maybe Lude.Text,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    availabilityZone :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    cidrBlock :: Lude.Text,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSubnet' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone or Local Zone for the subnet.
--
-- Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet.
-- To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
-- To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
-- * 'availabilityZoneId' - The AZ ID or the Local Zone ID of the subnet.
-- * 'cidrBlock' - The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'ipv6CidrBlock' - The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
-- * 'tagSpecifications' - The tags to assign to the subnet.
-- * 'vpcId' - The ID of the VPC.
mkCreateSubnet ::
  -- | 'cidrBlock'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  CreateSubnet
mkCreateSubnet pCidrBlock_ pVPCId_ =
  CreateSubnet'
    { ipv6CidrBlock = Lude.Nothing,
      availabilityZoneId = Lude.Nothing,
      outpostARN = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      dryRun = Lude.Nothing,
      cidrBlock = pCidrBlock_,
      vpcId = pVPCId_
    }

-- | The IPv6 network range for the subnet, in CIDR notation. The subnet size must use a /64 prefix length.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssIPv6CidrBlock :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
cssIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: CreateSubnet)
{-# DEPRECATED cssIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | The AZ ID or the Local Zone ID of the subnet.
--
-- /Note:/ Consider using 'availabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAvailabilityZoneId :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
cssAvailabilityZoneId = Lens.lens (availabilityZoneId :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZoneId = a} :: CreateSubnet)
{-# DEPRECATED cssAvailabilityZoneId "Use generic-lens or generic-optics with 'availabilityZoneId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost. If you specify an Outpost ARN, you must also specify the Availability Zone of the Outpost subnet.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssOutpostARN :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
cssOutpostARN = Lens.lens (outpostARN :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: CreateSubnet)
{-# DEPRECATED cssOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | The tags to assign to the subnet.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssTagSpecifications :: Lens.Lens' CreateSubnet (Lude.Maybe [TagSpecification])
cssTagSpecifications = Lens.lens (tagSpecifications :: CreateSubnet -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateSubnet)
{-# DEPRECATED cssTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Availability Zone or Local Zone for the subnet.
--
-- Default: AWS selects one for you. If you create more than one subnet in your VPC, we do not necessarily select a different zone for each subnet.
-- To create a subnet in a Local Zone, set this value to the Local Zone ID, for example @us-west-2-lax-1a@ . For information about the Regions that support Local Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html#concepts-available-regions Available Regions> in the /Amazon Elastic Compute Cloud User Guide/ .
-- To create a subnet in an Outpost, set this value to the Availability Zone for the Outpost and specify the Outpost ARN.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssAvailabilityZone :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Text)
cssAvailabilityZone = Lens.lens (availabilityZone :: CreateSubnet -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateSubnet)
{-# DEPRECATED cssAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssDryRun :: Lens.Lens' CreateSubnet (Lude.Maybe Lude.Bool)
cssDryRun = Lens.lens (dryRun :: CreateSubnet -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateSubnet)
{-# DEPRECATED cssDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IPv4 network range for the subnet, in CIDR notation. For example, @10.0.0.0/24@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssCidrBlock :: Lens.Lens' CreateSubnet Lude.Text
cssCidrBlock = Lens.lens (cidrBlock :: CreateSubnet -> Lude.Text) (\s a -> s {cidrBlock = a} :: CreateSubnet)
{-# DEPRECATED cssCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssVPCId :: Lens.Lens' CreateSubnet Lude.Text
cssVPCId = Lens.lens (vpcId :: CreateSubnet -> Lude.Text) (\s a -> s {vpcId = a} :: CreateSubnet)
{-# DEPRECATED cssVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

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
        "OutpostArn" Lude.=: outpostARN,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "AvailabilityZone" Lude.=: availabilityZone,
        "DryRun" Lude.=: dryRun,
        "CidrBlock" Lude.=: cidrBlock,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkCreateSubnetResponse' smart constructor.
data CreateSubnetResponse = CreateSubnetResponse'
  { subnet ::
      Lude.Maybe Subnet,
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

-- | Creates a value of 'CreateSubnetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'subnet' - Information about the subnet.
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
crersSubnet :: Lens.Lens' CreateSubnetResponse (Lude.Maybe Subnet)
crersSubnet = Lens.lens (subnet :: CreateSubnetResponse -> Lude.Maybe Subnet) (\s a -> s {subnet = a} :: CreateSubnetResponse)
{-# DEPRECATED crersSubnet "Use generic-lens or generic-optics with 'subnet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crersResponseStatus :: Lens.Lens' CreateSubnetResponse Lude.Int
crersResponseStatus = Lens.lens (responseStatus :: CreateSubnetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSubnetResponse)
{-# DEPRECATED crersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
