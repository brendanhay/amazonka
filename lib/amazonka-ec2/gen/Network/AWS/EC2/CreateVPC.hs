{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPC
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
module Network.AWS.EC2.CreateVPC
  ( -- * Creating a request
    CreateVPC (..),
    mkCreateVPC,

    -- ** Request lenses
    cIPv6CidrBlock,
    cIPv6CidrBlockNetworkBorderGroup,
    cTagSpecifications,
    cCidrBlock,
    cIPv6Pool,
    cAmazonProvidedIPv6CidrBlock,
    cInstanceTenancy,
    cDryRun,

    -- * Destructuring the response
    CreateVPCResponse (..),
    mkCreateVPCResponse,

    -- ** Response lenses
    cvrsVPC,
    cvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVPC' smart constructor.
data CreateVPC = CreateVPC'
  { -- | The IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
    --
    -- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
    ipv6CidrBlock :: Lude.Maybe Lude.Text,
    -- | The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the address to this location.
    --
    -- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
    ipv6CidrBlockNetworkBorderGroup :: Lude.Maybe Lude.Text,
    -- | The tags to assign to the VPC.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | The IPv4 network range for the VPC, in CIDR notation. For example, @10.0.0.0/16@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
    cidrBlock :: Lude.Text,
    -- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
    ipv6Pool :: Lude.Maybe Lude.Text,
    -- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IP addresses, or the size of the CIDR block.
    amazonProvidedIPv6CidrBlock :: Lude.Maybe Lude.Bool,
    -- | The tenancy options for instances launched into the VPC. For @default@ , instances are launched with shared tenancy by default. You can launch instances with any tenancy into a shared tenancy VPC. For @dedicated@ , instances are launched as dedicated tenancy instances by default. You can only launch instances with a tenancy of @dedicated@ or @host@ into a dedicated tenancy VPC.
    --
    -- __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only.
    -- Default: @default@
    instanceTenancy :: Lude.Maybe Tenancy,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPC' with the minimum fields required to make a request.
--
-- * 'ipv6CidrBlock' - The IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
-- * 'ipv6CidrBlockNetworkBorderGroup' - The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the address to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
-- * 'tagSpecifications' - The tags to assign to the VPC.
-- * 'cidrBlock' - The IPv4 network range for the VPC, in CIDR notation. For example, @10.0.0.0/16@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
-- * 'ipv6Pool' - The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
-- * 'amazonProvidedIPv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IP addresses, or the size of the CIDR block.
-- * 'instanceTenancy' - The tenancy options for instances launched into the VPC. For @default@ , instances are launched with shared tenancy by default. You can launch instances with any tenancy into a shared tenancy VPC. For @dedicated@ , instances are launched as dedicated tenancy instances by default. You can only launch instances with a tenancy of @dedicated@ or @host@ into a dedicated tenancy VPC.
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only.
-- Default: @default@
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateVPC ::
  -- | 'cidrBlock'
  Lude.Text ->
  CreateVPC
mkCreateVPC pCidrBlock_ =
  CreateVPC'
    { ipv6CidrBlock = Lude.Nothing,
      ipv6CidrBlockNetworkBorderGroup = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      cidrBlock = pCidrBlock_,
      ipv6Pool = Lude.Nothing,
      amazonProvidedIPv6CidrBlock = Lude.Nothing,
      instanceTenancy = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIPv6CidrBlock :: Lens.Lens' CreateVPC (Lude.Maybe Lude.Text)
cIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: CreateVPC -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: CreateVPC)
{-# DEPRECATED cIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the address to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
--
-- /Note:/ Consider using 'ipv6CidrBlockNetworkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIPv6CidrBlockNetworkBorderGroup :: Lens.Lens' CreateVPC (Lude.Maybe Lude.Text)
cIPv6CidrBlockNetworkBorderGroup = Lens.lens (ipv6CidrBlockNetworkBorderGroup :: CreateVPC -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlockNetworkBorderGroup = a} :: CreateVPC)
{-# DEPRECATED cIPv6CidrBlockNetworkBorderGroup "Use generic-lens or generic-optics with 'ipv6CidrBlockNetworkBorderGroup' instead." #-}

-- | The tags to assign to the VPC.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTagSpecifications :: Lens.Lens' CreateVPC (Lude.Maybe [TagSpecification])
cTagSpecifications = Lens.lens (tagSpecifications :: CreateVPC -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateVPC)
{-# DEPRECATED cTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The IPv4 network range for the VPC, in CIDR notation. For example, @10.0.0.0/16@ . We modify the specified CIDR block to its canonical form; for example, if you specify @100.68.0.18/18@ , we modify it to @100.68.0.0/18@ .
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCidrBlock :: Lens.Lens' CreateVPC Lude.Text
cCidrBlock = Lens.lens (cidrBlock :: CreateVPC -> Lude.Text) (\s a -> s {cidrBlock = a} :: CreateVPC)
{-# DEPRECATED cCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIPv6Pool :: Lens.Lens' CreateVPC (Lude.Maybe Lude.Text)
cIPv6Pool = Lens.lens (ipv6Pool :: CreateVPC -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Pool = a} :: CreateVPC)
{-# DEPRECATED cIPv6Pool "Use generic-lens or generic-optics with 'ipv6Pool' instead." #-}

-- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IP addresses, or the size of the CIDR block.
--
-- /Note:/ Consider using 'amazonProvidedIPv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAmazonProvidedIPv6CidrBlock :: Lens.Lens' CreateVPC (Lude.Maybe Lude.Bool)
cAmazonProvidedIPv6CidrBlock = Lens.lens (amazonProvidedIPv6CidrBlock :: CreateVPC -> Lude.Maybe Lude.Bool) (\s a -> s {amazonProvidedIPv6CidrBlock = a} :: CreateVPC)
{-# DEPRECATED cAmazonProvidedIPv6CidrBlock "Use generic-lens or generic-optics with 'amazonProvidedIPv6CidrBlock' instead." #-}

-- | The tenancy options for instances launched into the VPC. For @default@ , instances are launched with shared tenancy by default. You can launch instances with any tenancy into a shared tenancy VPC. For @dedicated@ , instances are launched as dedicated tenancy instances by default. You can only launch instances with a tenancy of @dedicated@ or @host@ into a dedicated tenancy VPC.
--
-- __Important:__ The @host@ value cannot be used with this parameter. Use the @default@ or @dedicated@ values only.
-- Default: @default@
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInstanceTenancy :: Lens.Lens' CreateVPC (Lude.Maybe Tenancy)
cInstanceTenancy = Lens.lens (instanceTenancy :: CreateVPC -> Lude.Maybe Tenancy) (\s a -> s {instanceTenancy = a} :: CreateVPC)
{-# DEPRECATED cInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDryRun :: Lens.Lens' CreateVPC (Lude.Maybe Lude.Bool)
cDryRun = Lens.lens (dryRun :: CreateVPC -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVPC)
{-# DEPRECATED cDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateVPC where
  type Rs CreateVPC = CreateVPCResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateVPCResponse'
            Lude.<$> (x Lude..@? "vpc") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPC where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPC where
  toQuery CreateVPC' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateVpc" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Ipv6CidrBlock" Lude.=: ipv6CidrBlock,
        "Ipv6CidrBlockNetworkBorderGroup"
          Lude.=: ipv6CidrBlockNetworkBorderGroup,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "CidrBlock" Lude.=: cidrBlock,
        "Ipv6Pool" Lude.=: ipv6Pool,
        "AmazonProvidedIpv6CidrBlock" Lude.=: amazonProvidedIPv6CidrBlock,
        "InstanceTenancy" Lude.=: instanceTenancy,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateVPCResponse' smart constructor.
data CreateVPCResponse = CreateVPCResponse'
  { -- | Information about the VPC.
    vpc :: Lude.Maybe VPC,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCResponse' with the minimum fields required to make a request.
--
-- * 'vpc' - Information about the VPC.
-- * 'responseStatus' - The response status code.
mkCreateVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCResponse
mkCreateVPCResponse pResponseStatus_ =
  CreateVPCResponse'
    { vpc = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsVPC :: Lens.Lens' CreateVPCResponse (Lude.Maybe VPC)
cvrsVPC = Lens.lens (vpc :: CreateVPCResponse -> Lude.Maybe VPC) (\s a -> s {vpc = a} :: CreateVPCResponse)
{-# DEPRECATED cvrsVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrsResponseStatus :: Lens.Lens' CreateVPCResponse Lude.Int
cvrsResponseStatus = Lens.lens (responseStatus :: CreateVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCResponse)
{-# DEPRECATED cvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
