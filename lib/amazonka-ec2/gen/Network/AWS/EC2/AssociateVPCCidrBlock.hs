{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateVPCCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your VPC. You can associate a secondary IPv4 CIDR block, an Amazon-provided IPv6 CIDR block, or an IPv6 CIDR block from an IPv6 address pool that you provisioned through bring your own IP addresses (<https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html BYOIP> ). The IPv6 CIDR block size is fixed at /56.
--
-- You must specify one of the following in the request: an IPv4 CIDR block, an IPv6 pool, or an Amazon-provided IPv6 CIDR block.
-- For more information about associating CIDR blocks with your VPC and applicable restrictions, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html#VPC_Sizing VPC and Subnet Sizing> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.AssociateVPCCidrBlock
  ( -- * Creating a request
    AssociateVPCCidrBlock (..),
    mkAssociateVPCCidrBlock,

    -- ** Request lenses
    avcbIPv6CidrBlock,
    avcbIPv6CidrBlockNetworkBorderGroup,
    avcbCidrBlock,
    avcbIPv6Pool,
    avcbAmazonProvidedIPv6CidrBlock,
    avcbVPCId,

    -- * Destructuring the response
    AssociateVPCCidrBlockResponse (..),
    mkAssociateVPCCidrBlockResponse,

    -- ** Response lenses
    avcbrsVPCId,
    avcbrsCidrBlockAssociation,
    avcbrsIPv6CidrBlockAssociation,
    avcbrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateVPCCidrBlock' smart constructor.
data AssociateVPCCidrBlock = AssociateVPCCidrBlock'
  { ipv6CidrBlock ::
      Lude.Maybe Lude.Text,
    ipv6CidrBlockNetworkBorderGroup ::
      Lude.Maybe Lude.Text,
    cidrBlock :: Lude.Maybe Lude.Text,
    ipv6Pool :: Lude.Maybe Lude.Text,
    amazonProvidedIPv6CidrBlock ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'AssociateVPCCidrBlock' with the minimum fields required to make a request.
--
-- * 'amazonProvidedIPv6CidrBlock' - Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
-- * 'cidrBlock' - An IPv4 CIDR block to associate with the VPC.
-- * 'ipv6CidrBlock' - An IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
-- * 'ipv6CidrBlockNetworkBorderGroup' - The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
-- You can have one IPv6 CIDR block association per network border group.
-- * 'ipv6Pool' - The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
-- * 'vpcId' - The ID of the VPC.
mkAssociateVPCCidrBlock ::
  -- | 'vpcId'
  Lude.Text ->
  AssociateVPCCidrBlock
mkAssociateVPCCidrBlock pVPCId_ =
  AssociateVPCCidrBlock'
    { ipv6CidrBlock = Lude.Nothing,
      ipv6CidrBlockNetworkBorderGroup = Lude.Nothing,
      cidrBlock = Lude.Nothing,
      ipv6Pool = Lude.Nothing,
      amazonProvidedIPv6CidrBlock = Lude.Nothing,
      vpcId = pVPCId_
    }

-- | An IPv6 CIDR block from the IPv6 address pool. You must also specify @Ipv6Pool@ in the request.
--
-- To let Amazon choose the IPv6 CIDR block for you, omit this parameter.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbIPv6CidrBlock :: Lens.Lens' AssociateVPCCidrBlock (Lude.Maybe Lude.Text)
avcbIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: AssociateVPCCidrBlock -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: AssociateVPCCidrBlock)
{-# DEPRECATED avcbIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | The name of the location from which we advertise the IPV6 CIDR block. Use this parameter to limit the CIDR block to this location.
--
-- You must set @AmazonProvidedIpv6CidrBlock@ to @true@ to use this parameter.
-- You can have one IPv6 CIDR block association per network border group.
--
-- /Note:/ Consider using 'ipv6CidrBlockNetworkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbIPv6CidrBlockNetworkBorderGroup :: Lens.Lens' AssociateVPCCidrBlock (Lude.Maybe Lude.Text)
avcbIPv6CidrBlockNetworkBorderGroup = Lens.lens (ipv6CidrBlockNetworkBorderGroup :: AssociateVPCCidrBlock -> Lude.Maybe Lude.Text) (\s a -> s {ipv6CidrBlockNetworkBorderGroup = a} :: AssociateVPCCidrBlock)
{-# DEPRECATED avcbIPv6CidrBlockNetworkBorderGroup "Use generic-lens or generic-optics with 'ipv6CidrBlockNetworkBorderGroup' instead." #-}

-- | An IPv4 CIDR block to associate with the VPC.
--
-- /Note:/ Consider using 'cidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbCidrBlock :: Lens.Lens' AssociateVPCCidrBlock (Lude.Maybe Lude.Text)
avcbCidrBlock = Lens.lens (cidrBlock :: AssociateVPCCidrBlock -> Lude.Maybe Lude.Text) (\s a -> s {cidrBlock = a} :: AssociateVPCCidrBlock)
{-# DEPRECATED avcbCidrBlock "Use generic-lens or generic-optics with 'cidrBlock' instead." #-}

-- | The ID of an IPv6 address pool from which to allocate the IPv6 CIDR block.
--
-- /Note:/ Consider using 'ipv6Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbIPv6Pool :: Lens.Lens' AssociateVPCCidrBlock (Lude.Maybe Lude.Text)
avcbIPv6Pool = Lens.lens (ipv6Pool :: AssociateVPCCidrBlock -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Pool = a} :: AssociateVPCCidrBlock)
{-# DEPRECATED avcbIPv6Pool "Use generic-lens or generic-optics with 'ipv6Pool' instead." #-}

-- | Requests an Amazon-provided IPv6 CIDR block with a /56 prefix length for the VPC. You cannot specify the range of IPv6 addresses, or the size of the CIDR block.
--
-- /Note:/ Consider using 'amazonProvidedIPv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbAmazonProvidedIPv6CidrBlock :: Lens.Lens' AssociateVPCCidrBlock (Lude.Maybe Lude.Bool)
avcbAmazonProvidedIPv6CidrBlock = Lens.lens (amazonProvidedIPv6CidrBlock :: AssociateVPCCidrBlock -> Lude.Maybe Lude.Bool) (\s a -> s {amazonProvidedIPv6CidrBlock = a} :: AssociateVPCCidrBlock)
{-# DEPRECATED avcbAmazonProvidedIPv6CidrBlock "Use generic-lens or generic-optics with 'amazonProvidedIPv6CidrBlock' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbVPCId :: Lens.Lens' AssociateVPCCidrBlock Lude.Text
avcbVPCId = Lens.lens (vpcId :: AssociateVPCCidrBlock -> Lude.Text) (\s a -> s {vpcId = a} :: AssociateVPCCidrBlock)
{-# DEPRECATED avcbVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest AssociateVPCCidrBlock where
  type Rs AssociateVPCCidrBlock = AssociateVPCCidrBlockResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateVPCCidrBlockResponse'
            Lude.<$> (x Lude..@? "vpcId")
            Lude.<*> (x Lude..@? "cidrBlockAssociation")
            Lude.<*> (x Lude..@? "ipv6CidrBlockAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateVPCCidrBlock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateVPCCidrBlock where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateVPCCidrBlock where
  toQuery AssociateVPCCidrBlock' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssociateVpcCidrBlock" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Ipv6CidrBlock" Lude.=: ipv6CidrBlock,
        "Ipv6CidrBlockNetworkBorderGroup"
          Lude.=: ipv6CidrBlockNetworkBorderGroup,
        "CidrBlock" Lude.=: cidrBlock,
        "Ipv6Pool" Lude.=: ipv6Pool,
        "AmazonProvidedIpv6CidrBlock" Lude.=: amazonProvidedIPv6CidrBlock,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkAssociateVPCCidrBlockResponse' smart constructor.
data AssociateVPCCidrBlockResponse = AssociateVPCCidrBlockResponse'
  { vpcId ::
      Lude.Maybe Lude.Text,
    cidrBlockAssociation ::
      Lude.Maybe
        VPCCidrBlockAssociation,
    ipv6CidrBlockAssociation ::
      Lude.Maybe
        VPCIPv6CidrBlockAssociation,
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

-- | Creates a value of 'AssociateVPCCidrBlockResponse' with the minimum fields required to make a request.
--
-- * 'cidrBlockAssociation' - Information about the IPv4 CIDR block association.
-- * 'ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
-- * 'responseStatus' - The response status code.
-- * 'vpcId' - The ID of the VPC.
mkAssociateVPCCidrBlockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateVPCCidrBlockResponse
mkAssociateVPCCidrBlockResponse pResponseStatus_ =
  AssociateVPCCidrBlockResponse'
    { vpcId = Lude.Nothing,
      cidrBlockAssociation = Lude.Nothing,
      ipv6CidrBlockAssociation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrsVPCId :: Lens.Lens' AssociateVPCCidrBlockResponse (Lude.Maybe Lude.Text)
avcbrsVPCId = Lens.lens (vpcId :: AssociateVPCCidrBlockResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: AssociateVPCCidrBlockResponse)
{-# DEPRECATED avcbrsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Information about the IPv4 CIDR block association.
--
-- /Note:/ Consider using 'cidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrsCidrBlockAssociation :: Lens.Lens' AssociateVPCCidrBlockResponse (Lude.Maybe VPCCidrBlockAssociation)
avcbrsCidrBlockAssociation = Lens.lens (cidrBlockAssociation :: AssociateVPCCidrBlockResponse -> Lude.Maybe VPCCidrBlockAssociation) (\s a -> s {cidrBlockAssociation = a} :: AssociateVPCCidrBlockResponse)
{-# DEPRECATED avcbrsCidrBlockAssociation "Use generic-lens or generic-optics with 'cidrBlockAssociation' instead." #-}

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrsIPv6CidrBlockAssociation :: Lens.Lens' AssociateVPCCidrBlockResponse (Lude.Maybe VPCIPv6CidrBlockAssociation)
avcbrsIPv6CidrBlockAssociation = Lens.lens (ipv6CidrBlockAssociation :: AssociateVPCCidrBlockResponse -> Lude.Maybe VPCIPv6CidrBlockAssociation) (\s a -> s {ipv6CidrBlockAssociation = a} :: AssociateVPCCidrBlockResponse)
{-# DEPRECATED avcbrsIPv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcbrsResponseStatus :: Lens.Lens' AssociateVPCCidrBlockResponse Lude.Int
avcbrsResponseStatus = Lens.lens (responseStatus :: AssociateVPCCidrBlockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateVPCCidrBlockResponse)
{-# DEPRECATED avcbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
