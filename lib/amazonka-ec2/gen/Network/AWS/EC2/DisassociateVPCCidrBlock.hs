{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateVPCCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a VPC. To disassociate the CIDR block, you must specify its association ID. You can get the association ID by using 'DescribeVpcs' . You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it.
--
-- You cannot disassociate the CIDR block with which you originally created the VPC (the primary CIDR block).
module Network.AWS.EC2.DisassociateVPCCidrBlock
  ( -- * Creating a request
    DisassociateVPCCidrBlock (..),
    mkDisassociateVPCCidrBlock,

    -- ** Request lenses
    dvcbAssociationId,

    -- * Destructuring the response
    DisassociateVPCCidrBlockResponse (..),
    mkDisassociateVPCCidrBlockResponse,

    -- ** Response lenses
    dvcbrsVPCId,
    dvcbrsCidrBlockAssociation,
    dvcbrsIPv6CidrBlockAssociation,
    dvcbrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateVPCCidrBlock' smart constructor.
newtype DisassociateVPCCidrBlock = DisassociateVPCCidrBlock'
  { -- | The association ID for the CIDR block.
    associationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateVPCCidrBlock' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for the CIDR block.
mkDisassociateVPCCidrBlock ::
  -- | 'associationId'
  Lude.Text ->
  DisassociateVPCCidrBlock
mkDisassociateVPCCidrBlock pAssociationId_ =
  DisassociateVPCCidrBlock' {associationId = pAssociationId_}

-- | The association ID for the CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbAssociationId :: Lens.Lens' DisassociateVPCCidrBlock Lude.Text
dvcbAssociationId = Lens.lens (associationId :: DisassociateVPCCidrBlock -> Lude.Text) (\s a -> s {associationId = a} :: DisassociateVPCCidrBlock)
{-# DEPRECATED dvcbAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Lude.AWSRequest DisassociateVPCCidrBlock where
  type Rs DisassociateVPCCidrBlock = DisassociateVPCCidrBlockResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateVPCCidrBlockResponse'
            Lude.<$> (x Lude..@? "vpcId")
            Lude.<*> (x Lude..@? "cidrBlockAssociation")
            Lude.<*> (x Lude..@? "ipv6CidrBlockAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateVPCCidrBlock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateVPCCidrBlock where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateVPCCidrBlock where
  toQuery DisassociateVPCCidrBlock' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisassociateVpcCidrBlock" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AssociationId" Lude.=: associationId
      ]

-- | /See:/ 'mkDisassociateVPCCidrBlockResponse' smart constructor.
data DisassociateVPCCidrBlockResponse = DisassociateVPCCidrBlockResponse'
  { -- | The ID of the VPC.
    vpcId :: Lude.Maybe Lude.Text,
    -- | Information about the IPv4 CIDR block association.
    cidrBlockAssociation :: Lude.Maybe VPCCidrBlockAssociation,
    -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Lude.Maybe VPCIPv6CidrBlockAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateVPCCidrBlockResponse' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
-- * 'cidrBlockAssociation' - Information about the IPv4 CIDR block association.
-- * 'ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
-- * 'responseStatus' - The response status code.
mkDisassociateVPCCidrBlockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateVPCCidrBlockResponse
mkDisassociateVPCCidrBlockResponse pResponseStatus_ =
  DisassociateVPCCidrBlockResponse'
    { vpcId = Lude.Nothing,
      cidrBlockAssociation = Lude.Nothing,
      ipv6CidrBlockAssociation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrsVPCId :: Lens.Lens' DisassociateVPCCidrBlockResponse (Lude.Maybe Lude.Text)
dvcbrsVPCId = Lens.lens (vpcId :: DisassociateVPCCidrBlockResponse -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DisassociateVPCCidrBlockResponse)
{-# DEPRECATED dvcbrsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Information about the IPv4 CIDR block association.
--
-- /Note:/ Consider using 'cidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrsCidrBlockAssociation :: Lens.Lens' DisassociateVPCCidrBlockResponse (Lude.Maybe VPCCidrBlockAssociation)
dvcbrsCidrBlockAssociation = Lens.lens (cidrBlockAssociation :: DisassociateVPCCidrBlockResponse -> Lude.Maybe VPCCidrBlockAssociation) (\s a -> s {cidrBlockAssociation = a} :: DisassociateVPCCidrBlockResponse)
{-# DEPRECATED dvcbrsCidrBlockAssociation "Use generic-lens or generic-optics with 'cidrBlockAssociation' instead." #-}

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrsIPv6CidrBlockAssociation :: Lens.Lens' DisassociateVPCCidrBlockResponse (Lude.Maybe VPCIPv6CidrBlockAssociation)
dvcbrsIPv6CidrBlockAssociation = Lens.lens (ipv6CidrBlockAssociation :: DisassociateVPCCidrBlockResponse -> Lude.Maybe VPCIPv6CidrBlockAssociation) (\s a -> s {ipv6CidrBlockAssociation = a} :: DisassociateVPCCidrBlockResponse)
{-# DEPRECATED dvcbrsIPv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcbrsResponseStatus :: Lens.Lens' DisassociateVPCCidrBlockResponse Lude.Int
dvcbrsResponseStatus = Lens.lens (responseStatus :: DisassociateVPCCidrBlockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateVPCCidrBlockResponse)
{-# DEPRECATED dvcbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
