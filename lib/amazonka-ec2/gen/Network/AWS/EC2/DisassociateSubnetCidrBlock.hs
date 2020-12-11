{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DisassociateSubnetCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a CIDR block from a subnet. Currently, you can disassociate an IPv6 CIDR block only. You must detach or delete all gateways and resources that are associated with the CIDR block before you can disassociate it.
module Network.AWS.EC2.DisassociateSubnetCidrBlock
  ( -- * Creating a request
    DisassociateSubnetCidrBlock (..),
    mkDisassociateSubnetCidrBlock,

    -- ** Request lenses
    dscbAssociationId,

    -- * Destructuring the response
    DisassociateSubnetCidrBlockResponse (..),
    mkDisassociateSubnetCidrBlockResponse,

    -- ** Response lenses
    dscbrsSubnetId,
    dscbrsIPv6CidrBlockAssociation,
    dscbrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateSubnetCidrBlock' smart constructor.
newtype DisassociateSubnetCidrBlock = DisassociateSubnetCidrBlock'
  { associationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSubnetCidrBlock' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID for the CIDR block.
mkDisassociateSubnetCidrBlock ::
  -- | 'associationId'
  Lude.Text ->
  DisassociateSubnetCidrBlock
mkDisassociateSubnetCidrBlock pAssociationId_ =
  DisassociateSubnetCidrBlock' {associationId = pAssociationId_}

-- | The association ID for the CIDR block.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbAssociationId :: Lens.Lens' DisassociateSubnetCidrBlock Lude.Text
dscbAssociationId = Lens.lens (associationId :: DisassociateSubnetCidrBlock -> Lude.Text) (\s a -> s {associationId = a} :: DisassociateSubnetCidrBlock)
{-# DEPRECATED dscbAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

instance Lude.AWSRequest DisassociateSubnetCidrBlock where
  type
    Rs DisassociateSubnetCidrBlock =
      DisassociateSubnetCidrBlockResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateSubnetCidrBlockResponse'
            Lude.<$> (x Lude..@? "subnetId")
            Lude.<*> (x Lude..@? "ipv6CidrBlockAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateSubnetCidrBlock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateSubnetCidrBlock where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateSubnetCidrBlock where
  toQuery DisassociateSubnetCidrBlock' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateSubnetCidrBlock" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AssociationId" Lude.=: associationId
      ]

-- | /See:/ 'mkDisassociateSubnetCidrBlockResponse' smart constructor.
data DisassociateSubnetCidrBlockResponse = DisassociateSubnetCidrBlockResponse'
  { subnetId ::
      Lude.Maybe
        Lude.Text,
    ipv6CidrBlockAssociation ::
      Lude.Maybe
        SubnetIPv6CidrBlockAssociation,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSubnetCidrBlockResponse' with the minimum fields required to make a request.
--
-- * 'ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
-- * 'responseStatus' - The response status code.
-- * 'subnetId' - The ID of the subnet.
mkDisassociateSubnetCidrBlockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateSubnetCidrBlockResponse
mkDisassociateSubnetCidrBlockResponse pResponseStatus_ =
  DisassociateSubnetCidrBlockResponse'
    { subnetId = Lude.Nothing,
      ipv6CidrBlockAssociation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrsSubnetId :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Lude.Maybe Lude.Text)
dscbrsSubnetId = Lens.lens (subnetId :: DisassociateSubnetCidrBlockResponse -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: DisassociateSubnetCidrBlockResponse)
{-# DEPRECATED dscbrsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrsIPv6CidrBlockAssociation :: Lens.Lens' DisassociateSubnetCidrBlockResponse (Lude.Maybe SubnetIPv6CidrBlockAssociation)
dscbrsIPv6CidrBlockAssociation = Lens.lens (ipv6CidrBlockAssociation :: DisassociateSubnetCidrBlockResponse -> Lude.Maybe SubnetIPv6CidrBlockAssociation) (\s a -> s {ipv6CidrBlockAssociation = a} :: DisassociateSubnetCidrBlockResponse)
{-# DEPRECATED dscbrsIPv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscbrsResponseStatus :: Lens.Lens' DisassociateSubnetCidrBlockResponse Lude.Int
dscbrsResponseStatus = Lens.lens (responseStatus :: DisassociateSubnetCidrBlockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateSubnetCidrBlockResponse)
{-# DEPRECATED dscbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
