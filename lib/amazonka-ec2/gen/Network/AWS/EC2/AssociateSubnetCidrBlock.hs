{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateSubnetCidrBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your subnet. You can only associate a single IPv6 CIDR block with your subnet. An IPv6 CIDR block must have a prefix length of /64.
module Network.AWS.EC2.AssociateSubnetCidrBlock
  ( -- * Creating a request
    AssociateSubnetCidrBlock (..),
    mkAssociateSubnetCidrBlock,

    -- ** Request lenses
    ascbIPv6CidrBlock,
    ascbSubnetId,

    -- * Destructuring the response
    AssociateSubnetCidrBlockResponse (..),
    mkAssociateSubnetCidrBlockResponse,

    -- ** Response lenses
    ascbrsSubnetId,
    ascbrsIPv6CidrBlockAssociation,
    ascbrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateSubnetCidrBlock' smart constructor.
data AssociateSubnetCidrBlock = AssociateSubnetCidrBlock'
  { ipv6CidrBlock ::
      Lude.Text,
    subnetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSubnetCidrBlock' with the minimum fields required to make a request.
--
-- * 'ipv6CidrBlock' - The IPv6 CIDR block for your subnet. The subnet must have a /64 prefix length.
-- * 'subnetId' - The ID of your subnet.
mkAssociateSubnetCidrBlock ::
  -- | 'ipv6CidrBlock'
  Lude.Text ->
  -- | 'subnetId'
  Lude.Text ->
  AssociateSubnetCidrBlock
mkAssociateSubnetCidrBlock pIPv6CidrBlock_ pSubnetId_ =
  AssociateSubnetCidrBlock'
    { ipv6CidrBlock = pIPv6CidrBlock_,
      subnetId = pSubnetId_
    }

-- | The IPv6 CIDR block for your subnet. The subnet must have a /64 prefix length.
--
-- /Note:/ Consider using 'ipv6CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascbIPv6CidrBlock :: Lens.Lens' AssociateSubnetCidrBlock Lude.Text
ascbIPv6CidrBlock = Lens.lens (ipv6CidrBlock :: AssociateSubnetCidrBlock -> Lude.Text) (\s a -> s {ipv6CidrBlock = a} :: AssociateSubnetCidrBlock)
{-# DEPRECATED ascbIPv6CidrBlock "Use generic-lens or generic-optics with 'ipv6CidrBlock' instead." #-}

-- | The ID of your subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascbSubnetId :: Lens.Lens' AssociateSubnetCidrBlock Lude.Text
ascbSubnetId = Lens.lens (subnetId :: AssociateSubnetCidrBlock -> Lude.Text) (\s a -> s {subnetId = a} :: AssociateSubnetCidrBlock)
{-# DEPRECATED ascbSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance Lude.AWSRequest AssociateSubnetCidrBlock where
  type Rs AssociateSubnetCidrBlock = AssociateSubnetCidrBlockResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateSubnetCidrBlockResponse'
            Lude.<$> (x Lude..@? "subnetId")
            Lude.<*> (x Lude..@? "ipv6CidrBlockAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateSubnetCidrBlock where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateSubnetCidrBlock where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateSubnetCidrBlock where
  toQuery AssociateSubnetCidrBlock' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssociateSubnetCidrBlock" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Ipv6CidrBlock" Lude.=: ipv6CidrBlock,
        "SubnetId" Lude.=: subnetId
      ]

-- | /See:/ 'mkAssociateSubnetCidrBlockResponse' smart constructor.
data AssociateSubnetCidrBlockResponse = AssociateSubnetCidrBlockResponse'
  { subnetId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'AssociateSubnetCidrBlockResponse' with the minimum fields required to make a request.
--
-- * 'ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
-- * 'responseStatus' - The response status code.
-- * 'subnetId' - The ID of the subnet.
mkAssociateSubnetCidrBlockResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateSubnetCidrBlockResponse
mkAssociateSubnetCidrBlockResponse pResponseStatus_ =
  AssociateSubnetCidrBlockResponse'
    { subnetId = Lude.Nothing,
      ipv6CidrBlockAssociation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascbrsSubnetId :: Lens.Lens' AssociateSubnetCidrBlockResponse (Lude.Maybe Lude.Text)
ascbrsSubnetId = Lens.lens (subnetId :: AssociateSubnetCidrBlockResponse -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: AssociateSubnetCidrBlockResponse)
{-# DEPRECATED ascbrsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Information about the IPv6 CIDR block association.
--
-- /Note:/ Consider using 'ipv6CidrBlockAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascbrsIPv6CidrBlockAssociation :: Lens.Lens' AssociateSubnetCidrBlockResponse (Lude.Maybe SubnetIPv6CidrBlockAssociation)
ascbrsIPv6CidrBlockAssociation = Lens.lens (ipv6CidrBlockAssociation :: AssociateSubnetCidrBlockResponse -> Lude.Maybe SubnetIPv6CidrBlockAssociation) (\s a -> s {ipv6CidrBlockAssociation = a} :: AssociateSubnetCidrBlockResponse)
{-# DEPRECATED ascbrsIPv6CidrBlockAssociation "Use generic-lens or generic-optics with 'ipv6CidrBlockAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascbrsResponseStatus :: Lens.Lens' AssociateSubnetCidrBlockResponse Lude.Int
ascbrsResponseStatus = Lens.lens (responseStatus :: AssociateSubnetCidrBlockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateSubnetCidrBlockResponse)
{-# DEPRECATED ascbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
