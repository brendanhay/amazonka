{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNetworkACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network ACL in a VPC. Network ACLs provide an optional layer of security (in addition to security groups) for the instances in your VPC.
--
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNetworkACL
  ( -- * Creating a request
    CreateNetworkACL (..),
    mkCreateNetworkACL,

    -- ** Request lenses
    cnaVPCId,
    cnaTagSpecifications,
    cnaDryRun,

    -- * Destructuring the response
    CreateNetworkACLResponse (..),
    mkCreateNetworkACLResponse,

    -- ** Response lenses
    cnarsNetworkACL,
    cnarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateNetworkACL' smart constructor.
data CreateNetworkACL = CreateNetworkACL'
  { -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | The tags to assign to the network ACL.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkACL' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC.
-- * 'tagSpecifications' - The tags to assign to the network ACL.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateNetworkACL ::
  -- | 'vpcId'
  Lude.Text ->
  CreateNetworkACL
mkCreateNetworkACL pVPCId_ =
  CreateNetworkACL'
    { vpcId = pVPCId_,
      tagSpecifications = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaVPCId :: Lens.Lens' CreateNetworkACL Lude.Text
cnaVPCId = Lens.lens (vpcId :: CreateNetworkACL -> Lude.Text) (\s a -> s {vpcId = a} :: CreateNetworkACL)
{-# DEPRECATED cnaVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The tags to assign to the network ACL.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaTagSpecifications :: Lens.Lens' CreateNetworkACL (Lude.Maybe [TagSpecification])
cnaTagSpecifications = Lens.lens (tagSpecifications :: CreateNetworkACL -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateNetworkACL)
{-# DEPRECATED cnaTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnaDryRun :: Lens.Lens' CreateNetworkACL (Lude.Maybe Lude.Bool)
cnaDryRun = Lens.lens (dryRun :: CreateNetworkACL -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateNetworkACL)
{-# DEPRECATED cnaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateNetworkACL where
  type Rs CreateNetworkACL = CreateNetworkACLResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateNetworkACLResponse'
            Lude.<$> (x Lude..@? "networkAcl") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNetworkACL where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateNetworkACL where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNetworkACL where
  toQuery CreateNetworkACL' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateNetworkAcl" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpcId" Lude.=: vpcId,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateNetworkACLResponse' smart constructor.
data CreateNetworkACLResponse = CreateNetworkACLResponse'
  { -- | Information about the network ACL.
    networkACL :: Lude.Maybe NetworkACL,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNetworkACLResponse' with the minimum fields required to make a request.
--
-- * 'networkACL' - Information about the network ACL.
-- * 'responseStatus' - The response status code.
mkCreateNetworkACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNetworkACLResponse
mkCreateNetworkACLResponse pResponseStatus_ =
  CreateNetworkACLResponse'
    { networkACL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the network ACL.
--
-- /Note:/ Consider using 'networkACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnarsNetworkACL :: Lens.Lens' CreateNetworkACLResponse (Lude.Maybe NetworkACL)
cnarsNetworkACL = Lens.lens (networkACL :: CreateNetworkACLResponse -> Lude.Maybe NetworkACL) (\s a -> s {networkACL = a} :: CreateNetworkACLResponse)
{-# DEPRECATED cnarsNetworkACL "Use generic-lens or generic-optics with 'networkACL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnarsResponseStatus :: Lens.Lens' CreateNetworkACLResponse Lude.Int
cnarsResponseStatus = Lens.lens (responseStatus :: CreateNetworkACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNetworkACLResponse)
{-# DEPRECATED cnarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
