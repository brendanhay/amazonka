{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachClassicLinkVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Links an EC2-Classic instance to a ClassicLink-enabled VPC through one or more of the VPC's security groups. You cannot link an EC2-Classic instance to more than one VPC at a time. You can only link an instance that's in the @running@ state. An instance is automatically unlinked from a VPC when it's stopped - you can link it to the VPC again when you restart it.
--
-- After you've linked an instance, you cannot change the VPC security groups that are associated with it. To change the security groups, you must first unlink the instance, and then link it again.
-- Linking your instance to a VPC is sometimes referred to as /attaching/ your instance.
module Network.AWS.EC2.AttachClassicLinkVPC
  ( -- * Creating a request
    AttachClassicLinkVPC (..),
    mkAttachClassicLinkVPC,

    -- ** Request lenses
    aclvInstanceId,
    aclvGroups,
    aclvVPCId,
    aclvDryRun,

    -- * Destructuring the response
    AttachClassicLinkVPCResponse (..),
    mkAttachClassicLinkVPCResponse,

    -- ** Response lenses
    aclvrsReturn,
    aclvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachClassicLinkVPC' smart constructor.
data AttachClassicLinkVPC = AttachClassicLinkVPC'
  { -- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
    instanceId :: Lude.Text,
    -- | The ID of one or more of the VPC's security groups. You cannot specify security groups from a different VPC.
    groups :: [Lude.Text],
    -- | The ID of a ClassicLink-enabled VPC.
    vpcId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachClassicLinkVPC' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
-- * 'groups' - The ID of one or more of the VPC's security groups. You cannot specify security groups from a different VPC.
-- * 'vpcId' - The ID of a ClassicLink-enabled VPC.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAttachClassicLinkVPC ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  AttachClassicLinkVPC
mkAttachClassicLinkVPC pInstanceId_ pVPCId_ =
  AttachClassicLinkVPC'
    { instanceId = pInstanceId_,
      groups = Lude.mempty,
      vpcId = pVPCId_,
      dryRun = Lude.Nothing
    }

-- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvInstanceId :: Lens.Lens' AttachClassicLinkVPC Lude.Text
aclvInstanceId = Lens.lens (instanceId :: AttachClassicLinkVPC -> Lude.Text) (\s a -> s {instanceId = a} :: AttachClassicLinkVPC)
{-# DEPRECATED aclvInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of one or more of the VPC's security groups. You cannot specify security groups from a different VPC.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvGroups :: Lens.Lens' AttachClassicLinkVPC [Lude.Text]
aclvGroups = Lens.lens (groups :: AttachClassicLinkVPC -> [Lude.Text]) (\s a -> s {groups = a} :: AttachClassicLinkVPC)
{-# DEPRECATED aclvGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The ID of a ClassicLink-enabled VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvVPCId :: Lens.Lens' AttachClassicLinkVPC Lude.Text
aclvVPCId = Lens.lens (vpcId :: AttachClassicLinkVPC -> Lude.Text) (\s a -> s {vpcId = a} :: AttachClassicLinkVPC)
{-# DEPRECATED aclvVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvDryRun :: Lens.Lens' AttachClassicLinkVPC (Lude.Maybe Lude.Bool)
aclvDryRun = Lens.lens (dryRun :: AttachClassicLinkVPC -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AttachClassicLinkVPC)
{-# DEPRECATED aclvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AttachClassicLinkVPC where
  type Rs AttachClassicLinkVPC = AttachClassicLinkVPCResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AttachClassicLinkVPCResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachClassicLinkVPC where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachClassicLinkVPC where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachClassicLinkVPC where
  toQuery AttachClassicLinkVPC' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachClassicLinkVpc" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        Lude.toQueryList "SecurityGroupId" groups,
        "VpcId" Lude.=: vpcId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkAttachClassicLinkVPCResponse' smart constructor.
data AttachClassicLinkVPCResponse = AttachClassicLinkVPCResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachClassicLinkVPCResponse' with the minimum fields required to make a request.
--
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
-- * 'responseStatus' - The response status code.
mkAttachClassicLinkVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachClassicLinkVPCResponse
mkAttachClassicLinkVPCResponse pResponseStatus_ =
  AttachClassicLinkVPCResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvrsReturn :: Lens.Lens' AttachClassicLinkVPCResponse (Lude.Maybe Lude.Bool)
aclvrsReturn = Lens.lens (return :: AttachClassicLinkVPCResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: AttachClassicLinkVPCResponse)
{-# DEPRECATED aclvrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclvrsResponseStatus :: Lens.Lens' AttachClassicLinkVPCResponse Lude.Int
aclvrsResponseStatus = Lens.lens (responseStatus :: AttachClassicLinkVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachClassicLinkVPCResponse)
{-# DEPRECATED aclvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
