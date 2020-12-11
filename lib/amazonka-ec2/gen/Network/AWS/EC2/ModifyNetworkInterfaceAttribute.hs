{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified network interface attribute. You can specify only one attribute at a time. You can use this action to attach and detach security groups from an existing EC2 instance.
module Network.AWS.EC2.ModifyNetworkInterfaceAttribute
  ( -- * Creating a request
    ModifyNetworkInterfaceAttribute (..),
    mkModifyNetworkInterfaceAttribute,

    -- ** Request lenses
    mniaGroups,
    mniaSourceDestCheck,
    mniaAttachment,
    mniaDescription,
    mniaDryRun,
    mniaNetworkInterfaceId,

    -- * Destructuring the response
    ModifyNetworkInterfaceAttributeResponse (..),
    mkModifyNetworkInterfaceAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ModifyNetworkInterfaceAttribute.
--
-- /See:/ 'mkModifyNetworkInterfaceAttribute' smart constructor.
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'
  { groups ::
      Lude.Maybe [Lude.Text],
    sourceDestCheck ::
      Lude.Maybe
        AttributeBooleanValue,
    attachment ::
      Lude.Maybe
        NetworkInterfaceAttachmentChanges,
    description ::
      Lude.Maybe AttributeValue,
    dryRun ::
      Lude.Maybe Lude.Bool,
    networkInterfaceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyNetworkInterfaceAttribute' with the minimum fields required to make a request.
--
-- * 'attachment' - Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
-- * 'description' - A description for the network interface.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groups' - Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'sourceDestCheck' - Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
mkModifyNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  ModifyNetworkInterfaceAttribute
mkModifyNetworkInterfaceAttribute pNetworkInterfaceId_ =
  ModifyNetworkInterfaceAttribute'
    { groups = Lude.Nothing,
      sourceDestCheck = Lude.Nothing,
      attachment = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaGroups :: Lens.Lens' ModifyNetworkInterfaceAttribute (Lude.Maybe [Lude.Text])
mniaGroups = Lens.lens (groups :: ModifyNetworkInterfaceAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: ModifyNetworkInterfaceAttribute)
{-# DEPRECATED mniaGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaSourceDestCheck :: Lens.Lens' ModifyNetworkInterfaceAttribute (Lude.Maybe AttributeBooleanValue)
mniaSourceDestCheck = Lens.lens (sourceDestCheck :: ModifyNetworkInterfaceAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {sourceDestCheck = a} :: ModifyNetworkInterfaceAttribute)
{-# DEPRECATED mniaSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaAttachment :: Lens.Lens' ModifyNetworkInterfaceAttribute (Lude.Maybe NetworkInterfaceAttachmentChanges)
mniaAttachment = Lens.lens (attachment :: ModifyNetworkInterfaceAttribute -> Lude.Maybe NetworkInterfaceAttachmentChanges) (\s a -> s {attachment = a} :: ModifyNetworkInterfaceAttribute)
{-# DEPRECATED mniaAttachment "Use generic-lens or generic-optics with 'attachment' instead." #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaDescription :: Lens.Lens' ModifyNetworkInterfaceAttribute (Lude.Maybe AttributeValue)
mniaDescription = Lens.lens (description :: ModifyNetworkInterfaceAttribute -> Lude.Maybe AttributeValue) (\s a -> s {description = a} :: ModifyNetworkInterfaceAttribute)
{-# DEPRECATED mniaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaDryRun :: Lens.Lens' ModifyNetworkInterfaceAttribute (Lude.Maybe Lude.Bool)
mniaDryRun = Lens.lens (dryRun :: ModifyNetworkInterfaceAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyNetworkInterfaceAttribute)
{-# DEPRECATED mniaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaNetworkInterfaceId :: Lens.Lens' ModifyNetworkInterfaceAttribute Lude.Text
mniaNetworkInterfaceId = Lens.lens (networkInterfaceId :: ModifyNetworkInterfaceAttribute -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: ModifyNetworkInterfaceAttribute)
{-# DEPRECATED mniaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.AWSRequest ModifyNetworkInterfaceAttribute where
  type
    Rs ModifyNetworkInterfaceAttribute =
      ModifyNetworkInterfaceAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifyNetworkInterfaceAttributeResponse'

instance Lude.ToHeaders ModifyNetworkInterfaceAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyNetworkInterfaceAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyNetworkInterfaceAttribute where
  toQuery ModifyNetworkInterfaceAttribute' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyNetworkInterfaceAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "SecurityGroupId" Lude.<$> groups),
        "SourceDestCheck" Lude.=: sourceDestCheck,
        "Attachment" Lude.=: attachment,
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun,
        "NetworkInterfaceId" Lude.=: networkInterfaceId
      ]

-- | /See:/ 'mkModifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyNetworkInterfaceAttributeResponse' with the minimum fields required to make a request.
mkModifyNetworkInterfaceAttributeResponse ::
  ModifyNetworkInterfaceAttributeResponse
mkModifyNetworkInterfaceAttributeResponse =
  ModifyNetworkInterfaceAttributeResponse'
