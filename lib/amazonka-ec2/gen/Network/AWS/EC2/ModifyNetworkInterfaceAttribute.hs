{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    mniaNetworkInterfaceId,
    mniaAttachment,
    mniaDescription,
    mniaDryRun,
    mniaGroups,
    mniaSourceDestCheck,

    -- * Destructuring the response
    ModifyNetworkInterfaceAttributeResponse (..),
    mkModifyNetworkInterfaceAttributeResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyNetworkInterfaceAttribute.
--
-- /See:/ 'mkModifyNetworkInterfaceAttribute' smart constructor.
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'
  { -- | The ID of the network interface.
    networkInterfaceId :: Types.NetworkInterfaceId,
    -- | Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
    attachment :: Core.Maybe Types.NetworkInterfaceAttachmentChanges,
    -- | A description for the network interface.
    description :: Core.Maybe Types.AttributeValue,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
    groups :: Core.Maybe [Types.SecurityGroupId],
    -- | Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
    sourceDestCheck :: Core.Maybe Types.AttributeBooleanValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyNetworkInterfaceAttribute' value with any optional fields omitted.
mkModifyNetworkInterfaceAttribute ::
  -- | 'networkInterfaceId'
  Types.NetworkInterfaceId ->
  ModifyNetworkInterfaceAttribute
mkModifyNetworkInterfaceAttribute networkInterfaceId =
  ModifyNetworkInterfaceAttribute'
    { networkInterfaceId,
      attachment = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      groups = Core.Nothing,
      sourceDestCheck = Core.Nothing
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaNetworkInterfaceId :: Lens.Lens' ModifyNetworkInterfaceAttribute Types.NetworkInterfaceId
mniaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED mniaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaAttachment :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Types.NetworkInterfaceAttachmentChanges)
mniaAttachment = Lens.field @"attachment"
{-# DEPRECATED mniaAttachment "Use generic-lens or generic-optics with 'attachment' instead." #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaDescription :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Types.AttributeValue)
mniaDescription = Lens.field @"description"
{-# DEPRECATED mniaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaDryRun :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Core.Bool)
mniaDryRun = Lens.field @"dryRun"
{-# DEPRECATED mniaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaGroups :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe [Types.SecurityGroupId])
mniaGroups = Lens.field @"groups"
{-# DEPRECATED mniaGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaSourceDestCheck :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Types.AttributeBooleanValue)
mniaSourceDestCheck = Lens.field @"sourceDestCheck"
{-# DEPRECATED mniaSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

instance Core.AWSRequest ModifyNetworkInterfaceAttribute where
  type
    Rs ModifyNetworkInterfaceAttribute =
      ModifyNetworkInterfaceAttributeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ModifyNetworkInterfaceAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "NetworkInterfaceId" networkInterfaceId)
                Core.<> (Core.toQueryValue "Attachment" Core.<$> attachment)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "SecurityGroupId" Core.<$> groups)
                Core.<> (Core.toQueryValue "SourceDestCheck" Core.<$> sourceDestCheck)
            )
      }
  response =
    Response.receiveNull ModifyNetworkInterfaceAttributeResponse'

-- | /See:/ 'mkModifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyNetworkInterfaceAttributeResponse' value with any optional fields omitted.
mkModifyNetworkInterfaceAttributeResponse ::
  ModifyNetworkInterfaceAttributeResponse
mkModifyNetworkInterfaceAttributeResponse =
  ModifyNetworkInterfaceAttributeResponse'
