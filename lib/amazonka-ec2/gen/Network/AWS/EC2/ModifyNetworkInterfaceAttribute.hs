{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyNetworkInterfaceAttribute (..)
    , mkModifyNetworkInterfaceAttribute
    -- ** Request lenses
    , mniaNetworkInterfaceId
    , mniaAttachment
    , mniaDescription
    , mniaDryRun
    , mniaGroups
    , mniaSourceDestCheck

    -- * Destructuring the response
    , ModifyNetworkInterfaceAttributeResponse (..)
    , mkModifyNetworkInterfaceAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyNetworkInterfaceAttribute.
--
-- /See:/ 'mkModifyNetworkInterfaceAttribute' smart constructor.
data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , attachment :: Core.Maybe Types.NetworkInterfaceAttachmentChanges
    -- ^ Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
  , description :: Core.Maybe Types.AttributeValue
    -- ^ A description for the network interface.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , groups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
  , sourceDestCheck :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyNetworkInterfaceAttribute' value with any optional fields omitted.
mkModifyNetworkInterfaceAttribute
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> ModifyNetworkInterfaceAttribute
mkModifyNetworkInterfaceAttribute networkInterfaceId
  = ModifyNetworkInterfaceAttribute'{networkInterfaceId,
                                     attachment = Core.Nothing, description = Core.Nothing,
                                     dryRun = Core.Nothing, groups = Core.Nothing,
                                     sourceDestCheck = Core.Nothing}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaNetworkInterfaceId :: Lens.Lens' ModifyNetworkInterfaceAttribute Types.NetworkInterfaceId
mniaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE mniaNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | Information about the interface attachment. If modifying the 'delete on termination' attribute, you must specify the ID of the interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaAttachment :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Types.NetworkInterfaceAttachmentChanges)
mniaAttachment = Lens.field @"attachment"
{-# INLINEABLE mniaAttachment #-}
{-# DEPRECATED attachment "Use generic-lens or generic-optics with 'attachment' instead"  #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaDescription :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Types.AttributeValue)
mniaDescription = Lens.field @"description"
{-# INLINEABLE mniaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaDryRun :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Core.Bool)
mniaDryRun = Lens.field @"dryRun"
{-# INLINEABLE mniaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Changes the security groups for the network interface. The new set of groups you specify replaces the current set. You must specify at least one group, even if it's just the default security group in the VPC. You must specify the ID of the security group, not the name.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaGroups :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe [Types.SecurityGroupId])
mniaGroups = Lens.field @"groups"
{-# INLINEABLE mniaGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | Indicates whether source/destination checking is enabled. A value of @true@ means checking is enabled, and @false@ means checking is disabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mniaSourceDestCheck :: Lens.Lens' ModifyNetworkInterfaceAttribute (Core.Maybe Types.AttributeBooleanValue)
mniaSourceDestCheck = Lens.field @"sourceDestCheck"
{-# INLINEABLE mniaSourceDestCheck #-}
{-# DEPRECATED sourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead"  #-}

instance Core.ToQuery ModifyNetworkInterfaceAttribute where
        toQuery ModifyNetworkInterfaceAttribute{..}
          = Core.toQueryPair "Action"
              ("ModifyNetworkInterfaceAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Attachment") attachment
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId") groups
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceDestCheck")
                sourceDestCheck

instance Core.ToHeaders ModifyNetworkInterfaceAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyNetworkInterfaceAttribute where
        type Rs ModifyNetworkInterfaceAttribute =
             ModifyNetworkInterfaceAttributeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull ModifyNetworkInterfaceAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyNetworkInterfaceAttributeResponse' smart constructor.
data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyNetworkInterfaceAttributeResponse' value with any optional fields omitted.
mkModifyNetworkInterfaceAttributeResponse
    :: ModifyNetworkInterfaceAttributeResponse
mkModifyNetworkInterfaceAttributeResponse
  = ModifyNetworkInterfaceAttributeResponse'
