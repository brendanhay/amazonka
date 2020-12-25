{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastGroup
  ( TransitGatewayMulticastGroup (..),

    -- * Smart constructor
    mkTransitGatewayMulticastGroup,

    -- * Lenses
    tgmgGroupIpAddress,
    tgmgGroupMember,
    tgmgGroupSource,
    tgmgMemberType,
    tgmgNetworkInterfaceId,
    tgmgResourceId,
    tgmgResourceType,
    tgmgSourceType,
    tgmgSubnetId,
    tgmgTransitGatewayAttachmentId,
  )
where

import qualified Network.AWS.EC2.Types.MembershipType as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the transit gateway multicast group resources.
--
-- /See:/ 'mkTransitGatewayMulticastGroup' smart constructor.
data TransitGatewayMulticastGroup = TransitGatewayMulticastGroup'
  { -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Types.String,
    -- | Indicates that the resource is a transit gateway multicast group member.
    groupMember :: Core.Maybe Core.Bool,
    -- | Indicates that the resource is a transit gateway multicast group member.
    groupSource :: Core.Maybe Core.Bool,
    -- | The member type (for example, @static@ ).
    memberType :: Core.Maybe Types.MembershipType,
    -- | The ID of the transit gateway attachment.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The ID of the resource.
    resourceId :: Core.Maybe Types.String,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType,
    -- | The source type.
    sourceType :: Core.Maybe Types.MembershipType,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Types.String,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastGroup' value with any optional fields omitted.
mkTransitGatewayMulticastGroup ::
  TransitGatewayMulticastGroup
mkTransitGatewayMulticastGroup =
  TransitGatewayMulticastGroup'
    { groupIpAddress = Core.Nothing,
      groupMember = Core.Nothing,
      groupSource = Core.Nothing,
      memberType = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      sourceType = Core.Nothing,
      subnetId = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing
    }

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupIpAddress :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.String)
tgmgGroupIpAddress = Lens.field @"groupIpAddress"
{-# DEPRECATED tgmgGroupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead." #-}

-- | Indicates that the resource is a transit gateway multicast group member.
--
-- /Note:/ Consider using 'groupMember' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupMember :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Bool)
tgmgGroupMember = Lens.field @"groupMember"
{-# DEPRECATED tgmgGroupMember "Use generic-lens or generic-optics with 'groupMember' instead." #-}

-- | Indicates that the resource is a transit gateway multicast group member.
--
-- /Note:/ Consider using 'groupSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupSource :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Bool)
tgmgGroupSource = Lens.field @"groupSource"
{-# DEPRECATED tgmgGroupSource "Use generic-lens or generic-optics with 'groupSource' instead." #-}

-- | The member type (for example, @static@ ).
--
-- /Note:/ Consider using 'memberType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgMemberType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.MembershipType)
tgmgMemberType = Lens.field @"memberType"
{-# DEPRECATED tgmgMemberType "Use generic-lens or generic-optics with 'memberType' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgNetworkInterfaceId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.String)
tgmgNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED tgmgNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgResourceId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.String)
tgmgResourceId = Lens.field @"resourceId"
{-# DEPRECATED tgmgResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgResourceType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgmgResourceType = Lens.field @"resourceType"
{-# DEPRECATED tgmgResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The source type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgSourceType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.MembershipType)
tgmgSourceType = Lens.field @"sourceType"
{-# DEPRECATED tgmgSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgSubnetId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.String)
tgmgSubnetId = Lens.field @"subnetId"
{-# DEPRECATED tgmgSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.String)
tgmgTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# DEPRECATED tgmgTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Core.FromXML TransitGatewayMulticastGroup where
  parseXML x =
    TransitGatewayMulticastGroup'
      Core.<$> (x Core..@? "groupIpAddress")
      Core.<*> (x Core..@? "groupMember")
      Core.<*> (x Core..@? "groupSource")
      Core.<*> (x Core..@? "memberType")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "sourceType")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
