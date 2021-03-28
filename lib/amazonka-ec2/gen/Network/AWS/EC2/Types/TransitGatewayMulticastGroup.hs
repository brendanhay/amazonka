{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulticastGroup
  ( TransitGatewayMulticastGroup (..)
  -- * Smart constructor
  , mkTransitGatewayMulticastGroup
  -- * Lenses
  , tgmgGroupIpAddress
  , tgmgGroupMember
  , tgmgGroupSource
  , tgmgMemberType
  , tgmgNetworkInterfaceId
  , tgmgResourceId
  , tgmgResourceType
  , tgmgSourceType
  , tgmgSubnetId
  , tgmgTransitGatewayAttachmentId
  ) where

import qualified Network.AWS.EC2.Types.MembershipType as Types
import qualified Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the transit gateway multicast group resources.
--
-- /See:/ 'mkTransitGatewayMulticastGroup' smart constructor.
data TransitGatewayMulticastGroup = TransitGatewayMulticastGroup'
  { groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , groupMember :: Core.Maybe Core.Bool
    -- ^ Indicates that the resource is a transit gateway multicast group member.
  , groupSource :: Core.Maybe Core.Bool
    -- ^ Indicates that the resource is a transit gateway multicast group member.
  , memberType :: Core.Maybe Types.MembershipType
    -- ^ The member type (for example, @static@ ).
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway attachment.
  , resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  , resourceType :: Core.Maybe Types.TransitGatewayAttachmentResourceType
    -- ^ The type of resource, for example a VPC attachment.
  , sourceType :: Core.Maybe Types.MembershipType
    -- ^ The source type.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet.
  , transitGatewayAttachmentId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastGroup' value with any optional fields omitted.
mkTransitGatewayMulticastGroup
    :: TransitGatewayMulticastGroup
mkTransitGatewayMulticastGroup
  = TransitGatewayMulticastGroup'{groupIpAddress = Core.Nothing,
                                  groupMember = Core.Nothing, groupSource = Core.Nothing,
                                  memberType = Core.Nothing, networkInterfaceId = Core.Nothing,
                                  resourceId = Core.Nothing, resourceType = Core.Nothing,
                                  sourceType = Core.Nothing, subnetId = Core.Nothing,
                                  transitGatewayAttachmentId = Core.Nothing}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupIpAddress :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
tgmgGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE tgmgGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | Indicates that the resource is a transit gateway multicast group member.
--
-- /Note:/ Consider using 'groupMember' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupMember :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Bool)
tgmgGroupMember = Lens.field @"groupMember"
{-# INLINEABLE tgmgGroupMember #-}
{-# DEPRECATED groupMember "Use generic-lens or generic-optics with 'groupMember' instead"  #-}

-- | Indicates that the resource is a transit gateway multicast group member.
--
-- /Note:/ Consider using 'groupSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupSource :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Bool)
tgmgGroupSource = Lens.field @"groupSource"
{-# INLINEABLE tgmgGroupSource #-}
{-# DEPRECATED groupSource "Use generic-lens or generic-optics with 'groupSource' instead"  #-}

-- | The member type (for example, @static@ ).
--
-- /Note:/ Consider using 'memberType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgMemberType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.MembershipType)
tgmgMemberType = Lens.field @"memberType"
{-# INLINEABLE tgmgMemberType #-}
{-# DEPRECATED memberType "Use generic-lens or generic-optics with 'memberType' instead"  #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgNetworkInterfaceId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
tgmgNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE tgmgNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgResourceId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
tgmgResourceId = Lens.field @"resourceId"
{-# INLINEABLE tgmgResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgResourceType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.TransitGatewayAttachmentResourceType)
tgmgResourceType = Lens.field @"resourceType"
{-# INLINEABLE tgmgResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The source type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgSourceType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Types.MembershipType)
tgmgSourceType = Lens.field @"sourceType"
{-# INLINEABLE tgmgSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgSubnetId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
tgmgSubnetId = Lens.field @"subnetId"
{-# INLINEABLE tgmgSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
tgmgTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE tgmgTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.FromXML TransitGatewayMulticastGroup where
        parseXML x
          = TransitGatewayMulticastGroup' Core.<$>
              (x Core..@? "groupIpAddress") Core.<*> x Core..@? "groupMember"
                Core.<*> x Core..@? "groupSource"
                Core.<*> x Core..@? "memberType"
                Core.<*> x Core..@? "networkInterfaceId"
                Core.<*> x Core..@? "resourceId"
                Core.<*> x Core..@? "resourceType"
                Core.<*> x Core..@? "sourceType"
                Core.<*> x Core..@? "subnetId"
                Core.<*> x Core..@? "transitGatewayAttachmentId"
