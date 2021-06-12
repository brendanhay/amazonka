{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.MembershipType
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens

-- | Describes the transit gateway multicast group resources.
--
-- /See:/ 'newTransitGatewayMulticastGroup' smart constructor.
data TransitGatewayMulticastGroup = TransitGatewayMulticastGroup'
  { -- | The ID of the resource.
    resourceId :: Core.Maybe Core.Text,
    -- | Indicates that the resource is a transit gateway multicast group member.
    groupMember :: Core.Maybe Core.Bool,
    -- | The member type (for example, @static@).
    memberType :: Core.Maybe MembershipType,
    -- | Indicates that the resource is a transit gateway multicast group member.
    groupSource :: Core.Maybe Core.Bool,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Core.Maybe TransitGatewayAttachmentResourceType,
    -- | The ID of the AWS account that owns the transit gateway multicast domain
    -- group resource.
    resourceOwnerId :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway attachment.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Core.Maybe Core.Text,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Core.Text,
    -- | The source type.
    sourceType :: Core.Maybe MembershipType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'transitGatewayMulticastGroup_resourceId' - The ID of the resource.
--
-- 'groupMember', 'transitGatewayMulticastGroup_groupMember' - Indicates that the resource is a transit gateway multicast group member.
--
-- 'memberType', 'transitGatewayMulticastGroup_memberType' - The member type (for example, @static@).
--
-- 'groupSource', 'transitGatewayMulticastGroup_groupSource' - Indicates that the resource is a transit gateway multicast group member.
--
-- 'resourceType', 'transitGatewayMulticastGroup_resourceType' - The type of resource, for example a VPC attachment.
--
-- 'resourceOwnerId', 'transitGatewayMulticastGroup_resourceOwnerId' - The ID of the AWS account that owns the transit gateway multicast domain
-- group resource.
--
-- 'networkInterfaceId', 'transitGatewayMulticastGroup_networkInterfaceId' - The ID of the transit gateway attachment.
--
-- 'subnetId', 'transitGatewayMulticastGroup_subnetId' - The ID of the subnet.
--
-- 'transitGatewayAttachmentId', 'transitGatewayMulticastGroup_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- 'groupIpAddress', 'transitGatewayMulticastGroup_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'sourceType', 'transitGatewayMulticastGroup_sourceType' - The source type.
newTransitGatewayMulticastGroup ::
  TransitGatewayMulticastGroup
newTransitGatewayMulticastGroup =
  TransitGatewayMulticastGroup'
    { resourceId =
        Core.Nothing,
      groupMember = Core.Nothing,
      memberType = Core.Nothing,
      groupSource = Core.Nothing,
      resourceType = Core.Nothing,
      resourceOwnerId = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      subnetId = Core.Nothing,
      transitGatewayAttachmentId = Core.Nothing,
      groupIpAddress = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The ID of the resource.
transitGatewayMulticastGroup_resourceId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
transitGatewayMulticastGroup_resourceId = Lens.lens (\TransitGatewayMulticastGroup' {resourceId} -> resourceId) (\s@TransitGatewayMulticastGroup' {} a -> s {resourceId = a} :: TransitGatewayMulticastGroup)

-- | Indicates that the resource is a transit gateway multicast group member.
transitGatewayMulticastGroup_groupMember :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Bool)
transitGatewayMulticastGroup_groupMember = Lens.lens (\TransitGatewayMulticastGroup' {groupMember} -> groupMember) (\s@TransitGatewayMulticastGroup' {} a -> s {groupMember = a} :: TransitGatewayMulticastGroup)

-- | The member type (for example, @static@).
transitGatewayMulticastGroup_memberType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe MembershipType)
transitGatewayMulticastGroup_memberType = Lens.lens (\TransitGatewayMulticastGroup' {memberType} -> memberType) (\s@TransitGatewayMulticastGroup' {} a -> s {memberType = a} :: TransitGatewayMulticastGroup)

-- | Indicates that the resource is a transit gateway multicast group member.
transitGatewayMulticastGroup_groupSource :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Bool)
transitGatewayMulticastGroup_groupSource = Lens.lens (\TransitGatewayMulticastGroup' {groupSource} -> groupSource) (\s@TransitGatewayMulticastGroup' {} a -> s {groupSource = a} :: TransitGatewayMulticastGroup)

-- | The type of resource, for example a VPC attachment.
transitGatewayMulticastGroup_resourceType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe TransitGatewayAttachmentResourceType)
transitGatewayMulticastGroup_resourceType = Lens.lens (\TransitGatewayMulticastGroup' {resourceType} -> resourceType) (\s@TransitGatewayMulticastGroup' {} a -> s {resourceType = a} :: TransitGatewayMulticastGroup)

-- | The ID of the AWS account that owns the transit gateway multicast domain
-- group resource.
transitGatewayMulticastGroup_resourceOwnerId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
transitGatewayMulticastGroup_resourceOwnerId = Lens.lens (\TransitGatewayMulticastGroup' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayMulticastGroup' {} a -> s {resourceOwnerId = a} :: TransitGatewayMulticastGroup)

-- | The ID of the transit gateway attachment.
transitGatewayMulticastGroup_networkInterfaceId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
transitGatewayMulticastGroup_networkInterfaceId = Lens.lens (\TransitGatewayMulticastGroup' {networkInterfaceId} -> networkInterfaceId) (\s@TransitGatewayMulticastGroup' {} a -> s {networkInterfaceId = a} :: TransitGatewayMulticastGroup)

-- | The ID of the subnet.
transitGatewayMulticastGroup_subnetId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
transitGatewayMulticastGroup_subnetId = Lens.lens (\TransitGatewayMulticastGroup' {subnetId} -> subnetId) (\s@TransitGatewayMulticastGroup' {} a -> s {subnetId = a} :: TransitGatewayMulticastGroup)

-- | The ID of the transit gateway attachment.
transitGatewayMulticastGroup_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
transitGatewayMulticastGroup_transitGatewayAttachmentId = Lens.lens (\TransitGatewayMulticastGroup' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayMulticastGroup' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastGroup)

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastGroup_groupIpAddress :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe Core.Text)
transitGatewayMulticastGroup_groupIpAddress = Lens.lens (\TransitGatewayMulticastGroup' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastGroup' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastGroup)

-- | The source type.
transitGatewayMulticastGroup_sourceType :: Lens.Lens' TransitGatewayMulticastGroup (Core.Maybe MembershipType)
transitGatewayMulticastGroup_sourceType = Lens.lens (\TransitGatewayMulticastGroup' {sourceType} -> sourceType) (\s@TransitGatewayMulticastGroup' {} a -> s {sourceType = a} :: TransitGatewayMulticastGroup)

instance Core.FromXML TransitGatewayMulticastGroup where
  parseXML x =
    TransitGatewayMulticastGroup'
      Core.<$> (x Core..@? "resourceId")
      Core.<*> (x Core..@? "groupMember")
      Core.<*> (x Core..@? "memberType")
      Core.<*> (x Core..@? "groupSource")
      Core.<*> (x Core..@? "resourceType")
      Core.<*> (x Core..@? "resourceOwnerId")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "transitGatewayAttachmentId")
      Core.<*> (x Core..@? "groupIpAddress")
      Core.<*> (x Core..@? "sourceType")

instance Core.Hashable TransitGatewayMulticastGroup

instance Core.NFData TransitGatewayMulticastGroup
