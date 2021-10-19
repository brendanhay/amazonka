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
import qualified Network.AWS.Prelude as Prelude

-- | Describes the transit gateway multicast group resources.
--
-- /See:/ 'newTransitGatewayMulticastGroup' smart constructor.
data TransitGatewayMulticastGroup = TransitGatewayMulticastGroup'
  { -- | The ID of the resource.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource, for example a VPC attachment.
    resourceType :: Prelude.Maybe TransitGatewayAttachmentResourceType,
    -- | The source type.
    sourceType :: Prelude.Maybe MembershipType,
    -- | The member type (for example, @static@).
    memberType :: Prelude.Maybe MembershipType,
    -- | The ID of the transit gateway attachment.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | Indicates that the resource is a transit gateway multicast group member.
    groupMember :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that the resource is a transit gateway multicast group member.
    groupSource :: Prelude.Maybe Prelude.Bool,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway attachment.
    transitGatewayAttachmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the transit gateway
    -- multicast domain group resource.
    resourceOwnerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'resourceType', 'transitGatewayMulticastGroup_resourceType' - The type of resource, for example a VPC attachment.
--
-- 'sourceType', 'transitGatewayMulticastGroup_sourceType' - The source type.
--
-- 'memberType', 'transitGatewayMulticastGroup_memberType' - The member type (for example, @static@).
--
-- 'networkInterfaceId', 'transitGatewayMulticastGroup_networkInterfaceId' - The ID of the transit gateway attachment.
--
-- 'subnetId', 'transitGatewayMulticastGroup_subnetId' - The ID of the subnet.
--
-- 'groupMember', 'transitGatewayMulticastGroup_groupMember' - Indicates that the resource is a transit gateway multicast group member.
--
-- 'groupSource', 'transitGatewayMulticastGroup_groupSource' - Indicates that the resource is a transit gateway multicast group member.
--
-- 'groupIpAddress', 'transitGatewayMulticastGroup_groupIpAddress' - The IP address assigned to the transit gateway multicast group.
--
-- 'transitGatewayAttachmentId', 'transitGatewayMulticastGroup_transitGatewayAttachmentId' - The ID of the transit gateway attachment.
--
-- 'resourceOwnerId', 'transitGatewayMulticastGroup_resourceOwnerId' - The ID of the Amazon Web Services account that owns the transit gateway
-- multicast domain group resource.
newTransitGatewayMulticastGroup ::
  TransitGatewayMulticastGroup
newTransitGatewayMulticastGroup =
  TransitGatewayMulticastGroup'
    { resourceId =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      memberType = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      groupMember = Prelude.Nothing,
      groupSource = Prelude.Nothing,
      groupIpAddress = Prelude.Nothing,
      transitGatewayAttachmentId = Prelude.Nothing,
      resourceOwnerId = Prelude.Nothing
    }

-- | The ID of the resource.
transitGatewayMulticastGroup_resourceId :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Text)
transitGatewayMulticastGroup_resourceId = Lens.lens (\TransitGatewayMulticastGroup' {resourceId} -> resourceId) (\s@TransitGatewayMulticastGroup' {} a -> s {resourceId = a} :: TransitGatewayMulticastGroup)

-- | The type of resource, for example a VPC attachment.
transitGatewayMulticastGroup_resourceType :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe TransitGatewayAttachmentResourceType)
transitGatewayMulticastGroup_resourceType = Lens.lens (\TransitGatewayMulticastGroup' {resourceType} -> resourceType) (\s@TransitGatewayMulticastGroup' {} a -> s {resourceType = a} :: TransitGatewayMulticastGroup)

-- | The source type.
transitGatewayMulticastGroup_sourceType :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe MembershipType)
transitGatewayMulticastGroup_sourceType = Lens.lens (\TransitGatewayMulticastGroup' {sourceType} -> sourceType) (\s@TransitGatewayMulticastGroup' {} a -> s {sourceType = a} :: TransitGatewayMulticastGroup)

-- | The member type (for example, @static@).
transitGatewayMulticastGroup_memberType :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe MembershipType)
transitGatewayMulticastGroup_memberType = Lens.lens (\TransitGatewayMulticastGroup' {memberType} -> memberType) (\s@TransitGatewayMulticastGroup' {} a -> s {memberType = a} :: TransitGatewayMulticastGroup)

-- | The ID of the transit gateway attachment.
transitGatewayMulticastGroup_networkInterfaceId :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Text)
transitGatewayMulticastGroup_networkInterfaceId = Lens.lens (\TransitGatewayMulticastGroup' {networkInterfaceId} -> networkInterfaceId) (\s@TransitGatewayMulticastGroup' {} a -> s {networkInterfaceId = a} :: TransitGatewayMulticastGroup)

-- | The ID of the subnet.
transitGatewayMulticastGroup_subnetId :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Text)
transitGatewayMulticastGroup_subnetId = Lens.lens (\TransitGatewayMulticastGroup' {subnetId} -> subnetId) (\s@TransitGatewayMulticastGroup' {} a -> s {subnetId = a} :: TransitGatewayMulticastGroup)

-- | Indicates that the resource is a transit gateway multicast group member.
transitGatewayMulticastGroup_groupMember :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Bool)
transitGatewayMulticastGroup_groupMember = Lens.lens (\TransitGatewayMulticastGroup' {groupMember} -> groupMember) (\s@TransitGatewayMulticastGroup' {} a -> s {groupMember = a} :: TransitGatewayMulticastGroup)

-- | Indicates that the resource is a transit gateway multicast group member.
transitGatewayMulticastGroup_groupSource :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Bool)
transitGatewayMulticastGroup_groupSource = Lens.lens (\TransitGatewayMulticastGroup' {groupSource} -> groupSource) (\s@TransitGatewayMulticastGroup' {} a -> s {groupSource = a} :: TransitGatewayMulticastGroup)

-- | The IP address assigned to the transit gateway multicast group.
transitGatewayMulticastGroup_groupIpAddress :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Text)
transitGatewayMulticastGroup_groupIpAddress = Lens.lens (\TransitGatewayMulticastGroup' {groupIpAddress} -> groupIpAddress) (\s@TransitGatewayMulticastGroup' {} a -> s {groupIpAddress = a} :: TransitGatewayMulticastGroup)

-- | The ID of the transit gateway attachment.
transitGatewayMulticastGroup_transitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Text)
transitGatewayMulticastGroup_transitGatewayAttachmentId = Lens.lens (\TransitGatewayMulticastGroup' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@TransitGatewayMulticastGroup' {} a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastGroup)

-- | The ID of the Amazon Web Services account that owns the transit gateway
-- multicast domain group resource.
transitGatewayMulticastGroup_resourceOwnerId :: Lens.Lens' TransitGatewayMulticastGroup (Prelude.Maybe Prelude.Text)
transitGatewayMulticastGroup_resourceOwnerId = Lens.lens (\TransitGatewayMulticastGroup' {resourceOwnerId} -> resourceOwnerId) (\s@TransitGatewayMulticastGroup' {} a -> s {resourceOwnerId = a} :: TransitGatewayMulticastGroup)

instance Core.FromXML TransitGatewayMulticastGroup where
  parseXML x =
    TransitGatewayMulticastGroup'
      Prelude.<$> (x Core..@? "resourceId")
      Prelude.<*> (x Core..@? "resourceType")
      Prelude.<*> (x Core..@? "sourceType")
      Prelude.<*> (x Core..@? "memberType")
      Prelude.<*> (x Core..@? "networkInterfaceId")
      Prelude.<*> (x Core..@? "subnetId")
      Prelude.<*> (x Core..@? "groupMember")
      Prelude.<*> (x Core..@? "groupSource")
      Prelude.<*> (x Core..@? "groupIpAddress")
      Prelude.<*> (x Core..@? "transitGatewayAttachmentId")
      Prelude.<*> (x Core..@? "resourceOwnerId")

instance
  Prelude.Hashable
    TransitGatewayMulticastGroup

instance Prelude.NFData TransitGatewayMulticastGroup
