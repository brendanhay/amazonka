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
    tgmgResourceId,
    tgmgResourceType,
    tgmgSourceType,
    tgmgMemberType,
    tgmgNetworkInterfaceId,
    tgmgSubnetId,
    tgmgGroupMember,
    tgmgGroupSource,
    tgmgGroupIPAddress,
    tgmgTransitGatewayAttachmentId,
  )
where

import Network.AWS.EC2.Types.MembershipType
import Network.AWS.EC2.Types.TransitGatewayAttachmentResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the transit gateway multicast group resources.
--
-- /See:/ 'mkTransitGatewayMulticastGroup' smart constructor.
data TransitGatewayMulticastGroup = TransitGatewayMulticastGroup'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType ::
      Lude.Maybe
        TransitGatewayAttachmentResourceType,
    sourceType ::
      Lude.Maybe MembershipType,
    memberType ::
      Lude.Maybe MembershipType,
    networkInterfaceId ::
      Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    groupMember ::
      Lude.Maybe Lude.Bool,
    groupSource ::
      Lude.Maybe Lude.Bool,
    groupIPAddress ::
      Lude.Maybe Lude.Text,
    transitGatewayAttachmentId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayMulticastGroup' with the minimum fields required to make a request.
--
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
-- * 'groupMember' - Indicates that the resource is a transit gateway multicast group member.
-- * 'groupSource' - Indicates that the resource is a transit gateway multicast group member.
-- * 'memberType' - The member type (for example, @static@ ).
-- * 'networkInterfaceId' - The ID of the transit gateway attachment.
-- * 'resourceId' - The ID of the resource.
-- * 'resourceType' - The type of resource, for example a VPC attachment.
-- * 'sourceType' - The source type.
-- * 'subnetId' - The ID of the subnet.
-- * 'transitGatewayAttachmentId' - The ID of the transit gateway attachment.
mkTransitGatewayMulticastGroup ::
  TransitGatewayMulticastGroup
mkTransitGatewayMulticastGroup =
  TransitGatewayMulticastGroup'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      sourceType = Lude.Nothing,
      memberType = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      groupMember = Lude.Nothing,
      groupSource = Lude.Nothing,
      groupIPAddress = Lude.Nothing,
      transitGatewayAttachmentId = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgResourceId :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe Lude.Text)
tgmgResourceId = Lens.lens (resourceId :: TransitGatewayMulticastGroup -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource, for example a VPC attachment.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgResourceType :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe TransitGatewayAttachmentResourceType)
tgmgResourceType = Lens.lens (resourceType :: TransitGatewayMulticastGroup -> Lude.Maybe TransitGatewayAttachmentResourceType) (\s a -> s {resourceType = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The source type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgSourceType :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe MembershipType)
tgmgSourceType = Lens.lens (sourceType :: TransitGatewayMulticastGroup -> Lude.Maybe MembershipType) (\s a -> s {sourceType = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The member type (for example, @static@ ).
--
-- /Note:/ Consider using 'memberType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgMemberType :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe MembershipType)
tgmgMemberType = Lens.lens (memberType :: TransitGatewayMulticastGroup -> Lude.Maybe MembershipType) (\s a -> s {memberType = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgMemberType "Use generic-lens or generic-optics with 'memberType' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgNetworkInterfaceId :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe Lude.Text)
tgmgNetworkInterfaceId = Lens.lens (networkInterfaceId :: TransitGatewayMulticastGroup -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgSubnetId :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe Lude.Text)
tgmgSubnetId = Lens.lens (subnetId :: TransitGatewayMulticastGroup -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Indicates that the resource is a transit gateway multicast group member.
--
-- /Note:/ Consider using 'groupMember' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupMember :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe Lude.Bool)
tgmgGroupMember = Lens.lens (groupMember :: TransitGatewayMulticastGroup -> Lude.Maybe Lude.Bool) (\s a -> s {groupMember = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgGroupMember "Use generic-lens or generic-optics with 'groupMember' instead." #-}

-- | Indicates that the resource is a transit gateway multicast group member.
--
-- /Note:/ Consider using 'groupSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupSource :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe Lude.Bool)
tgmgGroupSource = Lens.lens (groupSource :: TransitGatewayMulticastGroup -> Lude.Maybe Lude.Bool) (\s a -> s {groupSource = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgGroupSource "Use generic-lens or generic-optics with 'groupSource' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgGroupIPAddress :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe Lude.Text)
tgmgGroupIPAddress = Lens.lens (groupIPAddress :: TransitGatewayMulticastGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

-- | The ID of the transit gateway attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmgTransitGatewayAttachmentId :: Lens.Lens' TransitGatewayMulticastGroup (Lude.Maybe Lude.Text)
tgmgTransitGatewayAttachmentId = Lens.lens (transitGatewayAttachmentId :: TransitGatewayMulticastGroup -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayAttachmentId = a} :: TransitGatewayMulticastGroup)
{-# DEPRECATED tgmgTransitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead." #-}

instance Lude.FromXML TransitGatewayMulticastGroup where
  parseXML x =
    TransitGatewayMulticastGroup'
      Lude.<$> (x Lude..@? "resourceId")
      Lude.<*> (x Lude..@? "resourceType")
      Lude.<*> (x Lude..@? "sourceType")
      Lude.<*> (x Lude..@? "memberType")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "groupMember")
      Lude.<*> (x Lude..@? "groupSource")
      Lude.<*> (x Lude..@? "groupIpAddress")
      Lude.<*> (x Lude..@? "transitGatewayAttachmentId")
