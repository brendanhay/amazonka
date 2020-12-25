{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroup
  ( SecurityGroup (..),

    -- * Smart constructor
    mkSecurityGroup,

    -- * Lenses
    sgDescription,
    sgGroupId,
    sgGroupName,
    sgIpPermissions,
    sgIpPermissionsEgress,
    sgOwnerId,
    sgTags,
    sgVpcId,
  )
where

import qualified Network.AWS.EC2.Types.IpPermission as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a security group
--
-- /See:/ 'mkSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { -- | A description of the security group.
    description :: Types.String,
    -- | The ID of the security group.
    groupId :: Types.String,
    -- | The name of the security group.
    groupName :: Types.String,
    -- | The inbound rules associated with the security group.
    ipPermissions :: Core.Maybe [Types.IpPermission],
    -- | [VPC only] The outbound rules associated with the security group.
    ipPermissionsEgress :: Core.Maybe [Types.IpPermission],
    -- | The AWS account ID of the owner of the security group.
    ownerId :: Types.String,
    -- | Any tags assigned to the security group.
    tags :: Core.Maybe [Types.Tag],
    -- | [VPC only] The ID of the VPC for the security group.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroup' value with any optional fields omitted.
mkSecurityGroup ::
  -- | 'description'
  Types.String ->
  -- | 'groupId'
  Types.String ->
  -- | 'groupName'
  Types.String ->
  -- | 'ownerId'
  Types.String ->
  SecurityGroup
mkSecurityGroup description groupId groupName ownerId =
  SecurityGroup'
    { description,
      groupId,
      groupName,
      ipPermissions = Core.Nothing,
      ipPermissionsEgress = Core.Nothing,
      ownerId,
      tags = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | A description of the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SecurityGroup Types.String
sgDescription = Lens.field @"description"
{-# DEPRECATED sgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupId :: Lens.Lens' SecurityGroup Types.String
sgGroupId = Lens.field @"groupId"
{-# DEPRECATED sgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupName :: Lens.Lens' SecurityGroup Types.String
sgGroupName = Lens.field @"groupName"
{-# DEPRECATED sgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The inbound rules associated with the security group.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgIpPermissions :: Lens.Lens' SecurityGroup (Core.Maybe [Types.IpPermission])
sgIpPermissions = Lens.field @"ipPermissions"
{-# DEPRECATED sgIpPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | [VPC only] The outbound rules associated with the security group.
--
-- /Note:/ Consider using 'ipPermissionsEgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgIpPermissionsEgress :: Lens.Lens' SecurityGroup (Core.Maybe [Types.IpPermission])
sgIpPermissionsEgress = Lens.field @"ipPermissionsEgress"
{-# DEPRECATED sgIpPermissionsEgress "Use generic-lens or generic-optics with 'ipPermissionsEgress' instead." #-}

-- | The AWS account ID of the owner of the security group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgOwnerId :: Lens.Lens' SecurityGroup Types.String
sgOwnerId = Lens.field @"ownerId"
{-# DEPRECATED sgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Any tags assigned to the security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgTags :: Lens.Lens' SecurityGroup (Core.Maybe [Types.Tag])
sgTags = Lens.field @"tags"
{-# DEPRECATED sgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | [VPC only] The ID of the VPC for the security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgVpcId :: Lens.Lens' SecurityGroup (Core.Maybe Types.String)
sgVpcId = Lens.field @"vpcId"
{-# DEPRECATED sgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML SecurityGroup where
  parseXML x =
    SecurityGroup'
      Core.<$> (x Core..@ "groupDescription")
      Core.<*> (x Core..@ "groupId")
      Core.<*> (x Core..@ "groupName")
      Core.<*> (x Core..@? "ipPermissions" Core..<@> Core.parseXMLList "item")
      Core.<*> ( x Core..@? "ipPermissionsEgress"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@ "ownerId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "vpcId")
