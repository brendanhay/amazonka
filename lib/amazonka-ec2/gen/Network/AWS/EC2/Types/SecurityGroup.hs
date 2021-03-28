{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SecurityGroup
  ( SecurityGroup (..)
  -- * Smart constructor
  , mkSecurityGroup
  -- * Lenses
  , sgDescription
  , sgGroupId
  , sgGroupName
  , sgIpPermissions
  , sgIpPermissionsEgress
  , sgOwnerId
  , sgTags
  , sgVpcId
  ) where

import qualified Network.AWS.EC2.Types.IpPermission as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a security group
--
-- /See:/ 'mkSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { description :: Core.Text
    -- ^ A description of the security group.
  , groupId :: Core.Text
    -- ^ The ID of the security group.
  , groupName :: Core.Text
    -- ^ The name of the security group.
  , ipPermissions :: Core.Maybe [Types.IpPermission]
    -- ^ The inbound rules associated with the security group.
  , ipPermissionsEgress :: Core.Maybe [Types.IpPermission]
    -- ^ [VPC only] The outbound rules associated with the security group.
  , ownerId :: Core.Text
    -- ^ The AWS account ID of the owner of the security group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the security group.
  , vpcId :: Core.Maybe Core.Text
    -- ^ [VPC only] The ID of the VPC for the security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroup' value with any optional fields omitted.
mkSecurityGroup
    :: Core.Text -- ^ 'description'
    -> Core.Text -- ^ 'groupId'
    -> Core.Text -- ^ 'groupName'
    -> Core.Text -- ^ 'ownerId'
    -> SecurityGroup
mkSecurityGroup description groupId groupName ownerId
  = SecurityGroup'{description, groupId, groupName,
                   ipPermissions = Core.Nothing, ipPermissionsEgress = Core.Nothing,
                   ownerId, tags = Core.Nothing, vpcId = Core.Nothing}

-- | A description of the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SecurityGroup Core.Text
sgDescription = Lens.field @"description"
{-# INLINEABLE sgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupId :: Lens.Lens' SecurityGroup Core.Text
sgGroupId = Lens.field @"groupId"
{-# INLINEABLE sgGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupName :: Lens.Lens' SecurityGroup Core.Text
sgGroupName = Lens.field @"groupName"
{-# INLINEABLE sgGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The inbound rules associated with the security group.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgIpPermissions :: Lens.Lens' SecurityGroup (Core.Maybe [Types.IpPermission])
sgIpPermissions = Lens.field @"ipPermissions"
{-# INLINEABLE sgIpPermissions #-}
{-# DEPRECATED ipPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead"  #-}

-- | [VPC only] The outbound rules associated with the security group.
--
-- /Note:/ Consider using 'ipPermissionsEgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgIpPermissionsEgress :: Lens.Lens' SecurityGroup (Core.Maybe [Types.IpPermission])
sgIpPermissionsEgress = Lens.field @"ipPermissionsEgress"
{-# INLINEABLE sgIpPermissionsEgress #-}
{-# DEPRECATED ipPermissionsEgress "Use generic-lens or generic-optics with 'ipPermissionsEgress' instead"  #-}

-- | The AWS account ID of the owner of the security group.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgOwnerId :: Lens.Lens' SecurityGroup Core.Text
sgOwnerId = Lens.field @"ownerId"
{-# INLINEABLE sgOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Any tags assigned to the security group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgTags :: Lens.Lens' SecurityGroup (Core.Maybe [Types.Tag])
sgTags = Lens.field @"tags"
{-# INLINEABLE sgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | [VPC only] The ID of the VPC for the security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgVpcId :: Lens.Lens' SecurityGroup (Core.Maybe Core.Text)
sgVpcId = Lens.field @"vpcId"
{-# INLINEABLE sgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML SecurityGroup where
        parseXML x
          = SecurityGroup' Core.<$>
              (x Core..@ "groupDescription") Core.<*> x Core..@ "groupId"
                Core.<*> x Core..@ "groupName"
                Core.<*>
                x Core..@? "ipPermissions" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "ipPermissionsEgress" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@ "ownerId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcId"
