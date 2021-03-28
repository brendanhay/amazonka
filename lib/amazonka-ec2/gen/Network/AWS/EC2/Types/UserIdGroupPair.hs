{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserIdGroupPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.UserIdGroupPair
  ( UserIdGroupPair (..)
  -- * Smart constructor
  , mkUserIdGroupPair
  -- * Lenses
  , uigpDescription
  , uigpGroupId
  , uigpGroupName
  , uigpPeeringStatus
  , uigpUserId
  , uigpVpcId
  , uigpVpcPeeringConnectionId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a security group and AWS account ID pair.
--
-- /See:/ 'mkUserIdGroupPair' smart constructor.
data UserIdGroupPair = UserIdGroupPair'
  { description :: Core.Maybe Core.Text
    -- ^ A description for the security group rule that references this user ID group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
  , groupId :: Core.Maybe Core.Text
    -- ^ The ID of the security group.
  , groupName :: Core.Maybe Core.Text
    -- ^ The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID. 
--
-- For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
  , peeringStatus :: Core.Maybe Core.Text
    -- ^ The status of a VPC peering connection, if applicable.
  , userId :: Core.Maybe Core.Text
    -- ^ The ID of an AWS account.
--
-- For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned.
-- [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC for the referenced security group, if applicable.
  , vpcPeeringConnectionId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC peering connection, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserIdGroupPair' value with any optional fields omitted.
mkUserIdGroupPair
    :: UserIdGroupPair
mkUserIdGroupPair
  = UserIdGroupPair'{description = Core.Nothing,
                     groupId = Core.Nothing, groupName = Core.Nothing,
                     peeringStatus = Core.Nothing, userId = Core.Nothing,
                     vpcId = Core.Nothing, vpcPeeringConnectionId = Core.Nothing}

-- | A description for the security group rule that references this user ID group pair.
--
-- Constraints: Up to 255 characters in length. Allowed characters are a-z, A-Z, 0-9, spaces, and ._-:/()#,@[]+=;{}!$*
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpDescription :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
uigpDescription = Lens.field @"description"
{-# INLINEABLE uigpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpGroupId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
uigpGroupId = Lens.field @"groupId"
{-# INLINEABLE uigpGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The name of the security group. In a request, use this parameter for a security group in EC2-Classic or a default VPC only. For a security group in a nondefault VPC, use the security group ID. 
--
-- For a referenced security group in another VPC, this value is not returned if the referenced security group is deleted.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpGroupName :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
uigpGroupName = Lens.field @"groupName"
{-# INLINEABLE uigpGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The status of a VPC peering connection, if applicable.
--
-- /Note:/ Consider using 'peeringStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpPeeringStatus :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
uigpPeeringStatus = Lens.field @"peeringStatus"
{-# INLINEABLE uigpPeeringStatus #-}
{-# DEPRECATED peeringStatus "Use generic-lens or generic-optics with 'peeringStatus' instead"  #-}

-- | The ID of an AWS account.
--
-- For a referenced security group in another VPC, the account ID of the referenced security group is returned in the response. If the referenced security group is deleted, this value is not returned.
-- [EC2-Classic] Required when adding or removing rules that reference a security group in another AWS account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpUserId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
uigpUserId = Lens.field @"userId"
{-# INLINEABLE uigpUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The ID of the VPC for the referenced security group, if applicable.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpVpcId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
uigpVpcId = Lens.field @"vpcId"
{-# INLINEABLE uigpVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The ID of the VPC peering connection, if applicable.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uigpVpcPeeringConnectionId :: Lens.Lens' UserIdGroupPair (Core.Maybe Core.Text)
uigpVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE uigpVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.ToQuery UserIdGroupPair where
        toQuery UserIdGroupPair{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Description")
              description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "GroupId") groupId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "GroupName") groupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PeeringStatus")
                peeringStatus
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "UserId") userId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "VpcId") vpcId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VpcPeeringConnectionId")
                vpcPeeringConnectionId

instance Core.FromXML UserIdGroupPair where
        parseXML x
          = UserIdGroupPair' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "groupId" Core.<*>
                x Core..@? "groupName"
                Core.<*> x Core..@? "peeringStatus"
                Core.<*> x Core..@? "userId"
                Core.<*> x Core..@? "vpcId"
                Core.<*> x Core..@? "vpcPeeringConnectionId"
