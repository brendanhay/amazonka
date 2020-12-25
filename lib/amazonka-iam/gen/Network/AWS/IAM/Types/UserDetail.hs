{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.UserDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.UserDetail
  ( UserDetail (..),

    -- * Smart constructor
    mkUserDetail,

    -- * Lenses
    udArn,
    udAttachedManagedPolicies,
    udCreateDate,
    udGroupList,
    udPath,
    udPermissionsBoundary,
    udTags,
    udUserId,
    udUserName,
    udUserPolicyList,
  )
where

import qualified Network.AWS.IAM.Types.ArnType as Types
import qualified Network.AWS.IAM.Types.AttachedPermissionsBoundary as Types
import qualified Network.AWS.IAM.Types.AttachedPolicy as Types
import qualified Network.AWS.IAM.Types.GroupNameType as Types
import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.PathType as Types
import qualified Network.AWS.IAM.Types.PolicyDetail as Types
import qualified Network.AWS.IAM.Types.Tag as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an IAM user, including all the user's policies and all the IAM groups the user is in.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkUserDetail' smart constructor.
data UserDetail = UserDetail'
  { arn :: Core.Maybe Types.ArnType,
    -- | A list of the managed policies attached to the user.
    attachedManagedPolicies :: Core.Maybe [Types.AttachedPolicy],
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | A list of IAM groups that the user is in.
    groupList :: Core.Maybe [Types.GroupNameType],
    -- | The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Core.Maybe Types.PathType,
    -- | The ARN of the policy used to set the permissions boundary for the user.
    --
    -- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
    permissionsBoundary :: Core.Maybe Types.AttachedPermissionsBoundary,
    -- | A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
    tags :: Core.Maybe [Types.Tag],
    -- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    userId :: Core.Maybe Types.IdType,
    -- | The friendly name identifying the user.
    userName :: Core.Maybe Types.UserName,
    -- | A list of the inline policies embedded in the user.
    userPolicyList :: Core.Maybe [Types.PolicyDetail]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UserDetail' value with any optional fields omitted.
mkUserDetail ::
  UserDetail
mkUserDetail =
  UserDetail'
    { arn = Core.Nothing,
      attachedManagedPolicies = Core.Nothing,
      createDate = Core.Nothing,
      groupList = Core.Nothing,
      path = Core.Nothing,
      permissionsBoundary = Core.Nothing,
      tags = Core.Nothing,
      userId = Core.Nothing,
      userName = Core.Nothing,
      userPolicyList = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udArn :: Lens.Lens' UserDetail (Core.Maybe Types.ArnType)
udArn = Lens.field @"arn"
{-# DEPRECATED udArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of the managed policies attached to the user.
--
-- /Note:/ Consider using 'attachedManagedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udAttachedManagedPolicies :: Lens.Lens' UserDetail (Core.Maybe [Types.AttachedPolicy])
udAttachedManagedPolicies = Lens.field @"attachedManagedPolicies"
{-# DEPRECATED udAttachedManagedPolicies "Use generic-lens or generic-optics with 'attachedManagedPolicies' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCreateDate :: Lens.Lens' UserDetail (Core.Maybe Core.UTCTime)
udCreateDate = Lens.field @"createDate"
{-# DEPRECATED udCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | A list of IAM groups that the user is in.
--
-- /Note:/ Consider using 'groupList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udGroupList :: Lens.Lens' UserDetail (Core.Maybe [Types.GroupNameType])
udGroupList = Lens.field @"groupList"
{-# DEPRECATED udGroupList "Use generic-lens or generic-optics with 'groupList' instead." #-}

-- | The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udPath :: Lens.Lens' UserDetail (Core.Maybe Types.PathType)
udPath = Lens.field @"path"
{-# DEPRECATED udPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udPermissionsBoundary :: Lens.Lens' UserDetail (Core.Maybe Types.AttachedPermissionsBoundary)
udPermissionsBoundary = Lens.field @"permissionsBoundary"
{-# DEPRECATED udPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udTags :: Lens.Lens' UserDetail (Core.Maybe [Types.Tag])
udTags = Lens.field @"tags"
{-# DEPRECATED udTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserId :: Lens.Lens' UserDetail (Core.Maybe Types.IdType)
udUserId = Lens.field @"userId"
{-# DEPRECATED udUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The friendly name identifying the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserName :: Lens.Lens' UserDetail (Core.Maybe Types.UserName)
udUserName = Lens.field @"userName"
{-# DEPRECATED udUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | A list of the inline policies embedded in the user.
--
-- /Note:/ Consider using 'userPolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserPolicyList :: Lens.Lens' UserDetail (Core.Maybe [Types.PolicyDetail])
udUserPolicyList = Lens.field @"userPolicyList"
{-# DEPRECATED udUserPolicyList "Use generic-lens or generic-optics with 'userPolicyList' instead." #-}

instance Core.FromXML UserDetail where
  parseXML x =
    UserDetail'
      Core.<$> (x Core..@? "Arn")
      Core.<*> ( x Core..@? "AttachedManagedPolicies"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "GroupList" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "Path")
      Core.<*> (x Core..@? "PermissionsBoundary")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "UserId")
      Core.<*> (x Core..@? "UserName")
      Core.<*> (x Core..@? "UserPolicyList" Core..<@> Core.parseXMLList "member")
