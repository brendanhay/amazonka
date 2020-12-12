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
    udGroupList,
    udARN,
    udPath,
    udCreateDate,
    udUserName,
    udUserId,
    udPermissionsBoundary,
    udUserPolicyList,
    udTags,
    udAttachedManagedPolicies,
  )
where

import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an IAM user, including all the user's policies and all the IAM groups the user is in.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkUserDetail' smart constructor.
data UserDetail = UserDetail'
  { groupList :: Lude.Maybe [Lude.Text],
    arn :: Lude.Maybe Lude.Text,
    path :: Lude.Maybe Lude.Text,
    createDate :: Lude.Maybe Lude.DateTime,
    userName :: Lude.Maybe Lude.Text,
    userId :: Lude.Maybe Lude.Text,
    permissionsBoundary :: Lude.Maybe AttachedPermissionsBoundary,
    userPolicyList :: Lude.Maybe [PolicyDetail],
    tags :: Lude.Maybe [Tag],
    attachedManagedPolicies :: Lude.Maybe [AttachedPolicy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserDetail' with the minimum fields required to make a request.
--
-- * 'arn' - Undocumented field.
-- * 'attachedManagedPolicies' - A list of the managed policies attached to the user.
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
-- * 'groupList' - A list of IAM groups that the user is in.
-- * 'path' - The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
-- * 'tags' - A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
-- * 'userId' - The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'userName' - The friendly name identifying the user.
-- * 'userPolicyList' - A list of the inline policies embedded in the user.
mkUserDetail ::
  UserDetail
mkUserDetail =
  UserDetail'
    { groupList = Lude.Nothing,
      arn = Lude.Nothing,
      path = Lude.Nothing,
      createDate = Lude.Nothing,
      userName = Lude.Nothing,
      userId = Lude.Nothing,
      permissionsBoundary = Lude.Nothing,
      userPolicyList = Lude.Nothing,
      tags = Lude.Nothing,
      attachedManagedPolicies = Lude.Nothing
    }

-- | A list of IAM groups that the user is in.
--
-- /Note:/ Consider using 'groupList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udGroupList :: Lens.Lens' UserDetail (Lude.Maybe [Lude.Text])
udGroupList = Lens.lens (groupList :: UserDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {groupList = a} :: UserDetail)
{-# DEPRECATED udGroupList "Use generic-lens or generic-optics with 'groupList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udARN :: Lens.Lens' UserDetail (Lude.Maybe Lude.Text)
udARN = Lens.lens (arn :: UserDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UserDetail)
{-# DEPRECATED udARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udPath :: Lens.Lens' UserDetail (Lude.Maybe Lude.Text)
udPath = Lens.lens (path :: UserDetail -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: UserDetail)
{-# DEPRECATED udPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCreateDate :: Lens.Lens' UserDetail (Lude.Maybe Lude.DateTime)
udCreateDate = Lens.lens (createDate :: UserDetail -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: UserDetail)
{-# DEPRECATED udCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The friendly name identifying the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserName :: Lens.Lens' UserDetail (Lude.Maybe Lude.Text)
udUserName = Lens.lens (userName :: UserDetail -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: UserDetail)
{-# DEPRECATED udUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserId :: Lens.Lens' UserDetail (Lude.Maybe Lude.Text)
udUserId = Lens.lens (userId :: UserDetail -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: UserDetail)
{-# DEPRECATED udUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udPermissionsBoundary :: Lens.Lens' UserDetail (Lude.Maybe AttachedPermissionsBoundary)
udPermissionsBoundary = Lens.lens (permissionsBoundary :: UserDetail -> Lude.Maybe AttachedPermissionsBoundary) (\s a -> s {permissionsBoundary = a} :: UserDetail)
{-# DEPRECATED udPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A list of the inline policies embedded in the user.
--
-- /Note:/ Consider using 'userPolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUserPolicyList :: Lens.Lens' UserDetail (Lude.Maybe [PolicyDetail])
udUserPolicyList = Lens.lens (userPolicyList :: UserDetail -> Lude.Maybe [PolicyDetail]) (\s a -> s {userPolicyList = a} :: UserDetail)
{-# DEPRECATED udUserPolicyList "Use generic-lens or generic-optics with 'userPolicyList' instead." #-}

-- | A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udTags :: Lens.Lens' UserDetail (Lude.Maybe [Tag])
udTags = Lens.lens (tags :: UserDetail -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UserDetail)
{-# DEPRECATED udTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A list of the managed policies attached to the user.
--
-- /Note:/ Consider using 'attachedManagedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udAttachedManagedPolicies :: Lens.Lens' UserDetail (Lude.Maybe [AttachedPolicy])
udAttachedManagedPolicies = Lens.lens (attachedManagedPolicies :: UserDetail -> Lude.Maybe [AttachedPolicy]) (\s a -> s {attachedManagedPolicies = a} :: UserDetail)
{-# DEPRECATED udAttachedManagedPolicies "Use generic-lens or generic-optics with 'attachedManagedPolicies' instead." #-}

instance Lude.FromXML UserDetail where
  parseXML x =
    UserDetail'
      Lude.<$> ( x Lude..@? "GroupList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Arn")
      Lude.<*> (x Lude..@? "Path")
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "UserName")
      Lude.<*> (x Lude..@? "UserId")
      Lude.<*> (x Lude..@? "PermissionsBoundary")
      Lude.<*> ( x Lude..@? "UserPolicyList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "AttachedManagedPolicies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
