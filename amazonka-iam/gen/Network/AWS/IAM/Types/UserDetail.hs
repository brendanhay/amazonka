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
-- Module      : Network.AWS.IAM.Types.UserDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.UserDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Contains information about an IAM user, including all the user\'s
-- policies and all the IAM groups the user is in.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newUserDetail' smart constructor.
data UserDetail = UserDetail'
  { -- | A list of the managed policies attached to the user.
    attachedManagedPolicies :: Core.Maybe [AttachedPolicy],
    -- | The ARN of the policy used to set the permissions boundary for the user.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Core.Maybe AttachedPermissionsBoundary,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- user was created.
    createDate :: Core.Maybe Core.ISO8601,
    arn :: Core.Maybe Core.Text,
    -- | A list of IAM groups that the user is in.
    groupList :: Core.Maybe [Core.Text],
    -- | The stable and unique string identifying the user. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    userId :: Core.Maybe Core.Text,
    -- | A list of tags that are associated with the user. For more information
    -- about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Core.Maybe [Tag],
    -- | The friendly name identifying the user.
    userName :: Core.Maybe Core.Text,
    -- | A list of the inline policies embedded in the user.
    userPolicyList :: Core.Maybe [PolicyDetail],
    -- | The path to the user. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedManagedPolicies', 'userDetail_attachedManagedPolicies' - A list of the managed policies attached to the user.
--
-- 'permissionsBoundary', 'userDetail_permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'createDate', 'userDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
--
-- 'arn', 'userDetail_arn' - Undocumented member.
--
-- 'groupList', 'userDetail_groupList' - A list of IAM groups that the user is in.
--
-- 'userId', 'userDetail_userId' - The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'tags', 'userDetail_tags' - A list of tags that are associated with the user. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'userName', 'userDetail_userName' - The friendly name identifying the user.
--
-- 'userPolicyList', 'userDetail_userPolicyList' - A list of the inline policies embedded in the user.
--
-- 'path', 'userDetail_path' - The path to the user. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
newUserDetail ::
  UserDetail
newUserDetail =
  UserDetail'
    { attachedManagedPolicies = Core.Nothing,
      permissionsBoundary = Core.Nothing,
      createDate = Core.Nothing,
      arn = Core.Nothing,
      groupList = Core.Nothing,
      userId = Core.Nothing,
      tags = Core.Nothing,
      userName = Core.Nothing,
      userPolicyList = Core.Nothing,
      path = Core.Nothing
    }

-- | A list of the managed policies attached to the user.
userDetail_attachedManagedPolicies :: Lens.Lens' UserDetail (Core.Maybe [AttachedPolicy])
userDetail_attachedManagedPolicies = Lens.lens (\UserDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@UserDetail' {} a -> s {attachedManagedPolicies = a} :: UserDetail) Core.. Lens.mapping Lens._Coerce

-- | The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
userDetail_permissionsBoundary :: Lens.Lens' UserDetail (Core.Maybe AttachedPermissionsBoundary)
userDetail_permissionsBoundary = Lens.lens (\UserDetail' {permissionsBoundary} -> permissionsBoundary) (\s@UserDetail' {} a -> s {permissionsBoundary = a} :: UserDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
userDetail_createDate :: Lens.Lens' UserDetail (Core.Maybe Core.UTCTime)
userDetail_createDate = Lens.lens (\UserDetail' {createDate} -> createDate) (\s@UserDetail' {} a -> s {createDate = a} :: UserDetail) Core.. Lens.mapping Core._Time

-- | Undocumented member.
userDetail_arn :: Lens.Lens' UserDetail (Core.Maybe Core.Text)
userDetail_arn = Lens.lens (\UserDetail' {arn} -> arn) (\s@UserDetail' {} a -> s {arn = a} :: UserDetail)

-- | A list of IAM groups that the user is in.
userDetail_groupList :: Lens.Lens' UserDetail (Core.Maybe [Core.Text])
userDetail_groupList = Lens.lens (\UserDetail' {groupList} -> groupList) (\s@UserDetail' {} a -> s {groupList = a} :: UserDetail) Core.. Lens.mapping Lens._Coerce

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
userDetail_userId :: Lens.Lens' UserDetail (Core.Maybe Core.Text)
userDetail_userId = Lens.lens (\UserDetail' {userId} -> userId) (\s@UserDetail' {} a -> s {userId = a} :: UserDetail)

-- | A list of tags that are associated with the user. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
userDetail_tags :: Lens.Lens' UserDetail (Core.Maybe [Tag])
userDetail_tags = Lens.lens (\UserDetail' {tags} -> tags) (\s@UserDetail' {} a -> s {tags = a} :: UserDetail) Core.. Lens.mapping Lens._Coerce

-- | The friendly name identifying the user.
userDetail_userName :: Lens.Lens' UserDetail (Core.Maybe Core.Text)
userDetail_userName = Lens.lens (\UserDetail' {userName} -> userName) (\s@UserDetail' {} a -> s {userName = a} :: UserDetail)

-- | A list of the inline policies embedded in the user.
userDetail_userPolicyList :: Lens.Lens' UserDetail (Core.Maybe [PolicyDetail])
userDetail_userPolicyList = Lens.lens (\UserDetail' {userPolicyList} -> userPolicyList) (\s@UserDetail' {} a -> s {userPolicyList = a} :: UserDetail) Core.. Lens.mapping Lens._Coerce

-- | The path to the user. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
userDetail_path :: Lens.Lens' UserDetail (Core.Maybe Core.Text)
userDetail_path = Lens.lens (\UserDetail' {path} -> path) (\s@UserDetail' {} a -> s {path = a} :: UserDetail)

instance Core.FromXML UserDetail where
  parseXML x =
    UserDetail'
      Core.<$> ( x Core..@? "AttachedManagedPolicies"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "PermissionsBoundary")
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "Arn")
      Core.<*> ( x Core..@? "GroupList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "UserId")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "UserName")
      Core.<*> ( x Core..@? "UserPolicyList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Path")

instance Core.Hashable UserDetail

instance Core.NFData UserDetail
