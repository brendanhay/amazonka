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
-- Module      : Amazonka.IAM.Types.UserDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.UserDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.AttachedPermissionsBoundary
import Amazonka.IAM.Types.AttachedPolicy
import Amazonka.IAM.Types.PolicyDetail
import Amazonka.IAM.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an IAM user, including all the user\'s
-- policies and all the IAM groups the user is in.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newUserDetail' smart constructor.
data UserDetail = UserDetail'
  { -- | A list of tags that are associated with the user. For more information
    -- about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The friendly name identifying the user.
    userName :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    -- | The path to the user. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | A list of IAM groups that the user is in.
    groupList :: Prelude.Maybe [Prelude.Text],
    -- | A list of the managed policies attached to the user.
    attachedManagedPolicies :: Prelude.Maybe [AttachedPolicy],
    -- | The ARN of the policy used to set the permissions boundary for the user.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Prelude.Maybe AttachedPermissionsBoundary,
    -- | The stable and unique string identifying the user. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- user was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | A list of the inline policies embedded in the user.
    userPolicyList :: Prelude.Maybe [PolicyDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'userDetail_tags' - A list of tags that are associated with the user. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'userName', 'userDetail_userName' - The friendly name identifying the user.
--
-- 'arn', 'userDetail_arn' - Undocumented member.
--
-- 'path', 'userDetail_path' - The path to the user. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'groupList', 'userDetail_groupList' - A list of IAM groups that the user is in.
--
-- 'attachedManagedPolicies', 'userDetail_attachedManagedPolicies' - A list of the managed policies attached to the user.
--
-- 'permissionsBoundary', 'userDetail_permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'userId', 'userDetail_userId' - The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'createDate', 'userDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
--
-- 'userPolicyList', 'userDetail_userPolicyList' - A list of the inline policies embedded in the user.
newUserDetail ::
  UserDetail
newUserDetail =
  UserDetail'
    { tags = Prelude.Nothing,
      userName = Prelude.Nothing,
      arn = Prelude.Nothing,
      path = Prelude.Nothing,
      groupList = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      userId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      userPolicyList = Prelude.Nothing
    }

-- | A list of tags that are associated with the user. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
userDetail_tags :: Lens.Lens' UserDetail (Prelude.Maybe [Tag])
userDetail_tags = Lens.lens (\UserDetail' {tags} -> tags) (\s@UserDetail' {} a -> s {tags = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

-- | The friendly name identifying the user.
userDetail_userName :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_userName = Lens.lens (\UserDetail' {userName} -> userName) (\s@UserDetail' {} a -> s {userName = a} :: UserDetail)

-- | Undocumented member.
userDetail_arn :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_arn = Lens.lens (\UserDetail' {arn} -> arn) (\s@UserDetail' {} a -> s {arn = a} :: UserDetail)

-- | The path to the user. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
userDetail_path :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_path = Lens.lens (\UserDetail' {path} -> path) (\s@UserDetail' {} a -> s {path = a} :: UserDetail)

-- | A list of IAM groups that the user is in.
userDetail_groupList :: Lens.Lens' UserDetail (Prelude.Maybe [Prelude.Text])
userDetail_groupList = Lens.lens (\UserDetail' {groupList} -> groupList) (\s@UserDetail' {} a -> s {groupList = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

-- | A list of the managed policies attached to the user.
userDetail_attachedManagedPolicies :: Lens.Lens' UserDetail (Prelude.Maybe [AttachedPolicy])
userDetail_attachedManagedPolicies = Lens.lens (\UserDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@UserDetail' {} a -> s {attachedManagedPolicies = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
userDetail_permissionsBoundary :: Lens.Lens' UserDetail (Prelude.Maybe AttachedPermissionsBoundary)
userDetail_permissionsBoundary = Lens.lens (\UserDetail' {permissionsBoundary} -> permissionsBoundary) (\s@UserDetail' {} a -> s {permissionsBoundary = a} :: UserDetail)

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
userDetail_userId :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_userId = Lens.lens (\UserDetail' {userId} -> userId) (\s@UserDetail' {} a -> s {userId = a} :: UserDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
userDetail_createDate :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.UTCTime)
userDetail_createDate = Lens.lens (\UserDetail' {createDate} -> createDate) (\s@UserDetail' {} a -> s {createDate = a} :: UserDetail) Prelude.. Lens.mapping Data._Time

-- | A list of the inline policies embedded in the user.
userDetail_userPolicyList :: Lens.Lens' UserDetail (Prelude.Maybe [PolicyDetail])
userDetail_userPolicyList = Lens.lens (\UserDetail' {userPolicyList} -> userPolicyList) (\s@UserDetail' {} a -> s {userPolicyList = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML UserDetail where
  parseXML x =
    UserDetail'
      Prelude.<$> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "UserName")
      Prelude.<*> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "Path")
      Prelude.<*> ( x Data..@? "GroupList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x Data..@? "AttachedManagedPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "PermissionsBoundary")
      Prelude.<*> (x Data..@? "UserId")
      Prelude.<*> (x Data..@? "CreateDate")
      Prelude.<*> ( x Data..@? "UserPolicyList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable UserDetail where
  hashWithSalt _salt UserDetail' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` groupList
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` userPolicyList

instance Prelude.NFData UserDetail where
  rnf UserDetail' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf groupList
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf userPolicyList
