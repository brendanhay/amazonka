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
-- Module      : Amazonka.SecurityHub.Types.AwsIamUserDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamUserDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary
import Amazonka.SecurityHub.Types.AwsIamUserPolicy

-- | Information about an IAM user.
--
-- /See:/ 'newAwsIamUserDetails' smart constructor.
data AwsIamUserDetails = AwsIamUserDetails'
  { -- | The name of the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The path to the user.
    path :: Prelude.Maybe Prelude.Text,
    -- | A list of IAM groups that the user belongs to.
    groupList :: Prelude.Maybe [Prelude.Text],
    -- | A list of the managed policies that are attached to the user.
    attachedManagedPolicies :: Prelude.Maybe [AwsIamAttachedManagedPolicy],
    -- | The permissions boundary for the user.
    permissionsBoundary :: Prelude.Maybe AwsIamPermissionsBoundary,
    -- | The unique identifier for the user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the user was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The list of inline policies that are embedded in the user.
    userPolicyList :: Prelude.Maybe [AwsIamUserPolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamUserDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'awsIamUserDetails_userName' - The name of the user.
--
-- 'path', 'awsIamUserDetails_path' - The path to the user.
--
-- 'groupList', 'awsIamUserDetails_groupList' - A list of IAM groups that the user belongs to.
--
-- 'attachedManagedPolicies', 'awsIamUserDetails_attachedManagedPolicies' - A list of the managed policies that are attached to the user.
--
-- 'permissionsBoundary', 'awsIamUserDetails_permissionsBoundary' - The permissions boundary for the user.
--
-- 'userId', 'awsIamUserDetails_userId' - The unique identifier for the user.
--
-- 'createDate', 'awsIamUserDetails_createDate' - Indicates when the user was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'userPolicyList', 'awsIamUserDetails_userPolicyList' - The list of inline policies that are embedded in the user.
newAwsIamUserDetails ::
  AwsIamUserDetails
newAwsIamUserDetails =
  AwsIamUserDetails'
    { userName = Prelude.Nothing,
      path = Prelude.Nothing,
      groupList = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      userId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      userPolicyList = Prelude.Nothing
    }

-- | The name of the user.
awsIamUserDetails_userName :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_userName = Lens.lens (\AwsIamUserDetails' {userName} -> userName) (\s@AwsIamUserDetails' {} a -> s {userName = a} :: AwsIamUserDetails)

-- | The path to the user.
awsIamUserDetails_path :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_path = Lens.lens (\AwsIamUserDetails' {path} -> path) (\s@AwsIamUserDetails' {} a -> s {path = a} :: AwsIamUserDetails)

-- | A list of IAM groups that the user belongs to.
awsIamUserDetails_groupList :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe [Prelude.Text])
awsIamUserDetails_groupList = Lens.lens (\AwsIamUserDetails' {groupList} -> groupList) (\s@AwsIamUserDetails' {} a -> s {groupList = a} :: AwsIamUserDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of the managed policies that are attached to the user.
awsIamUserDetails_attachedManagedPolicies :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe [AwsIamAttachedManagedPolicy])
awsIamUserDetails_attachedManagedPolicies = Lens.lens (\AwsIamUserDetails' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@AwsIamUserDetails' {} a -> s {attachedManagedPolicies = a} :: AwsIamUserDetails) Prelude.. Lens.mapping Lens.coerced

-- | The permissions boundary for the user.
awsIamUserDetails_permissionsBoundary :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe AwsIamPermissionsBoundary)
awsIamUserDetails_permissionsBoundary = Lens.lens (\AwsIamUserDetails' {permissionsBoundary} -> permissionsBoundary) (\s@AwsIamUserDetails' {} a -> s {permissionsBoundary = a} :: AwsIamUserDetails)

-- | The unique identifier for the user.
awsIamUserDetails_userId :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_userId = Lens.lens (\AwsIamUserDetails' {userId} -> userId) (\s@AwsIamUserDetails' {} a -> s {userId = a} :: AwsIamUserDetails)

-- | Indicates when the user was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamUserDetails_createDate :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_createDate = Lens.lens (\AwsIamUserDetails' {createDate} -> createDate) (\s@AwsIamUserDetails' {} a -> s {createDate = a} :: AwsIamUserDetails)

-- | The list of inline policies that are embedded in the user.
awsIamUserDetails_userPolicyList :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe [AwsIamUserPolicy])
awsIamUserDetails_userPolicyList = Lens.lens (\AwsIamUserDetails' {userPolicyList} -> userPolicyList) (\s@AwsIamUserDetails' {} a -> s {userPolicyList = a} :: AwsIamUserDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsIamUserDetails where
  parseJSON =
    Core.withObject
      "AwsIamUserDetails"
      ( \x ->
          AwsIamUserDetails'
            Prelude.<$> (x Core..:? "UserName")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "GroupList" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "AttachedManagedPolicies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PermissionsBoundary")
            Prelude.<*> (x Core..:? "UserId")
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> ( x Core..:? "UserPolicyList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsIamUserDetails where
  hashWithSalt _salt AwsIamUserDetails' {..} =
    _salt `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` groupList
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` userPolicyList

instance Prelude.NFData AwsIamUserDetails where
  rnf AwsIamUserDetails' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf groupList
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf userPolicyList

instance Core.ToJSON AwsIamUserDetails where
  toJSON AwsIamUserDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UserName" Core..=) Prelude.<$> userName,
            ("Path" Core..=) Prelude.<$> path,
            ("GroupList" Core..=) Prelude.<$> groupList,
            ("AttachedManagedPolicies" Core..=)
              Prelude.<$> attachedManagedPolicies,
            ("PermissionsBoundary" Core..=)
              Prelude.<$> permissionsBoundary,
            ("UserId" Core..=) Prelude.<$> userId,
            ("CreateDate" Core..=) Prelude.<$> createDate,
            ("UserPolicyList" Core..=)
              Prelude.<$> userPolicyList
          ]
      )
