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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamUserDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary
import Amazonka.SecurityHub.Types.AwsIamUserPolicy

-- | Information about an IAM user.
--
-- /See:/ 'newAwsIamUserDetails' smart constructor.
data AwsIamUserDetails = AwsIamUserDetails'
  { -- | A list of the managed policies that are attached to the user.
    attachedManagedPolicies :: Prelude.Maybe [AwsIamAttachedManagedPolicy],
    -- | Indicates when the user was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | A list of IAM groups that the user belongs to.
    groupList :: Prelude.Maybe [Prelude.Text],
    -- | The path to the user.
    path :: Prelude.Maybe Prelude.Text,
    -- | The permissions boundary for the user.
    permissionsBoundary :: Prelude.Maybe AwsIamPermissionsBoundary,
    -- | The unique identifier for the user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user.
    userName :: Prelude.Maybe Prelude.Text,
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
-- 'attachedManagedPolicies', 'awsIamUserDetails_attachedManagedPolicies' - A list of the managed policies that are attached to the user.
--
-- 'createDate', 'awsIamUserDetails_createDate' - Indicates when the user was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
--
-- 'groupList', 'awsIamUserDetails_groupList' - A list of IAM groups that the user belongs to.
--
-- 'path', 'awsIamUserDetails_path' - The path to the user.
--
-- 'permissionsBoundary', 'awsIamUserDetails_permissionsBoundary' - The permissions boundary for the user.
--
-- 'userId', 'awsIamUserDetails_userId' - The unique identifier for the user.
--
-- 'userName', 'awsIamUserDetails_userName' - The name of the user.
--
-- 'userPolicyList', 'awsIamUserDetails_userPolicyList' - The list of inline policies that are embedded in the user.
newAwsIamUserDetails ::
  AwsIamUserDetails
newAwsIamUserDetails =
  AwsIamUserDetails'
    { attachedManagedPolicies =
        Prelude.Nothing,
      createDate = Prelude.Nothing,
      groupList = Prelude.Nothing,
      path = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      userId = Prelude.Nothing,
      userName = Prelude.Nothing,
      userPolicyList = Prelude.Nothing
    }

-- | A list of the managed policies that are attached to the user.
awsIamUserDetails_attachedManagedPolicies :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe [AwsIamAttachedManagedPolicy])
awsIamUserDetails_attachedManagedPolicies = Lens.lens (\AwsIamUserDetails' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@AwsIamUserDetails' {} a -> s {attachedManagedPolicies = a} :: AwsIamUserDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the user was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsIamUserDetails_createDate :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_createDate = Lens.lens (\AwsIamUserDetails' {createDate} -> createDate) (\s@AwsIamUserDetails' {} a -> s {createDate = a} :: AwsIamUserDetails)

-- | A list of IAM groups that the user belongs to.
awsIamUserDetails_groupList :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe [Prelude.Text])
awsIamUserDetails_groupList = Lens.lens (\AwsIamUserDetails' {groupList} -> groupList) (\s@AwsIamUserDetails' {} a -> s {groupList = a} :: AwsIamUserDetails) Prelude.. Lens.mapping Lens.coerced

-- | The path to the user.
awsIamUserDetails_path :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_path = Lens.lens (\AwsIamUserDetails' {path} -> path) (\s@AwsIamUserDetails' {} a -> s {path = a} :: AwsIamUserDetails)

-- | The permissions boundary for the user.
awsIamUserDetails_permissionsBoundary :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe AwsIamPermissionsBoundary)
awsIamUserDetails_permissionsBoundary = Lens.lens (\AwsIamUserDetails' {permissionsBoundary} -> permissionsBoundary) (\s@AwsIamUserDetails' {} a -> s {permissionsBoundary = a} :: AwsIamUserDetails)

-- | The unique identifier for the user.
awsIamUserDetails_userId :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_userId = Lens.lens (\AwsIamUserDetails' {userId} -> userId) (\s@AwsIamUserDetails' {} a -> s {userId = a} :: AwsIamUserDetails)

-- | The name of the user.
awsIamUserDetails_userName :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe Prelude.Text)
awsIamUserDetails_userName = Lens.lens (\AwsIamUserDetails' {userName} -> userName) (\s@AwsIamUserDetails' {} a -> s {userName = a} :: AwsIamUserDetails)

-- | The list of inline policies that are embedded in the user.
awsIamUserDetails_userPolicyList :: Lens.Lens' AwsIamUserDetails (Prelude.Maybe [AwsIamUserPolicy])
awsIamUserDetails_userPolicyList = Lens.lens (\AwsIamUserDetails' {userPolicyList} -> userPolicyList) (\s@AwsIamUserDetails' {} a -> s {userPolicyList = a} :: AwsIamUserDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsIamUserDetails where
  parseJSON =
    Data.withObject
      "AwsIamUserDetails"
      ( \x ->
          AwsIamUserDetails'
            Prelude.<$> ( x
                            Data..:? "AttachedManagedPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "GroupList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "PermissionsBoundary")
            Prelude.<*> (x Data..:? "UserId")
            Prelude.<*> (x Data..:? "UserName")
            Prelude.<*> ( x
                            Data..:? "UserPolicyList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsIamUserDetails where
  hashWithSalt _salt AwsIamUserDetails' {..} =
    _salt
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` groupList
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` userPolicyList

instance Prelude.NFData AwsIamUserDetails where
  rnf AwsIamUserDetails' {..} =
    Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf groupList
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf userPolicyList

instance Data.ToJSON AwsIamUserDetails where
  toJSON AwsIamUserDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttachedManagedPolicies" Data..=)
              Prelude.<$> attachedManagedPolicies,
            ("CreateDate" Data..=) Prelude.<$> createDate,
            ("GroupList" Data..=) Prelude.<$> groupList,
            ("Path" Data..=) Prelude.<$> path,
            ("PermissionsBoundary" Data..=)
              Prelude.<$> permissionsBoundary,
            ("UserId" Data..=) Prelude.<$> userId,
            ("UserName" Data..=) Prelude.<$> userName,
            ("UserPolicyList" Data..=)
              Prelude.<$> userPolicyList
          ]
      )
