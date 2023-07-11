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
-- Module      : Amazonka.SecurityHub.Types.AwsIamRoleDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamRoleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamInstanceProfile
import Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary
import Amazonka.SecurityHub.Types.AwsIamRolePolicy

-- | Contains information about an IAM role, including all of the role\'s
-- policies.
--
-- /See:/ 'newAwsIamRoleDetails' smart constructor.
data AwsIamRoleDetails = AwsIamRoleDetails'
  { -- | The trust policy that grants permission to assume the role.
    assumeRolePolicyDocument :: Prelude.Maybe Prelude.Text,
    -- | The list of the managed policies that are attached to the role.
    attachedManagedPolicies :: Prelude.Maybe [AwsIamAttachedManagedPolicy],
    -- | Indicates when the role was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The list of instance profiles that contain this role.
    instanceProfileList :: Prelude.Maybe [AwsIamInstanceProfile],
    -- | The maximum session duration (in seconds) that you want to set for the
    -- specified role.
    maxSessionDuration :: Prelude.Maybe Prelude.Int,
    -- | The path to the role.
    path :: Prelude.Maybe Prelude.Text,
    permissionsBoundary :: Prelude.Maybe AwsIamPermissionsBoundary,
    -- | The stable and unique string identifying the role.
    roleId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name that identifies the role.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The list of inline policies that are embedded in the role.
    rolePolicyList :: Prelude.Maybe [AwsIamRolePolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamRoleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assumeRolePolicyDocument', 'awsIamRoleDetails_assumeRolePolicyDocument' - The trust policy that grants permission to assume the role.
--
-- 'attachedManagedPolicies', 'awsIamRoleDetails_attachedManagedPolicies' - The list of the managed policies that are attached to the role.
--
-- 'createDate', 'awsIamRoleDetails_createDate' - Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'instanceProfileList', 'awsIamRoleDetails_instanceProfileList' - The list of instance profiles that contain this role.
--
-- 'maxSessionDuration', 'awsIamRoleDetails_maxSessionDuration' - The maximum session duration (in seconds) that you want to set for the
-- specified role.
--
-- 'path', 'awsIamRoleDetails_path' - The path to the role.
--
-- 'permissionsBoundary', 'awsIamRoleDetails_permissionsBoundary' - Undocumented member.
--
-- 'roleId', 'awsIamRoleDetails_roleId' - The stable and unique string identifying the role.
--
-- 'roleName', 'awsIamRoleDetails_roleName' - The friendly name that identifies the role.
--
-- 'rolePolicyList', 'awsIamRoleDetails_rolePolicyList' - The list of inline policies that are embedded in the role.
newAwsIamRoleDetails ::
  AwsIamRoleDetails
newAwsIamRoleDetails =
  AwsIamRoleDetails'
    { assumeRolePolicyDocument =
        Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing,
      createDate = Prelude.Nothing,
      instanceProfileList = Prelude.Nothing,
      maxSessionDuration = Prelude.Nothing,
      path = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      roleId = Prelude.Nothing,
      roleName = Prelude.Nothing,
      rolePolicyList = Prelude.Nothing
    }

-- | The trust policy that grants permission to assume the role.
awsIamRoleDetails_assumeRolePolicyDocument :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_assumeRolePolicyDocument = Lens.lens (\AwsIamRoleDetails' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@AwsIamRoleDetails' {} a -> s {assumeRolePolicyDocument = a} :: AwsIamRoleDetails)

-- | The list of the managed policies that are attached to the role.
awsIamRoleDetails_attachedManagedPolicies :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe [AwsIamAttachedManagedPolicy])
awsIamRoleDetails_attachedManagedPolicies = Lens.lens (\AwsIamRoleDetails' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@AwsIamRoleDetails' {} a -> s {attachedManagedPolicies = a} :: AwsIamRoleDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamRoleDetails_createDate :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_createDate = Lens.lens (\AwsIamRoleDetails' {createDate} -> createDate) (\s@AwsIamRoleDetails' {} a -> s {createDate = a} :: AwsIamRoleDetails)

-- | The list of instance profiles that contain this role.
awsIamRoleDetails_instanceProfileList :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe [AwsIamInstanceProfile])
awsIamRoleDetails_instanceProfileList = Lens.lens (\AwsIamRoleDetails' {instanceProfileList} -> instanceProfileList) (\s@AwsIamRoleDetails' {} a -> s {instanceProfileList = a} :: AwsIamRoleDetails) Prelude.. Lens.mapping Lens.coerced

-- | The maximum session duration (in seconds) that you want to set for the
-- specified role.
awsIamRoleDetails_maxSessionDuration :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Int)
awsIamRoleDetails_maxSessionDuration = Lens.lens (\AwsIamRoleDetails' {maxSessionDuration} -> maxSessionDuration) (\s@AwsIamRoleDetails' {} a -> s {maxSessionDuration = a} :: AwsIamRoleDetails)

-- | The path to the role.
awsIamRoleDetails_path :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_path = Lens.lens (\AwsIamRoleDetails' {path} -> path) (\s@AwsIamRoleDetails' {} a -> s {path = a} :: AwsIamRoleDetails)

-- | Undocumented member.
awsIamRoleDetails_permissionsBoundary :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe AwsIamPermissionsBoundary)
awsIamRoleDetails_permissionsBoundary = Lens.lens (\AwsIamRoleDetails' {permissionsBoundary} -> permissionsBoundary) (\s@AwsIamRoleDetails' {} a -> s {permissionsBoundary = a} :: AwsIamRoleDetails)

-- | The stable and unique string identifying the role.
awsIamRoleDetails_roleId :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_roleId = Lens.lens (\AwsIamRoleDetails' {roleId} -> roleId) (\s@AwsIamRoleDetails' {} a -> s {roleId = a} :: AwsIamRoleDetails)

-- | The friendly name that identifies the role.
awsIamRoleDetails_roleName :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_roleName = Lens.lens (\AwsIamRoleDetails' {roleName} -> roleName) (\s@AwsIamRoleDetails' {} a -> s {roleName = a} :: AwsIamRoleDetails)

-- | The list of inline policies that are embedded in the role.
awsIamRoleDetails_rolePolicyList :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe [AwsIamRolePolicy])
awsIamRoleDetails_rolePolicyList = Lens.lens (\AwsIamRoleDetails' {rolePolicyList} -> rolePolicyList) (\s@AwsIamRoleDetails' {} a -> s {rolePolicyList = a} :: AwsIamRoleDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsIamRoleDetails where
  parseJSON =
    Data.withObject
      "AwsIamRoleDetails"
      ( \x ->
          AwsIamRoleDetails'
            Prelude.<$> (x Data..:? "AssumeRolePolicyDocument")
            Prelude.<*> ( x
                            Data..:? "AttachedManagedPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> ( x
                            Data..:? "InstanceProfileList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MaxSessionDuration")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "PermissionsBoundary")
            Prelude.<*> (x Data..:? "RoleId")
            Prelude.<*> (x Data..:? "RoleName")
            Prelude.<*> ( x
                            Data..:? "RolePolicyList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsIamRoleDetails where
  hashWithSalt _salt AwsIamRoleDetails' {..} =
    _salt
      `Prelude.hashWithSalt` assumeRolePolicyDocument
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` instanceProfileList
      `Prelude.hashWithSalt` maxSessionDuration
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` roleId
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` rolePolicyList

instance Prelude.NFData AwsIamRoleDetails where
  rnf AwsIamRoleDetails' {..} =
    Prelude.rnf assumeRolePolicyDocument
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf instanceProfileList
      `Prelude.seq` Prelude.rnf maxSessionDuration
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf roleId
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf rolePolicyList

instance Data.ToJSON AwsIamRoleDetails where
  toJSON AwsIamRoleDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssumeRolePolicyDocument" Data..=)
              Prelude.<$> assumeRolePolicyDocument,
            ("AttachedManagedPolicies" Data..=)
              Prelude.<$> attachedManagedPolicies,
            ("CreateDate" Data..=) Prelude.<$> createDate,
            ("InstanceProfileList" Data..=)
              Prelude.<$> instanceProfileList,
            ("MaxSessionDuration" Data..=)
              Prelude.<$> maxSessionDuration,
            ("Path" Data..=) Prelude.<$> path,
            ("PermissionsBoundary" Data..=)
              Prelude.<$> permissionsBoundary,
            ("RoleId" Data..=) Prelude.<$> roleId,
            ("RoleName" Data..=) Prelude.<$> roleName,
            ("RolePolicyList" Data..=)
              Prelude.<$> rolePolicyList
          ]
      )
