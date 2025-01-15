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
-- Module      : Amazonka.IAM.Types.Role
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.Role where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.AttachedPermissionsBoundary
import Amazonka.IAM.Types.RoleLastUsed
import Amazonka.IAM.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an IAM role. This structure is returned as a
-- response element in several API operations that interact with roles.
--
-- /See:/ 'newRole' smart constructor.
data Role = Role'
  { -- | The policy that grants an entity permission to assume the role.
    assumeRolePolicyDocument :: Prelude.Maybe Prelude.Text,
    -- | A description of the role that you provide.
    description :: Prelude.Maybe Prelude.Text,
    -- | The maximum session duration (in seconds) for the specified role. Anyone
    -- who uses the CLI, or API to assume the role can specify the duration
    -- using the optional @DurationSeconds@ API parameter or @duration-seconds@
    -- CLI parameter.
    maxSessionDuration :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the policy used to set the permissions boundary for the role.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Prelude.Maybe AttachedPermissionsBoundary,
    -- | Contains information about the last time that an IAM role was used. This
    -- includes the date and time and the Region in which the role was last
    -- used. Activity is only reported for the trailing 400 days. This period
    -- can be shorter if your Region began supporting these features within the
    -- last year. The role might have been used more than 400 days ago. For
    -- more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
    -- in the /IAM User Guide/.
    roleLastUsed :: Prelude.Maybe RoleLastUsed,
    -- | A list of tags that are attached to the role. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The path to the role. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Text,
    -- | The friendly name that identifies the role.
    roleName :: Prelude.Text,
    -- | The stable and unique string identifying the role. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    roleId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) specifying the role. For more information
    -- about ARNs and how to use them in policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/ guide.
    arn :: Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- role was created.
    createDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Role' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assumeRolePolicyDocument', 'role_assumeRolePolicyDocument' - The policy that grants an entity permission to assume the role.
--
-- 'description', 'role_description' - A description of the role that you provide.
--
-- 'maxSessionDuration', 'role_maxSessionDuration' - The maximum session duration (in seconds) for the specified role. Anyone
-- who uses the CLI, or API to assume the role can specify the duration
-- using the optional @DurationSeconds@ API parameter or @duration-seconds@
-- CLI parameter.
--
-- 'permissionsBoundary', 'role_permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'roleLastUsed', 'role_roleLastUsed' - Contains information about the last time that an IAM role was used. This
-- includes the date and time and the Region in which the role was last
-- used. Activity is only reported for the trailing 400 days. This period
-- can be shorter if your Region began supporting these features within the
-- last year. The role might have been used more than 400 days ago. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
--
-- 'tags', 'role_tags' - A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'path', 'role_path' - The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'roleName', 'role_roleName' - The friendly name that identifies the role.
--
-- 'roleId', 'role_roleId' - The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'arn', 'role_arn' - The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/ guide.
--
-- 'createDate', 'role_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
newRole ::
  -- | 'path'
  Prelude.Text ->
  -- | 'roleName'
  Prelude.Text ->
  -- | 'roleId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createDate'
  Prelude.UTCTime ->
  Role
newRole pPath_ pRoleName_ pRoleId_ pArn_ pCreateDate_ =
  Role'
    { assumeRolePolicyDocument = Prelude.Nothing,
      description = Prelude.Nothing,
      maxSessionDuration = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      roleLastUsed = Prelude.Nothing,
      tags = Prelude.Nothing,
      path = pPath_,
      roleName = pRoleName_,
      roleId = pRoleId_,
      arn = pArn_,
      createDate = Data._Time Lens.# pCreateDate_
    }

-- | The policy that grants an entity permission to assume the role.
role_assumeRolePolicyDocument :: Lens.Lens' Role (Prelude.Maybe Prelude.Text)
role_assumeRolePolicyDocument = Lens.lens (\Role' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@Role' {} a -> s {assumeRolePolicyDocument = a} :: Role)

-- | A description of the role that you provide.
role_description :: Lens.Lens' Role (Prelude.Maybe Prelude.Text)
role_description = Lens.lens (\Role' {description} -> description) (\s@Role' {} a -> s {description = a} :: Role)

-- | The maximum session duration (in seconds) for the specified role. Anyone
-- who uses the CLI, or API to assume the role can specify the duration
-- using the optional @DurationSeconds@ API parameter or @duration-seconds@
-- CLI parameter.
role_maxSessionDuration :: Lens.Lens' Role (Prelude.Maybe Prelude.Natural)
role_maxSessionDuration = Lens.lens (\Role' {maxSessionDuration} -> maxSessionDuration) (\s@Role' {} a -> s {maxSessionDuration = a} :: Role)

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
role_permissionsBoundary :: Lens.Lens' Role (Prelude.Maybe AttachedPermissionsBoundary)
role_permissionsBoundary = Lens.lens (\Role' {permissionsBoundary} -> permissionsBoundary) (\s@Role' {} a -> s {permissionsBoundary = a} :: Role)

-- | Contains information about the last time that an IAM role was used. This
-- includes the date and time and the Region in which the role was last
-- used. Activity is only reported for the trailing 400 days. This period
-- can be shorter if your Region began supporting these features within the
-- last year. The role might have been used more than 400 days ago. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
role_roleLastUsed :: Lens.Lens' Role (Prelude.Maybe RoleLastUsed)
role_roleLastUsed = Lens.lens (\Role' {roleLastUsed} -> roleLastUsed) (\s@Role' {} a -> s {roleLastUsed = a} :: Role)

-- | A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
role_tags :: Lens.Lens' Role (Prelude.Maybe [Tag])
role_tags = Lens.lens (\Role' {tags} -> tags) (\s@Role' {} a -> s {tags = a} :: Role) Prelude.. Lens.mapping Lens.coerced

-- | The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
role_path :: Lens.Lens' Role Prelude.Text
role_path = Lens.lens (\Role' {path} -> path) (\s@Role' {} a -> s {path = a} :: Role)

-- | The friendly name that identifies the role.
role_roleName :: Lens.Lens' Role Prelude.Text
role_roleName = Lens.lens (\Role' {roleName} -> roleName) (\s@Role' {} a -> s {roleName = a} :: Role)

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
role_roleId :: Lens.Lens' Role Prelude.Text
role_roleId = Lens.lens (\Role' {roleId} -> roleId) (\s@Role' {} a -> s {roleId = a} :: Role)

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/ guide.
role_arn :: Lens.Lens' Role Prelude.Text
role_arn = Lens.lens (\Role' {arn} -> arn) (\s@Role' {} a -> s {arn = a} :: Role)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
role_createDate :: Lens.Lens' Role Prelude.UTCTime
role_createDate = Lens.lens (\Role' {createDate} -> createDate) (\s@Role' {} a -> s {createDate = a} :: Role) Prelude.. Data._Time

instance Data.FromXML Role where
  parseXML x =
    Role'
      Prelude.<$> (x Data..@? "AssumeRolePolicyDocument")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "MaxSessionDuration")
      Prelude.<*> (x Data..@? "PermissionsBoundary")
      Prelude.<*> (x Data..@? "RoleLastUsed")
      Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@ "Path")
      Prelude.<*> (x Data..@ "RoleName")
      Prelude.<*> (x Data..@ "RoleId")
      Prelude.<*> (x Data..@ "Arn")
      Prelude.<*> (x Data..@ "CreateDate")

instance Prelude.Hashable Role where
  hashWithSalt _salt Role' {..} =
    _salt
      `Prelude.hashWithSalt` assumeRolePolicyDocument
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxSessionDuration
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` roleLastUsed
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` roleId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createDate

instance Prelude.NFData Role where
  rnf Role' {..} =
    Prelude.rnf assumeRolePolicyDocument `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf maxSessionDuration `Prelude.seq`
          Prelude.rnf permissionsBoundary `Prelude.seq`
            Prelude.rnf roleLastUsed `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf path `Prelude.seq`
                  Prelude.rnf roleName `Prelude.seq`
                    Prelude.rnf roleId `Prelude.seq`
                      Prelude.rnf arn `Prelude.seq`
                        Prelude.rnf createDate
