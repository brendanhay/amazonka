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
-- Module      : Network.AWS.IAM.Types.Role
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Role where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Contains information about an IAM role. This structure is returned as a
-- response element in several API operations that interact with roles.
--
-- /See:/ 'newRole' smart constructor.
data Role = Role'
  { -- | The policy that grants an entity permission to assume the role.
    assumeRolePolicyDocument :: Core.Maybe Core.Text,
    -- | The maximum session duration (in seconds) for the specified role. Anyone
    -- who uses the AWS CLI, or API to assume the role can specify the duration
    -- using the optional @DurationSeconds@ API parameter or @duration-seconds@
    -- CLI parameter.
    maxSessionDuration :: Core.Maybe Core.Natural,
    -- | Contains information about the last time that an IAM role was used. This
    -- includes the date and time and the Region in which the role was last
    -- used. Activity is only reported for the trailing 400 days. This period
    -- can be shorter if your Region began supporting these features within the
    -- last year. The role might have been used more than 400 days ago. For
    -- more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
    -- in the /IAM User Guide/.
    roleLastUsed :: Core.Maybe RoleLastUsed,
    -- | The ARN of the policy used to set the permissions boundary for the role.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Core.Maybe AttachedPermissionsBoundary,
    -- | A list of tags that are attached to the role. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Core.Maybe [Tag],
    -- | A description of the role that you provide.
    description :: Core.Maybe Core.Text,
    -- | The path to the role. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Text,
    -- | The friendly name that identifies the role.
    roleName :: Core.Text,
    -- | The stable and unique string identifying the role. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    roleId :: Core.Text,
    -- | The Amazon Resource Name (ARN) specifying the role. For more information
    -- about ARNs and how to use them in policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/ guide.
    arn :: Core.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- role was created.
    createDate :: Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxSessionDuration', 'role_maxSessionDuration' - The maximum session duration (in seconds) for the specified role. Anyone
-- who uses the AWS CLI, or API to assume the role can specify the duration
-- using the optional @DurationSeconds@ API parameter or @duration-seconds@
-- CLI parameter.
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
-- 'permissionsBoundary', 'role_permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'tags', 'role_tags' - A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'description', 'role_description' - A description of the role that you provide.
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
  Core.Text ->
  -- | 'roleName'
  Core.Text ->
  -- | 'roleId'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  -- | 'createDate'
  Core.UTCTime ->
  Role
newRole pPath_ pRoleName_ pRoleId_ pArn_ pCreateDate_ =
  Role'
    { assumeRolePolicyDocument = Core.Nothing,
      maxSessionDuration = Core.Nothing,
      roleLastUsed = Core.Nothing,
      permissionsBoundary = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      path = pPath_,
      roleName = pRoleName_,
      roleId = pRoleId_,
      arn = pArn_,
      createDate = Core._Time Lens.# pCreateDate_
    }

-- | The policy that grants an entity permission to assume the role.
role_assumeRolePolicyDocument :: Lens.Lens' Role (Core.Maybe Core.Text)
role_assumeRolePolicyDocument = Lens.lens (\Role' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@Role' {} a -> s {assumeRolePolicyDocument = a} :: Role)

-- | The maximum session duration (in seconds) for the specified role. Anyone
-- who uses the AWS CLI, or API to assume the role can specify the duration
-- using the optional @DurationSeconds@ API parameter or @duration-seconds@
-- CLI parameter.
role_maxSessionDuration :: Lens.Lens' Role (Core.Maybe Core.Natural)
role_maxSessionDuration = Lens.lens (\Role' {maxSessionDuration} -> maxSessionDuration) (\s@Role' {} a -> s {maxSessionDuration = a} :: Role)

-- | Contains information about the last time that an IAM role was used. This
-- includes the date and time and the Region in which the role was last
-- used. Activity is only reported for the trailing 400 days. This period
-- can be shorter if your Region began supporting these features within the
-- last year. The role might have been used more than 400 days ago. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
role_roleLastUsed :: Lens.Lens' Role (Core.Maybe RoleLastUsed)
role_roleLastUsed = Lens.lens (\Role' {roleLastUsed} -> roleLastUsed) (\s@Role' {} a -> s {roleLastUsed = a} :: Role)

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
role_permissionsBoundary :: Lens.Lens' Role (Core.Maybe AttachedPermissionsBoundary)
role_permissionsBoundary = Lens.lens (\Role' {permissionsBoundary} -> permissionsBoundary) (\s@Role' {} a -> s {permissionsBoundary = a} :: Role)

-- | A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
role_tags :: Lens.Lens' Role (Core.Maybe [Tag])
role_tags = Lens.lens (\Role' {tags} -> tags) (\s@Role' {} a -> s {tags = a} :: Role) Core.. Lens.mapping Lens._Coerce

-- | A description of the role that you provide.
role_description :: Lens.Lens' Role (Core.Maybe Core.Text)
role_description = Lens.lens (\Role' {description} -> description) (\s@Role' {} a -> s {description = a} :: Role)

-- | The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
role_path :: Lens.Lens' Role Core.Text
role_path = Lens.lens (\Role' {path} -> path) (\s@Role' {} a -> s {path = a} :: Role)

-- | The friendly name that identifies the role.
role_roleName :: Lens.Lens' Role Core.Text
role_roleName = Lens.lens (\Role' {roleName} -> roleName) (\s@Role' {} a -> s {roleName = a} :: Role)

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
role_roleId :: Lens.Lens' Role Core.Text
role_roleId = Lens.lens (\Role' {roleId} -> roleId) (\s@Role' {} a -> s {roleId = a} :: Role)

-- | The Amazon Resource Name (ARN) specifying the role. For more information
-- about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/ guide.
role_arn :: Lens.Lens' Role Core.Text
role_arn = Lens.lens (\Role' {arn} -> arn) (\s@Role' {} a -> s {arn = a} :: Role)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
role_createDate :: Lens.Lens' Role Core.UTCTime
role_createDate = Lens.lens (\Role' {createDate} -> createDate) (\s@Role' {} a -> s {createDate = a} :: Role) Core.. Core._Time

instance Core.FromXML Role where
  parseXML x =
    Role'
      Core.<$> (x Core..@? "AssumeRolePolicyDocument")
      Core.<*> (x Core..@? "MaxSessionDuration")
      Core.<*> (x Core..@? "RoleLastUsed")
      Core.<*> (x Core..@? "PermissionsBoundary")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@ "Path")
      Core.<*> (x Core..@ "RoleName")
      Core.<*> (x Core..@ "RoleId")
      Core.<*> (x Core..@ "Arn")
      Core.<*> (x Core..@ "CreateDate")

instance Core.Hashable Role

instance Core.NFData Role
