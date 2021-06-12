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
-- Module      : Network.AWS.IAM.Types.RoleDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.InstanceProfile
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Contains information about an IAM role, including all of the role\'s
-- policies.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newRoleDetail' smart constructor.
data RoleDetail = RoleDetail'
  { -- | The trust policy that grants permission to assume the role.
    assumeRolePolicyDocument :: Core.Maybe Core.Text,
    -- | The stable and unique string identifying the role. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    roleId :: Core.Maybe Core.Text,
    -- | Contains information about the last time that an IAM role was used. This
    -- includes the date and time and the Region in which the role was last
    -- used. Activity is only reported for the trailing 400 days. This period
    -- can be shorter if your Region began supporting these features within the
    -- last year. The role might have been used more than 400 days ago. For
    -- more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
    -- in the /IAM User Guide/.
    roleLastUsed :: Core.Maybe RoleLastUsed,
    -- | A list of managed policies attached to the role. These policies are the
    -- role\'s access (permissions) policies.
    attachedManagedPolicies :: Core.Maybe [AttachedPolicy],
    -- | A list of inline policies embedded in the role. These policies are the
    -- role\'s access (permissions) policies.
    rolePolicyList :: Core.Maybe [PolicyDetail],
    -- | The friendly name that identifies the role.
    roleName :: Core.Maybe Core.Text,
    -- | The ARN of the policy used to set the permissions boundary for the role.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Core.Maybe AttachedPermissionsBoundary,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- role was created.
    createDate :: Core.Maybe Core.ISO8601,
    arn :: Core.Maybe Core.Text,
    -- | A list of tags that are attached to the role. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Core.Maybe [Tag],
    -- | A list of instance profiles that contain this role.
    instanceProfileList :: Core.Maybe [InstanceProfile],
    -- | The path to the role. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RoleDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assumeRolePolicyDocument', 'roleDetail_assumeRolePolicyDocument' - The trust policy that grants permission to assume the role.
--
-- 'roleId', 'roleDetail_roleId' - The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'roleLastUsed', 'roleDetail_roleLastUsed' - Contains information about the last time that an IAM role was used. This
-- includes the date and time and the Region in which the role was last
-- used. Activity is only reported for the trailing 400 days. This period
-- can be shorter if your Region began supporting these features within the
-- last year. The role might have been used more than 400 days ago. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
--
-- 'attachedManagedPolicies', 'roleDetail_attachedManagedPolicies' - A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
--
-- 'rolePolicyList', 'roleDetail_rolePolicyList' - A list of inline policies embedded in the role. These policies are the
-- role\'s access (permissions) policies.
--
-- 'roleName', 'roleDetail_roleName' - The friendly name that identifies the role.
--
-- 'permissionsBoundary', 'roleDetail_permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'createDate', 'roleDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
--
-- 'arn', 'roleDetail_arn' - Undocumented member.
--
-- 'tags', 'roleDetail_tags' - A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'instanceProfileList', 'roleDetail_instanceProfileList' - A list of instance profiles that contain this role.
--
-- 'path', 'roleDetail_path' - The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
newRoleDetail ::
  RoleDetail
newRoleDetail =
  RoleDetail'
    { assumeRolePolicyDocument =
        Core.Nothing,
      roleId = Core.Nothing,
      roleLastUsed = Core.Nothing,
      attachedManagedPolicies = Core.Nothing,
      rolePolicyList = Core.Nothing,
      roleName = Core.Nothing,
      permissionsBoundary = Core.Nothing,
      createDate = Core.Nothing,
      arn = Core.Nothing,
      tags = Core.Nothing,
      instanceProfileList = Core.Nothing,
      path = Core.Nothing
    }

-- | The trust policy that grants permission to assume the role.
roleDetail_assumeRolePolicyDocument :: Lens.Lens' RoleDetail (Core.Maybe Core.Text)
roleDetail_assumeRolePolicyDocument = Lens.lens (\RoleDetail' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@RoleDetail' {} a -> s {assumeRolePolicyDocument = a} :: RoleDetail)

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
roleDetail_roleId :: Lens.Lens' RoleDetail (Core.Maybe Core.Text)
roleDetail_roleId = Lens.lens (\RoleDetail' {roleId} -> roleId) (\s@RoleDetail' {} a -> s {roleId = a} :: RoleDetail)

-- | Contains information about the last time that an IAM role was used. This
-- includes the date and time and the Region in which the role was last
-- used. Activity is only reported for the trailing 400 days. This period
-- can be shorter if your Region began supporting these features within the
-- last year. The role might have been used more than 400 days ago. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
roleDetail_roleLastUsed :: Lens.Lens' RoleDetail (Core.Maybe RoleLastUsed)
roleDetail_roleLastUsed = Lens.lens (\RoleDetail' {roleLastUsed} -> roleLastUsed) (\s@RoleDetail' {} a -> s {roleLastUsed = a} :: RoleDetail)

-- | A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
roleDetail_attachedManagedPolicies :: Lens.Lens' RoleDetail (Core.Maybe [AttachedPolicy])
roleDetail_attachedManagedPolicies = Lens.lens (\RoleDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@RoleDetail' {} a -> s {attachedManagedPolicies = a} :: RoleDetail) Core.. Lens.mapping Lens._Coerce

-- | A list of inline policies embedded in the role. These policies are the
-- role\'s access (permissions) policies.
roleDetail_rolePolicyList :: Lens.Lens' RoleDetail (Core.Maybe [PolicyDetail])
roleDetail_rolePolicyList = Lens.lens (\RoleDetail' {rolePolicyList} -> rolePolicyList) (\s@RoleDetail' {} a -> s {rolePolicyList = a} :: RoleDetail) Core.. Lens.mapping Lens._Coerce

-- | The friendly name that identifies the role.
roleDetail_roleName :: Lens.Lens' RoleDetail (Core.Maybe Core.Text)
roleDetail_roleName = Lens.lens (\RoleDetail' {roleName} -> roleName) (\s@RoleDetail' {} a -> s {roleName = a} :: RoleDetail)

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
roleDetail_permissionsBoundary :: Lens.Lens' RoleDetail (Core.Maybe AttachedPermissionsBoundary)
roleDetail_permissionsBoundary = Lens.lens (\RoleDetail' {permissionsBoundary} -> permissionsBoundary) (\s@RoleDetail' {} a -> s {permissionsBoundary = a} :: RoleDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
roleDetail_createDate :: Lens.Lens' RoleDetail (Core.Maybe Core.UTCTime)
roleDetail_createDate = Lens.lens (\RoleDetail' {createDate} -> createDate) (\s@RoleDetail' {} a -> s {createDate = a} :: RoleDetail) Core.. Lens.mapping Core._Time

-- | Undocumented member.
roleDetail_arn :: Lens.Lens' RoleDetail (Core.Maybe Core.Text)
roleDetail_arn = Lens.lens (\RoleDetail' {arn} -> arn) (\s@RoleDetail' {} a -> s {arn = a} :: RoleDetail)

-- | A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
roleDetail_tags :: Lens.Lens' RoleDetail (Core.Maybe [Tag])
roleDetail_tags = Lens.lens (\RoleDetail' {tags} -> tags) (\s@RoleDetail' {} a -> s {tags = a} :: RoleDetail) Core.. Lens.mapping Lens._Coerce

-- | A list of instance profiles that contain this role.
roleDetail_instanceProfileList :: Lens.Lens' RoleDetail (Core.Maybe [InstanceProfile])
roleDetail_instanceProfileList = Lens.lens (\RoleDetail' {instanceProfileList} -> instanceProfileList) (\s@RoleDetail' {} a -> s {instanceProfileList = a} :: RoleDetail) Core.. Lens.mapping Lens._Coerce

-- | The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
roleDetail_path :: Lens.Lens' RoleDetail (Core.Maybe Core.Text)
roleDetail_path = Lens.lens (\RoleDetail' {path} -> path) (\s@RoleDetail' {} a -> s {path = a} :: RoleDetail)

instance Core.FromXML RoleDetail where
  parseXML x =
    RoleDetail'
      Core.<$> (x Core..@? "AssumeRolePolicyDocument")
      Core.<*> (x Core..@? "RoleId")
      Core.<*> (x Core..@? "RoleLastUsed")
      Core.<*> ( x Core..@? "AttachedManagedPolicies"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "RolePolicyList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "RoleName")
      Core.<*> (x Core..@? "PermissionsBoundary")
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "Arn")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> ( x Core..@? "InstanceProfileList"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Path")

instance Core.Hashable RoleDetail

instance Core.NFData RoleDetail
