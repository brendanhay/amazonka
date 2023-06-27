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
-- Module      : Amazonka.IAM.Types.RoleDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.RoleDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.AttachedPermissionsBoundary
import Amazonka.IAM.Types.AttachedPolicy
import Amazonka.IAM.Types.InstanceProfile
import Amazonka.IAM.Types.PolicyDetail
import Amazonka.IAM.Types.RoleLastUsed
import Amazonka.IAM.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an IAM role, including all of the role\'s
-- policies.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newRoleDetail' smart constructor.
data RoleDetail = RoleDetail'
  { arn :: Prelude.Maybe Prelude.Text,
    -- | The trust policy that grants permission to assume the role.
    assumeRolePolicyDocument :: Prelude.Maybe Prelude.Text,
    -- | A list of managed policies attached to the role. These policies are the
    -- role\'s access (permissions) policies.
    attachedManagedPolicies :: Prelude.Maybe [AttachedPolicy],
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- role was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | A list of instance profiles that contain this role.
    instanceProfileList :: Prelude.Maybe [InstanceProfile],
    -- | The path to the role. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the policy used to set the permissions boundary for the role.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Prelude.Maybe AttachedPermissionsBoundary,
    -- | The stable and unique string identifying the role. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    roleId :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the last time that an IAM role was used. This
    -- includes the date and time and the Region in which the role was last
    -- used. Activity is only reported for the trailing 400 days. This period
    -- can be shorter if your Region began supporting these features within the
    -- last year. The role might have been used more than 400 days ago. For
    -- more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
    -- in the /IAM User Guide/.
    roleLastUsed :: Prelude.Maybe RoleLastUsed,
    -- | The friendly name that identifies the role.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | A list of inline policies embedded in the role. These policies are the
    -- role\'s access (permissions) policies.
    rolePolicyList :: Prelude.Maybe [PolicyDetail],
    -- | A list of tags that are attached to the role. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoleDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'roleDetail_arn' - Undocumented member.
--
-- 'assumeRolePolicyDocument', 'roleDetail_assumeRolePolicyDocument' - The trust policy that grants permission to assume the role.
--
-- 'attachedManagedPolicies', 'roleDetail_attachedManagedPolicies' - A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
--
-- 'createDate', 'roleDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
--
-- 'instanceProfileList', 'roleDetail_instanceProfileList' - A list of instance profiles that contain this role.
--
-- 'path', 'roleDetail_path' - The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'permissionsBoundary', 'roleDetail_permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
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
-- 'roleName', 'roleDetail_roleName' - The friendly name that identifies the role.
--
-- 'rolePolicyList', 'roleDetail_rolePolicyList' - A list of inline policies embedded in the role. These policies are the
-- role\'s access (permissions) policies.
--
-- 'tags', 'roleDetail_tags' - A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
newRoleDetail ::
  RoleDetail
newRoleDetail =
  RoleDetail'
    { arn = Prelude.Nothing,
      assumeRolePolicyDocument = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing,
      createDate = Prelude.Nothing,
      instanceProfileList = Prelude.Nothing,
      path = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      roleId = Prelude.Nothing,
      roleLastUsed = Prelude.Nothing,
      roleName = Prelude.Nothing,
      rolePolicyList = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
roleDetail_arn :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_arn = Lens.lens (\RoleDetail' {arn} -> arn) (\s@RoleDetail' {} a -> s {arn = a} :: RoleDetail)

-- | The trust policy that grants permission to assume the role.
roleDetail_assumeRolePolicyDocument :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_assumeRolePolicyDocument = Lens.lens (\RoleDetail' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@RoleDetail' {} a -> s {assumeRolePolicyDocument = a} :: RoleDetail)

-- | A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
roleDetail_attachedManagedPolicies :: Lens.Lens' RoleDetail (Prelude.Maybe [AttachedPolicy])
roleDetail_attachedManagedPolicies = Lens.lens (\RoleDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@RoleDetail' {} a -> s {attachedManagedPolicies = a} :: RoleDetail) Prelude.. Lens.mapping Lens.coerced

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
roleDetail_createDate :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.UTCTime)
roleDetail_createDate = Lens.lens (\RoleDetail' {createDate} -> createDate) (\s@RoleDetail' {} a -> s {createDate = a} :: RoleDetail) Prelude.. Lens.mapping Data._Time

-- | A list of instance profiles that contain this role.
roleDetail_instanceProfileList :: Lens.Lens' RoleDetail (Prelude.Maybe [InstanceProfile])
roleDetail_instanceProfileList = Lens.lens (\RoleDetail' {instanceProfileList} -> instanceProfileList) (\s@RoleDetail' {} a -> s {instanceProfileList = a} :: RoleDetail) Prelude.. Lens.mapping Lens.coerced

-- | The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
roleDetail_path :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_path = Lens.lens (\RoleDetail' {path} -> path) (\s@RoleDetail' {} a -> s {path = a} :: RoleDetail)

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
roleDetail_permissionsBoundary :: Lens.Lens' RoleDetail (Prelude.Maybe AttachedPermissionsBoundary)
roleDetail_permissionsBoundary = Lens.lens (\RoleDetail' {permissionsBoundary} -> permissionsBoundary) (\s@RoleDetail' {} a -> s {permissionsBoundary = a} :: RoleDetail)

-- | The stable and unique string identifying the role. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
roleDetail_roleId :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_roleId = Lens.lens (\RoleDetail' {roleId} -> roleId) (\s@RoleDetail' {} a -> s {roleId = a} :: RoleDetail)

-- | Contains information about the last time that an IAM role was used. This
-- includes the date and time and the Region in which the role was last
-- used. Activity is only reported for the trailing 400 days. This period
-- can be shorter if your Region began supporting these features within the
-- last year. The role might have been used more than 400 days ago. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
roleDetail_roleLastUsed :: Lens.Lens' RoleDetail (Prelude.Maybe RoleLastUsed)
roleDetail_roleLastUsed = Lens.lens (\RoleDetail' {roleLastUsed} -> roleLastUsed) (\s@RoleDetail' {} a -> s {roleLastUsed = a} :: RoleDetail)

-- | The friendly name that identifies the role.
roleDetail_roleName :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_roleName = Lens.lens (\RoleDetail' {roleName} -> roleName) (\s@RoleDetail' {} a -> s {roleName = a} :: RoleDetail)

-- | A list of inline policies embedded in the role. These policies are the
-- role\'s access (permissions) policies.
roleDetail_rolePolicyList :: Lens.Lens' RoleDetail (Prelude.Maybe [PolicyDetail])
roleDetail_rolePolicyList = Lens.lens (\RoleDetail' {rolePolicyList} -> rolePolicyList) (\s@RoleDetail' {} a -> s {rolePolicyList = a} :: RoleDetail) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
roleDetail_tags :: Lens.Lens' RoleDetail (Prelude.Maybe [Tag])
roleDetail_tags = Lens.lens (\RoleDetail' {tags} -> tags) (\s@RoleDetail' {} a -> s {tags = a} :: RoleDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML RoleDetail where
  parseXML x =
    RoleDetail'
      Prelude.<$> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "AssumeRolePolicyDocument")
      Prelude.<*> ( x
                      Data..@? "AttachedManagedPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "CreateDate")
      Prelude.<*> ( x
                      Data..@? "InstanceProfileList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Path")
      Prelude.<*> (x Data..@? "PermissionsBoundary")
      Prelude.<*> (x Data..@? "RoleId")
      Prelude.<*> (x Data..@? "RoleLastUsed")
      Prelude.<*> (x Data..@? "RoleName")
      Prelude.<*> ( x
                      Data..@? "RolePolicyList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable RoleDetail where
  hashWithSalt _salt RoleDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` assumeRolePolicyDocument
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` instanceProfileList
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` roleId
      `Prelude.hashWithSalt` roleLastUsed
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` rolePolicyList
      `Prelude.hashWithSalt` tags

instance Prelude.NFData RoleDetail where
  rnf RoleDetail' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assumeRolePolicyDocument
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf instanceProfileList
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf roleId
      `Prelude.seq` Prelude.rnf roleLastUsed
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf rolePolicyList
      `Prelude.seq` Prelude.rnf tags
