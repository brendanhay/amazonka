{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.InstanceProfile
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an IAM role, including all of the role\'s
-- policies.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newRoleDetail' smart constructor.
data RoleDetail = RoleDetail'
  { -- | The trust policy that grants permission to assume the role.
    assumeRolePolicyDocument :: Prelude.Maybe Prelude.Text,
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
    -- | A list of managed policies attached to the role. These policies are the
    -- role\'s access (permissions) policies.
    attachedManagedPolicies :: Prelude.Maybe [AttachedPolicy],
    -- | A list of inline policies embedded in the role. These policies are the
    -- role\'s access (permissions) policies.
    rolePolicyList :: Prelude.Maybe [PolicyDetail],
    -- | The friendly name that identifies the role.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the policy used to set the permissions boundary for the role.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Prelude.Maybe AttachedPermissionsBoundary,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- role was created.
    createDate :: Prelude.Maybe Prelude.ISO8601,
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that are attached to the role. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | A list of instance profiles that contain this role.
    instanceProfileList :: Prelude.Maybe [InstanceProfile],
    -- | The path to the role. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      roleId = Prelude.Nothing,
      roleLastUsed = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing,
      rolePolicyList = Prelude.Nothing,
      roleName = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      createDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      tags = Prelude.Nothing,
      instanceProfileList = Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | The trust policy that grants permission to assume the role.
roleDetail_assumeRolePolicyDocument :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_assumeRolePolicyDocument = Lens.lens (\RoleDetail' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@RoleDetail' {} a -> s {assumeRolePolicyDocument = a} :: RoleDetail)

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

-- | A list of managed policies attached to the role. These policies are the
-- role\'s access (permissions) policies.
roleDetail_attachedManagedPolicies :: Lens.Lens' RoleDetail (Prelude.Maybe [AttachedPolicy])
roleDetail_attachedManagedPolicies = Lens.lens (\RoleDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@RoleDetail' {} a -> s {attachedManagedPolicies = a} :: RoleDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of inline policies embedded in the role. These policies are the
-- role\'s access (permissions) policies.
roleDetail_rolePolicyList :: Lens.Lens' RoleDetail (Prelude.Maybe [PolicyDetail])
roleDetail_rolePolicyList = Lens.lens (\RoleDetail' {rolePolicyList} -> rolePolicyList) (\s@RoleDetail' {} a -> s {rolePolicyList = a} :: RoleDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The friendly name that identifies the role.
roleDetail_roleName :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_roleName = Lens.lens (\RoleDetail' {roleName} -> roleName) (\s@RoleDetail' {} a -> s {roleName = a} :: RoleDetail)

-- | The ARN of the policy used to set the permissions boundary for the role.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
roleDetail_permissionsBoundary :: Lens.Lens' RoleDetail (Prelude.Maybe AttachedPermissionsBoundary)
roleDetail_permissionsBoundary = Lens.lens (\RoleDetail' {permissionsBoundary} -> permissionsBoundary) (\s@RoleDetail' {} a -> s {permissionsBoundary = a} :: RoleDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- role was created.
roleDetail_createDate :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.UTCTime)
roleDetail_createDate = Lens.lens (\RoleDetail' {createDate} -> createDate) (\s@RoleDetail' {} a -> s {createDate = a} :: RoleDetail) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
roleDetail_arn :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_arn = Lens.lens (\RoleDetail' {arn} -> arn) (\s@RoleDetail' {} a -> s {arn = a} :: RoleDetail)

-- | A list of tags that are attached to the role. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
roleDetail_tags :: Lens.Lens' RoleDetail (Prelude.Maybe [Tag])
roleDetail_tags = Lens.lens (\RoleDetail' {tags} -> tags) (\s@RoleDetail' {} a -> s {tags = a} :: RoleDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of instance profiles that contain this role.
roleDetail_instanceProfileList :: Lens.Lens' RoleDetail (Prelude.Maybe [InstanceProfile])
roleDetail_instanceProfileList = Lens.lens (\RoleDetail' {instanceProfileList} -> instanceProfileList) (\s@RoleDetail' {} a -> s {instanceProfileList = a} :: RoleDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The path to the role. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
roleDetail_path :: Lens.Lens' RoleDetail (Prelude.Maybe Prelude.Text)
roleDetail_path = Lens.lens (\RoleDetail' {path} -> path) (\s@RoleDetail' {} a -> s {path = a} :: RoleDetail)

instance Prelude.FromXML RoleDetail where
  parseXML x =
    RoleDetail'
      Prelude.<$> (x Prelude..@? "AssumeRolePolicyDocument")
      Prelude.<*> (x Prelude..@? "RoleId")
      Prelude.<*> (x Prelude..@? "RoleLastUsed")
      Prelude.<*> ( x Prelude..@? "AttachedManagedPolicies"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "RolePolicyList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "RoleName")
      Prelude.<*> (x Prelude..@? "PermissionsBoundary")
      Prelude.<*> (x Prelude..@? "CreateDate")
      Prelude.<*> (x Prelude..@? "Arn")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> ( x Prelude..@? "InstanceProfileList"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "Path")

instance Prelude.Hashable RoleDetail

instance Prelude.NFData RoleDetail
