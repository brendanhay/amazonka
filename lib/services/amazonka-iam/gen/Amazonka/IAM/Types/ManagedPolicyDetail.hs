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
-- Module      : Amazonka.IAM.Types.ManagedPolicyDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ManagedPolicyDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.PolicyVersion
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a managed policy, including the policy\'s
-- ARN, versions, and the number of principal entities (users, groups, and
-- roles) that the policy is attached to.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- For more information about managed policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- /See:/ 'newManagedPolicyDetail' smart constructor.
data ManagedPolicyDetail = ManagedPolicyDetail'
  { -- | The friendly name (not ARN) identifying the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The stable and unique string identifying the policy.
    --
    -- For more information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the version of the policy that is set as the default
    -- (operative) version.
    --
    -- For more information about policy versions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
    -- in the /IAM User Guide/.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    -- | The path to the policy.
    --
    -- For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was last updated.
    --
    -- When a policy has only one version, this field contains the date and
    -- time when the policy was created. When a policy has more than one
    -- version, this field contains the date and time when the most recent
    -- policy version was created.
    updateDate :: Prelude.Maybe Data.ISO8601,
    -- | A list containing information about the versions of the policy.
    policyVersionList :: Prelude.Maybe [PolicyVersion],
    -- | A friendly description of the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The number of principal entities (users, groups, and roles) that the
    -- policy is attached to.
    attachmentCount :: Prelude.Maybe Prelude.Int,
    -- | The number of entities (users and roles) for which the policy is used as
    -- the permissions boundary.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundaryUsageCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the policy can be attached to an IAM user, group, or
    -- role.
    isAttachable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedPolicyDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'managedPolicyDetail_policyName' - The friendly name (not ARN) identifying the policy.
--
-- 'policyId', 'managedPolicyDetail_policyId' - The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'defaultVersionId', 'managedPolicyDetail_defaultVersionId' - The identifier for the version of the policy that is set as the default
-- (operative) version.
--
-- For more information about policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
--
-- 'arn', 'managedPolicyDetail_arn' - Undocumented member.
--
-- 'path', 'managedPolicyDetail_path' - The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'updateDate', 'managedPolicyDetail_updateDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
--
-- 'policyVersionList', 'managedPolicyDetail_policyVersionList' - A list containing information about the versions of the policy.
--
-- 'description', 'managedPolicyDetail_description' - A friendly description of the policy.
--
-- 'createDate', 'managedPolicyDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
--
-- 'attachmentCount', 'managedPolicyDetail_attachmentCount' - The number of principal entities (users, groups, and roles) that the
-- policy is attached to.
--
-- 'permissionsBoundaryUsageCount', 'managedPolicyDetail_permissionsBoundaryUsageCount' - The number of entities (users and roles) for which the policy is used as
-- the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'isAttachable', 'managedPolicyDetail_isAttachable' - Specifies whether the policy can be attached to an IAM user, group, or
-- role.
newManagedPolicyDetail ::
  ManagedPolicyDetail
newManagedPolicyDetail =
  ManagedPolicyDetail'
    { policyName = Prelude.Nothing,
      policyId = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      arn = Prelude.Nothing,
      path = Prelude.Nothing,
      updateDate = Prelude.Nothing,
      policyVersionList = Prelude.Nothing,
      description = Prelude.Nothing,
      createDate = Prelude.Nothing,
      attachmentCount = Prelude.Nothing,
      permissionsBoundaryUsageCount = Prelude.Nothing,
      isAttachable = Prelude.Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
managedPolicyDetail_policyName :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Text)
managedPolicyDetail_policyName = Lens.lens (\ManagedPolicyDetail' {policyName} -> policyName) (\s@ManagedPolicyDetail' {} a -> s {policyName = a} :: ManagedPolicyDetail)

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
managedPolicyDetail_policyId :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Text)
managedPolicyDetail_policyId = Lens.lens (\ManagedPolicyDetail' {policyId} -> policyId) (\s@ManagedPolicyDetail' {} a -> s {policyId = a} :: ManagedPolicyDetail)

-- | The identifier for the version of the policy that is set as the default
-- (operative) version.
--
-- For more information about policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
managedPolicyDetail_defaultVersionId :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Text)
managedPolicyDetail_defaultVersionId = Lens.lens (\ManagedPolicyDetail' {defaultVersionId} -> defaultVersionId) (\s@ManagedPolicyDetail' {} a -> s {defaultVersionId = a} :: ManagedPolicyDetail)

-- | Undocumented member.
managedPolicyDetail_arn :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Text)
managedPolicyDetail_arn = Lens.lens (\ManagedPolicyDetail' {arn} -> arn) (\s@ManagedPolicyDetail' {} a -> s {arn = a} :: ManagedPolicyDetail)

-- | The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
managedPolicyDetail_path :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Text)
managedPolicyDetail_path = Lens.lens (\ManagedPolicyDetail' {path} -> path) (\s@ManagedPolicyDetail' {} a -> s {path = a} :: ManagedPolicyDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
managedPolicyDetail_updateDate :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.UTCTime)
managedPolicyDetail_updateDate = Lens.lens (\ManagedPolicyDetail' {updateDate} -> updateDate) (\s@ManagedPolicyDetail' {} a -> s {updateDate = a} :: ManagedPolicyDetail) Prelude.. Lens.mapping Data._Time

-- | A list containing information about the versions of the policy.
managedPolicyDetail_policyVersionList :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe [PolicyVersion])
managedPolicyDetail_policyVersionList = Lens.lens (\ManagedPolicyDetail' {policyVersionList} -> policyVersionList) (\s@ManagedPolicyDetail' {} a -> s {policyVersionList = a} :: ManagedPolicyDetail) Prelude.. Lens.mapping Lens.coerced

-- | A friendly description of the policy.
managedPolicyDetail_description :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Text)
managedPolicyDetail_description = Lens.lens (\ManagedPolicyDetail' {description} -> description) (\s@ManagedPolicyDetail' {} a -> s {description = a} :: ManagedPolicyDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
managedPolicyDetail_createDate :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.UTCTime)
managedPolicyDetail_createDate = Lens.lens (\ManagedPolicyDetail' {createDate} -> createDate) (\s@ManagedPolicyDetail' {} a -> s {createDate = a} :: ManagedPolicyDetail) Prelude.. Lens.mapping Data._Time

-- | The number of principal entities (users, groups, and roles) that the
-- policy is attached to.
managedPolicyDetail_attachmentCount :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Int)
managedPolicyDetail_attachmentCount = Lens.lens (\ManagedPolicyDetail' {attachmentCount} -> attachmentCount) (\s@ManagedPolicyDetail' {} a -> s {attachmentCount = a} :: ManagedPolicyDetail)

-- | The number of entities (users and roles) for which the policy is used as
-- the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
managedPolicyDetail_permissionsBoundaryUsageCount :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Int)
managedPolicyDetail_permissionsBoundaryUsageCount = Lens.lens (\ManagedPolicyDetail' {permissionsBoundaryUsageCount} -> permissionsBoundaryUsageCount) (\s@ManagedPolicyDetail' {} a -> s {permissionsBoundaryUsageCount = a} :: ManagedPolicyDetail)

-- | Specifies whether the policy can be attached to an IAM user, group, or
-- role.
managedPolicyDetail_isAttachable :: Lens.Lens' ManagedPolicyDetail (Prelude.Maybe Prelude.Bool)
managedPolicyDetail_isAttachable = Lens.lens (\ManagedPolicyDetail' {isAttachable} -> isAttachable) (\s@ManagedPolicyDetail' {} a -> s {isAttachable = a} :: ManagedPolicyDetail)

instance Data.FromXML ManagedPolicyDetail where
  parseXML x =
    ManagedPolicyDetail'
      Prelude.<$> (x Data..@? "PolicyName")
      Prelude.<*> (x Data..@? "PolicyId")
      Prelude.<*> (x Data..@? "DefaultVersionId")
      Prelude.<*> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "Path")
      Prelude.<*> (x Data..@? "UpdateDate")
      Prelude.<*> ( x Data..@? "PolicyVersionList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "CreateDate")
      Prelude.<*> (x Data..@? "AttachmentCount")
      Prelude.<*> (x Data..@? "PermissionsBoundaryUsageCount")
      Prelude.<*> (x Data..@? "IsAttachable")

instance Prelude.Hashable ManagedPolicyDetail where
  hashWithSalt _salt ManagedPolicyDetail' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` defaultVersionId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` updateDate
      `Prelude.hashWithSalt` policyVersionList
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` attachmentCount
      `Prelude.hashWithSalt` permissionsBoundaryUsageCount
      `Prelude.hashWithSalt` isAttachable

instance Prelude.NFData ManagedPolicyDetail where
  rnf ManagedPolicyDetail' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf updateDate
      `Prelude.seq` Prelude.rnf policyVersionList
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf attachmentCount
      `Prelude.seq` Prelude.rnf permissionsBoundaryUsageCount
      `Prelude.seq` Prelude.rnf isAttachable
