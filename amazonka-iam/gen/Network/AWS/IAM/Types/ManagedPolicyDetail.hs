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
-- Module      : Network.AWS.IAM.Types.ManagedPolicyDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ManagedPolicyDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.PolicyVersion
import qualified Network.AWS.Lens as Lens

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
    policyName :: Core.Maybe Core.Text,
    -- | The number of entities (users and roles) for which the policy is used as
    -- the permissions boundary.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundaryUsageCount :: Core.Maybe Core.Int,
    -- | Specifies whether the policy can be attached to an IAM user, group, or
    -- role.
    isAttachable :: Core.Maybe Core.Bool,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was created.
    createDate :: Core.Maybe Core.ISO8601,
    arn :: Core.Maybe Core.Text,
    -- | The number of principal entities (users, groups, and roles) that the
    -- policy is attached to.
    attachmentCount :: Core.Maybe Core.Int,
    -- | The identifier for the version of the policy that is set as the default
    -- (operative) version.
    --
    -- For more information about policy versions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
    -- in the /IAM User Guide/.
    defaultVersionId :: Core.Maybe Core.Text,
    -- | A friendly description of the policy.
    description :: Core.Maybe Core.Text,
    -- | A list containing information about the versions of the policy.
    policyVersionList :: Core.Maybe [PolicyVersion],
    -- | The path to the policy.
    --
    -- For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Maybe Core.Text,
    -- | The stable and unique string identifying the policy.
    --
    -- For more information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    policyId :: Core.Maybe Core.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was last updated.
    --
    -- When a policy has only one version, this field contains the date and
    -- time when the policy was created. When a policy has more than one
    -- version, this field contains the date and time when the most recent
    -- policy version was created.
    updateDate :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'permissionsBoundaryUsageCount', 'managedPolicyDetail_permissionsBoundaryUsageCount' - The number of entities (users and roles) for which the policy is used as
-- the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'isAttachable', 'managedPolicyDetail_isAttachable' - Specifies whether the policy can be attached to an IAM user, group, or
-- role.
--
-- 'createDate', 'managedPolicyDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
--
-- 'arn', 'managedPolicyDetail_arn' - Undocumented member.
--
-- 'attachmentCount', 'managedPolicyDetail_attachmentCount' - The number of principal entities (users, groups, and roles) that the
-- policy is attached to.
--
-- 'defaultVersionId', 'managedPolicyDetail_defaultVersionId' - The identifier for the version of the policy that is set as the default
-- (operative) version.
--
-- For more information about policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
--
-- 'description', 'managedPolicyDetail_description' - A friendly description of the policy.
--
-- 'policyVersionList', 'managedPolicyDetail_policyVersionList' - A list containing information about the versions of the policy.
--
-- 'path', 'managedPolicyDetail_path' - The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'policyId', 'managedPolicyDetail_policyId' - The stable and unique string identifying the policy.
--
-- For more information about IDs, see
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
newManagedPolicyDetail ::
  ManagedPolicyDetail
newManagedPolicyDetail =
  ManagedPolicyDetail'
    { policyName = Core.Nothing,
      permissionsBoundaryUsageCount = Core.Nothing,
      isAttachable = Core.Nothing,
      createDate = Core.Nothing,
      arn = Core.Nothing,
      attachmentCount = Core.Nothing,
      defaultVersionId = Core.Nothing,
      description = Core.Nothing,
      policyVersionList = Core.Nothing,
      path = Core.Nothing,
      policyId = Core.Nothing,
      updateDate = Core.Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
managedPolicyDetail_policyName :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Text)
managedPolicyDetail_policyName = Lens.lens (\ManagedPolicyDetail' {policyName} -> policyName) (\s@ManagedPolicyDetail' {} a -> s {policyName = a} :: ManagedPolicyDetail)

-- | The number of entities (users and roles) for which the policy is used as
-- the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
managedPolicyDetail_permissionsBoundaryUsageCount :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Int)
managedPolicyDetail_permissionsBoundaryUsageCount = Lens.lens (\ManagedPolicyDetail' {permissionsBoundaryUsageCount} -> permissionsBoundaryUsageCount) (\s@ManagedPolicyDetail' {} a -> s {permissionsBoundaryUsageCount = a} :: ManagedPolicyDetail)

-- | Specifies whether the policy can be attached to an IAM user, group, or
-- role.
managedPolicyDetail_isAttachable :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Bool)
managedPolicyDetail_isAttachable = Lens.lens (\ManagedPolicyDetail' {isAttachable} -> isAttachable) (\s@ManagedPolicyDetail' {} a -> s {isAttachable = a} :: ManagedPolicyDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
managedPolicyDetail_createDate :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.UTCTime)
managedPolicyDetail_createDate = Lens.lens (\ManagedPolicyDetail' {createDate} -> createDate) (\s@ManagedPolicyDetail' {} a -> s {createDate = a} :: ManagedPolicyDetail) Core.. Lens.mapping Core._Time

-- | Undocumented member.
managedPolicyDetail_arn :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Text)
managedPolicyDetail_arn = Lens.lens (\ManagedPolicyDetail' {arn} -> arn) (\s@ManagedPolicyDetail' {} a -> s {arn = a} :: ManagedPolicyDetail)

-- | The number of principal entities (users, groups, and roles) that the
-- policy is attached to.
managedPolicyDetail_attachmentCount :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Int)
managedPolicyDetail_attachmentCount = Lens.lens (\ManagedPolicyDetail' {attachmentCount} -> attachmentCount) (\s@ManagedPolicyDetail' {} a -> s {attachmentCount = a} :: ManagedPolicyDetail)

-- | The identifier for the version of the policy that is set as the default
-- (operative) version.
--
-- For more information about policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
managedPolicyDetail_defaultVersionId :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Text)
managedPolicyDetail_defaultVersionId = Lens.lens (\ManagedPolicyDetail' {defaultVersionId} -> defaultVersionId) (\s@ManagedPolicyDetail' {} a -> s {defaultVersionId = a} :: ManagedPolicyDetail)

-- | A friendly description of the policy.
managedPolicyDetail_description :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Text)
managedPolicyDetail_description = Lens.lens (\ManagedPolicyDetail' {description} -> description) (\s@ManagedPolicyDetail' {} a -> s {description = a} :: ManagedPolicyDetail)

-- | A list containing information about the versions of the policy.
managedPolicyDetail_policyVersionList :: Lens.Lens' ManagedPolicyDetail (Core.Maybe [PolicyVersion])
managedPolicyDetail_policyVersionList = Lens.lens (\ManagedPolicyDetail' {policyVersionList} -> policyVersionList) (\s@ManagedPolicyDetail' {} a -> s {policyVersionList = a} :: ManagedPolicyDetail) Core.. Lens.mapping Lens._Coerce

-- | The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
managedPolicyDetail_path :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Text)
managedPolicyDetail_path = Lens.lens (\ManagedPolicyDetail' {path} -> path) (\s@ManagedPolicyDetail' {} a -> s {path = a} :: ManagedPolicyDetail)

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
managedPolicyDetail_policyId :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.Text)
managedPolicyDetail_policyId = Lens.lens (\ManagedPolicyDetail' {policyId} -> policyId) (\s@ManagedPolicyDetail' {} a -> s {policyId = a} :: ManagedPolicyDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
managedPolicyDetail_updateDate :: Lens.Lens' ManagedPolicyDetail (Core.Maybe Core.UTCTime)
managedPolicyDetail_updateDate = Lens.lens (\ManagedPolicyDetail' {updateDate} -> updateDate) (\s@ManagedPolicyDetail' {} a -> s {updateDate = a} :: ManagedPolicyDetail) Core.. Lens.mapping Core._Time

instance Core.FromXML ManagedPolicyDetail where
  parseXML x =
    ManagedPolicyDetail'
      Core.<$> (x Core..@? "PolicyName")
      Core.<*> (x Core..@? "PermissionsBoundaryUsageCount")
      Core.<*> (x Core..@? "IsAttachable")
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "Arn")
      Core.<*> (x Core..@? "AttachmentCount")
      Core.<*> (x Core..@? "DefaultVersionId")
      Core.<*> (x Core..@? "Description")
      Core.<*> ( x Core..@? "PolicyVersionList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Path")
      Core.<*> (x Core..@? "PolicyId")
      Core.<*> (x Core..@? "UpdateDate")

instance Core.Hashable ManagedPolicyDetail

instance Core.NFData ManagedPolicyDetail
