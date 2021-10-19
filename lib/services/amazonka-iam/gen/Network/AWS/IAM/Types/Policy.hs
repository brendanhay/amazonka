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
-- Module      : Network.AWS.IAM.Types.Policy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Policy where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a managed policy.
--
-- This data type is used as a response element in the CreatePolicy,
-- GetPolicy, and ListPolicies operations.
--
-- For more information about managed policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | The friendly name (not ARN) identifying the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was last updated.
    --
    -- When a policy has only one version, this field contains the date and
    -- time when the policy was created. When a policy has more than one
    -- version, this field contains the date and time when the most recent
    -- policy version was created.
    updateDate :: Prelude.Maybe Core.ISO8601,
    -- | The stable and unique string identifying the policy.
    --
    -- For more information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The path to the policy.
    --
    -- For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was created.
    createDate :: Prelude.Maybe Core.ISO8601,
    -- | Specifies whether the policy can be attached to an IAM user, group, or
    -- role.
    isAttachable :: Prelude.Maybe Prelude.Bool,
    -- | The number of entities (users and roles) for which the policy is used to
    -- set the permissions boundary.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundaryUsageCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the version of the policy that is set as the default
    -- version.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    -- | The number of entities (users, groups, and roles) that the policy is
    -- attached to.
    attachmentCount :: Prelude.Maybe Prelude.Int,
    -- | A friendly description of the policy.
    --
    -- This element is included in the response to the GetPolicy operation. It
    -- is not included in the response to the ListPolicies operation.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that are attached to the instance profile. For more
    -- information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Policy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'policy_policyName' - The friendly name (not ARN) identifying the policy.
--
-- 'arn', 'policy_arn' - Undocumented member.
--
-- 'updateDate', 'policy_updateDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
--
-- 'policyId', 'policy_policyId' - The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'path', 'policy_path' - The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'createDate', 'policy_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
--
-- 'isAttachable', 'policy_isAttachable' - Specifies whether the policy can be attached to an IAM user, group, or
-- role.
--
-- 'permissionsBoundaryUsageCount', 'policy_permissionsBoundaryUsageCount' - The number of entities (users and roles) for which the policy is used to
-- set the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'defaultVersionId', 'policy_defaultVersionId' - The identifier for the version of the policy that is set as the default
-- version.
--
-- 'attachmentCount', 'policy_attachmentCount' - The number of entities (users, groups, and roles) that the policy is
-- attached to.
--
-- 'description', 'policy_description' - A friendly description of the policy.
--
-- This element is included in the response to the GetPolicy operation. It
-- is not included in the response to the ListPolicies operation.
--
-- 'tags', 'policy_tags' - A list of tags that are attached to the instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
newPolicy ::
  Policy
newPolicy =
  Policy'
    { policyName = Prelude.Nothing,
      arn = Prelude.Nothing,
      updateDate = Prelude.Nothing,
      policyId = Prelude.Nothing,
      path = Prelude.Nothing,
      createDate = Prelude.Nothing,
      isAttachable = Prelude.Nothing,
      permissionsBoundaryUsageCount = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      attachmentCount = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
policy_policyName :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_policyName = Lens.lens (\Policy' {policyName} -> policyName) (\s@Policy' {} a -> s {policyName = a} :: Policy)

-- | Undocumented member.
policy_arn :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_arn = Lens.lens (\Policy' {arn} -> arn) (\s@Policy' {} a -> s {arn = a} :: Policy)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
policy_updateDate :: Lens.Lens' Policy (Prelude.Maybe Prelude.UTCTime)
policy_updateDate = Lens.lens (\Policy' {updateDate} -> updateDate) (\s@Policy' {} a -> s {updateDate = a} :: Policy) Prelude.. Lens.mapping Core._Time

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
policy_policyId :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_policyId = Lens.lens (\Policy' {policyId} -> policyId) (\s@Policy' {} a -> s {policyId = a} :: Policy)

-- | The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
policy_path :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_path = Lens.lens (\Policy' {path} -> path) (\s@Policy' {} a -> s {path = a} :: Policy)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
policy_createDate :: Lens.Lens' Policy (Prelude.Maybe Prelude.UTCTime)
policy_createDate = Lens.lens (\Policy' {createDate} -> createDate) (\s@Policy' {} a -> s {createDate = a} :: Policy) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the policy can be attached to an IAM user, group, or
-- role.
policy_isAttachable :: Lens.Lens' Policy (Prelude.Maybe Prelude.Bool)
policy_isAttachable = Lens.lens (\Policy' {isAttachable} -> isAttachable) (\s@Policy' {} a -> s {isAttachable = a} :: Policy)

-- | The number of entities (users and roles) for which the policy is used to
-- set the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
policy_permissionsBoundaryUsageCount :: Lens.Lens' Policy (Prelude.Maybe Prelude.Int)
policy_permissionsBoundaryUsageCount = Lens.lens (\Policy' {permissionsBoundaryUsageCount} -> permissionsBoundaryUsageCount) (\s@Policy' {} a -> s {permissionsBoundaryUsageCount = a} :: Policy)

-- | The identifier for the version of the policy that is set as the default
-- version.
policy_defaultVersionId :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_defaultVersionId = Lens.lens (\Policy' {defaultVersionId} -> defaultVersionId) (\s@Policy' {} a -> s {defaultVersionId = a} :: Policy)

-- | The number of entities (users, groups, and roles) that the policy is
-- attached to.
policy_attachmentCount :: Lens.Lens' Policy (Prelude.Maybe Prelude.Int)
policy_attachmentCount = Lens.lens (\Policy' {attachmentCount} -> attachmentCount) (\s@Policy' {} a -> s {attachmentCount = a} :: Policy)

-- | A friendly description of the policy.
--
-- This element is included in the response to the GetPolicy operation. It
-- is not included in the response to the ListPolicies operation.
policy_description :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_description = Lens.lens (\Policy' {description} -> description) (\s@Policy' {} a -> s {description = a} :: Policy)

-- | A list of tags that are attached to the instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
policy_tags :: Lens.Lens' Policy (Prelude.Maybe [Tag])
policy_tags = Lens.lens (\Policy' {tags} -> tags) (\s@Policy' {} a -> s {tags = a} :: Policy) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML Policy where
  parseXML x =
    Policy'
      Prelude.<$> (x Core..@? "PolicyName")
      Prelude.<*> (x Core..@? "Arn")
      Prelude.<*> (x Core..@? "UpdateDate")
      Prelude.<*> (x Core..@? "PolicyId")
      Prelude.<*> (x Core..@? "Path")
      Prelude.<*> (x Core..@? "CreateDate")
      Prelude.<*> (x Core..@? "IsAttachable")
      Prelude.<*> (x Core..@? "PermissionsBoundaryUsageCount")
      Prelude.<*> (x Core..@? "DefaultVersionId")
      Prelude.<*> (x Core..@? "AttachmentCount")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable Policy

instance Prelude.NFData Policy
