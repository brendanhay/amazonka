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
-- Module      : Network.AWS.IAM.Types.Policy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Policy where

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
    -- | The number of entities (users and roles) for which the policy is used to
    -- set the permissions boundary.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundaryUsageCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the policy can be attached to an IAM user, group, or
    -- role.
    isAttachable :: Prelude.Maybe Prelude.Bool,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was created.
    createDate :: Prelude.Maybe Prelude.ISO8601,
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of entities (users, groups, and roles) that the policy is
    -- attached to.
    attachmentCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier for the version of the policy that is set as the default
    -- version.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that are attached to the instance profile. For more
    -- information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | A friendly description of the policy.
    --
    -- This element is included in the response to the GetPolicy operation. It
    -- is not included in the response to the ListPolicies operation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The path to the policy.
    --
    -- For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | The stable and unique string identifying the policy.
    --
    -- For more information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- policy was last updated.
    --
    -- When a policy has only one version, this field contains the date and
    -- time when the policy was created. When a policy has more than one
    -- version, this field contains the date and time when the most recent
    -- policy version was created.
    updateDate :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'permissionsBoundaryUsageCount', 'policy_permissionsBoundaryUsageCount' - The number of entities (users and roles) for which the policy is used to
-- set the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'isAttachable', 'policy_isAttachable' - Specifies whether the policy can be attached to an IAM user, group, or
-- role.
--
-- 'createDate', 'policy_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
--
-- 'arn', 'policy_arn' - Undocumented member.
--
-- 'attachmentCount', 'policy_attachmentCount' - The number of entities (users, groups, and roles) that the policy is
-- attached to.
--
-- 'defaultVersionId', 'policy_defaultVersionId' - The identifier for the version of the policy that is set as the default
-- version.
--
-- 'tags', 'policy_tags' - A list of tags that are attached to the instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'description', 'policy_description' - A friendly description of the policy.
--
-- This element is included in the response to the GetPolicy operation. It
-- is not included in the response to the ListPolicies operation.
--
-- 'path', 'policy_path' - The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'policyId', 'policy_policyId' - The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'updateDate', 'policy_updateDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
newPolicy ::
  Policy
newPolicy =
  Policy'
    { policyName = Prelude.Nothing,
      permissionsBoundaryUsageCount = Prelude.Nothing,
      isAttachable = Prelude.Nothing,
      createDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      attachmentCount = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      path = Prelude.Nothing,
      policyId = Prelude.Nothing,
      updateDate = Prelude.Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
policy_policyName :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_policyName = Lens.lens (\Policy' {policyName} -> policyName) (\s@Policy' {} a -> s {policyName = a} :: Policy)

-- | The number of entities (users and roles) for which the policy is used to
-- set the permissions boundary.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
policy_permissionsBoundaryUsageCount :: Lens.Lens' Policy (Prelude.Maybe Prelude.Int)
policy_permissionsBoundaryUsageCount = Lens.lens (\Policy' {permissionsBoundaryUsageCount} -> permissionsBoundaryUsageCount) (\s@Policy' {} a -> s {permissionsBoundaryUsageCount = a} :: Policy)

-- | Specifies whether the policy can be attached to an IAM user, group, or
-- role.
policy_isAttachable :: Lens.Lens' Policy (Prelude.Maybe Prelude.Bool)
policy_isAttachable = Lens.lens (\Policy' {isAttachable} -> isAttachable) (\s@Policy' {} a -> s {isAttachable = a} :: Policy)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was created.
policy_createDate :: Lens.Lens' Policy (Prelude.Maybe Prelude.UTCTime)
policy_createDate = Lens.lens (\Policy' {createDate} -> createDate) (\s@Policy' {} a -> s {createDate = a} :: Policy) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
policy_arn :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_arn = Lens.lens (\Policy' {arn} -> arn) (\s@Policy' {} a -> s {arn = a} :: Policy)

-- | The number of entities (users, groups, and roles) that the policy is
-- attached to.
policy_attachmentCount :: Lens.Lens' Policy (Prelude.Maybe Prelude.Int)
policy_attachmentCount = Lens.lens (\Policy' {attachmentCount} -> attachmentCount) (\s@Policy' {} a -> s {attachmentCount = a} :: Policy)

-- | The identifier for the version of the policy that is set as the default
-- version.
policy_defaultVersionId :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_defaultVersionId = Lens.lens (\Policy' {defaultVersionId} -> defaultVersionId) (\s@Policy' {} a -> s {defaultVersionId = a} :: Policy)

-- | A list of tags that are attached to the instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
policy_tags :: Lens.Lens' Policy (Prelude.Maybe [Tag])
policy_tags = Lens.lens (\Policy' {tags} -> tags) (\s@Policy' {} a -> s {tags = a} :: Policy) Prelude.. Lens.mapping Prelude._Coerce

-- | A friendly description of the policy.
--
-- This element is included in the response to the GetPolicy operation. It
-- is not included in the response to the ListPolicies operation.
policy_description :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_description = Lens.lens (\Policy' {description} -> description) (\s@Policy' {} a -> s {description = a} :: Policy)

-- | The path to the policy.
--
-- For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
policy_path :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_path = Lens.lens (\Policy' {path} -> path) (\s@Policy' {} a -> s {path = a} :: Policy)

-- | The stable and unique string identifying the policy.
--
-- For more information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
policy_policyId :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_policyId = Lens.lens (\Policy' {policyId} -> policyId) (\s@Policy' {} a -> s {policyId = a} :: Policy)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- policy was last updated.
--
-- When a policy has only one version, this field contains the date and
-- time when the policy was created. When a policy has more than one
-- version, this field contains the date and time when the most recent
-- policy version was created.
policy_updateDate :: Lens.Lens' Policy (Prelude.Maybe Prelude.UTCTime)
policy_updateDate = Lens.lens (\Policy' {updateDate} -> updateDate) (\s@Policy' {} a -> s {updateDate = a} :: Policy) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML Policy where
  parseXML x =
    Policy'
      Prelude.<$> (x Prelude..@? "PolicyName")
      Prelude.<*> (x Prelude..@? "PermissionsBoundaryUsageCount")
      Prelude.<*> (x Prelude..@? "IsAttachable")
      Prelude.<*> (x Prelude..@? "CreateDate")
      Prelude.<*> (x Prelude..@? "Arn")
      Prelude.<*> (x Prelude..@? "AttachmentCount")
      Prelude.<*> (x Prelude..@? "DefaultVersionId")
      Prelude.<*> ( x Prelude..@? "Tags" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "Path")
      Prelude.<*> (x Prelude..@? "PolicyId")
      Prelude.<*> (x Prelude..@? "UpdateDate")

instance Prelude.Hashable Policy

instance Prelude.NFData Policy
