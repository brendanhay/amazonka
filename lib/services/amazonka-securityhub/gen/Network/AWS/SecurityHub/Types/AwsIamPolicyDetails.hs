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
-- Module      : Network.AWS.SecurityHub.Types.AwsIamPolicyDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsIamPolicyDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsIamPolicyVersion

-- | Represents an IAM permissions policy.
--
-- /See:/ 'newAwsIamPolicyDetails' smart constructor.
data AwsIamPolicyDetails = AwsIamPolicyDetails'
  { -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | When the policy was most recently updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    updateDate :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The path to the policy.
    path :: Prelude.Maybe Prelude.Text,
    -- | List of versions of the policy.
    policyVersionList :: Prelude.Maybe [AwsIamPolicyVersion],
    -- | When the policy was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | Whether the policy can be attached to a user, group, or role.
    isAttachable :: Prelude.Maybe Prelude.Bool,
    -- | The number of users and roles that use the policy to set the permissions
    -- boundary.
    permissionsBoundaryUsageCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the default version of the policy.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    -- | The number of users, groups, and roles that the policy is attached to.
    attachmentCount :: Prelude.Maybe Prelude.Int,
    -- | A description of the policy.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamPolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'awsIamPolicyDetails_policyName' - The name of the policy.
--
-- 'updateDate', 'awsIamPolicyDetails_updateDate' - When the policy was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'policyId', 'awsIamPolicyDetails_policyId' - The unique identifier of the policy.
--
-- 'path', 'awsIamPolicyDetails_path' - The path to the policy.
--
-- 'policyVersionList', 'awsIamPolicyDetails_policyVersionList' - List of versions of the policy.
--
-- 'createDate', 'awsIamPolicyDetails_createDate' - When the policy was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'isAttachable', 'awsIamPolicyDetails_isAttachable' - Whether the policy can be attached to a user, group, or role.
--
-- 'permissionsBoundaryUsageCount', 'awsIamPolicyDetails_permissionsBoundaryUsageCount' - The number of users and roles that use the policy to set the permissions
-- boundary.
--
-- 'defaultVersionId', 'awsIamPolicyDetails_defaultVersionId' - The identifier of the default version of the policy.
--
-- 'attachmentCount', 'awsIamPolicyDetails_attachmentCount' - The number of users, groups, and roles that the policy is attached to.
--
-- 'description', 'awsIamPolicyDetails_description' - A description of the policy.
newAwsIamPolicyDetails ::
  AwsIamPolicyDetails
newAwsIamPolicyDetails =
  AwsIamPolicyDetails'
    { policyName = Prelude.Nothing,
      updateDate = Prelude.Nothing,
      policyId = Prelude.Nothing,
      path = Prelude.Nothing,
      policyVersionList = Prelude.Nothing,
      createDate = Prelude.Nothing,
      isAttachable = Prelude.Nothing,
      permissionsBoundaryUsageCount = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      attachmentCount = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The name of the policy.
awsIamPolicyDetails_policyName :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_policyName = Lens.lens (\AwsIamPolicyDetails' {policyName} -> policyName) (\s@AwsIamPolicyDetails' {} a -> s {policyName = a} :: AwsIamPolicyDetails)

-- | When the policy was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamPolicyDetails_updateDate :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_updateDate = Lens.lens (\AwsIamPolicyDetails' {updateDate} -> updateDate) (\s@AwsIamPolicyDetails' {} a -> s {updateDate = a} :: AwsIamPolicyDetails)

-- | The unique identifier of the policy.
awsIamPolicyDetails_policyId :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_policyId = Lens.lens (\AwsIamPolicyDetails' {policyId} -> policyId) (\s@AwsIamPolicyDetails' {} a -> s {policyId = a} :: AwsIamPolicyDetails)

-- | The path to the policy.
awsIamPolicyDetails_path :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_path = Lens.lens (\AwsIamPolicyDetails' {path} -> path) (\s@AwsIamPolicyDetails' {} a -> s {path = a} :: AwsIamPolicyDetails)

-- | List of versions of the policy.
awsIamPolicyDetails_policyVersionList :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe [AwsIamPolicyVersion])
awsIamPolicyDetails_policyVersionList = Lens.lens (\AwsIamPolicyDetails' {policyVersionList} -> policyVersionList) (\s@AwsIamPolicyDetails' {} a -> s {policyVersionList = a} :: AwsIamPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | When the policy was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamPolicyDetails_createDate :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_createDate = Lens.lens (\AwsIamPolicyDetails' {createDate} -> createDate) (\s@AwsIamPolicyDetails' {} a -> s {createDate = a} :: AwsIamPolicyDetails)

-- | Whether the policy can be attached to a user, group, or role.
awsIamPolicyDetails_isAttachable :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Bool)
awsIamPolicyDetails_isAttachable = Lens.lens (\AwsIamPolicyDetails' {isAttachable} -> isAttachable) (\s@AwsIamPolicyDetails' {} a -> s {isAttachable = a} :: AwsIamPolicyDetails)

-- | The number of users and roles that use the policy to set the permissions
-- boundary.
awsIamPolicyDetails_permissionsBoundaryUsageCount :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Int)
awsIamPolicyDetails_permissionsBoundaryUsageCount = Lens.lens (\AwsIamPolicyDetails' {permissionsBoundaryUsageCount} -> permissionsBoundaryUsageCount) (\s@AwsIamPolicyDetails' {} a -> s {permissionsBoundaryUsageCount = a} :: AwsIamPolicyDetails)

-- | The identifier of the default version of the policy.
awsIamPolicyDetails_defaultVersionId :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_defaultVersionId = Lens.lens (\AwsIamPolicyDetails' {defaultVersionId} -> defaultVersionId) (\s@AwsIamPolicyDetails' {} a -> s {defaultVersionId = a} :: AwsIamPolicyDetails)

-- | The number of users, groups, and roles that the policy is attached to.
awsIamPolicyDetails_attachmentCount :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Int)
awsIamPolicyDetails_attachmentCount = Lens.lens (\AwsIamPolicyDetails' {attachmentCount} -> attachmentCount) (\s@AwsIamPolicyDetails' {} a -> s {attachmentCount = a} :: AwsIamPolicyDetails)

-- | A description of the policy.
awsIamPolicyDetails_description :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_description = Lens.lens (\AwsIamPolicyDetails' {description} -> description) (\s@AwsIamPolicyDetails' {} a -> s {description = a} :: AwsIamPolicyDetails)

instance Core.FromJSON AwsIamPolicyDetails where
  parseJSON =
    Core.withObject
      "AwsIamPolicyDetails"
      ( \x ->
          AwsIamPolicyDetails'
            Prelude.<$> (x Core..:? "PolicyName")
            Prelude.<*> (x Core..:? "UpdateDate")
            Prelude.<*> (x Core..:? "PolicyId")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> ( x Core..:? "PolicyVersionList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "IsAttachable")
            Prelude.<*> (x Core..:? "PermissionsBoundaryUsageCount")
            Prelude.<*> (x Core..:? "DefaultVersionId")
            Prelude.<*> (x Core..:? "AttachmentCount")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable AwsIamPolicyDetails

instance Prelude.NFData AwsIamPolicyDetails

instance Core.ToJSON AwsIamPolicyDetails where
  toJSON AwsIamPolicyDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PolicyName" Core..=) Prelude.<$> policyName,
            ("UpdateDate" Core..=) Prelude.<$> updateDate,
            ("PolicyId" Core..=) Prelude.<$> policyId,
            ("Path" Core..=) Prelude.<$> path,
            ("PolicyVersionList" Core..=)
              Prelude.<$> policyVersionList,
            ("CreateDate" Core..=) Prelude.<$> createDate,
            ("IsAttachable" Core..=) Prelude.<$> isAttachable,
            ("PermissionsBoundaryUsageCount" Core..=)
              Prelude.<$> permissionsBoundaryUsageCount,
            ("DefaultVersionId" Core..=)
              Prelude.<$> defaultVersionId,
            ("AttachmentCount" Core..=)
              Prelude.<$> attachmentCount,
            ("Description" Core..=) Prelude.<$> description
          ]
      )
