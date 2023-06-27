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
-- Module      : Amazonka.SecurityHub.Types.AwsIamPolicyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamPolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamPolicyVersion

-- | Represents an IAM permissions policy.
--
-- /See:/ 'newAwsIamPolicyDetails' smart constructor.
data AwsIamPolicyDetails = AwsIamPolicyDetails'
  { -- | The number of users, groups, and roles that the policy is attached to.
    attachmentCount :: Prelude.Maybe Prelude.Int,
    -- | When the policy was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the default version of the policy.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    -- | A description of the policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the policy can be attached to a user, group, or role.
    isAttachable :: Prelude.Maybe Prelude.Bool,
    -- | The path to the policy.
    path :: Prelude.Maybe Prelude.Text,
    -- | The number of users and roles that use the policy to set the permissions
    -- boundary.
    permissionsBoundaryUsageCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier of the policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | List of versions of the policy.
    policyVersionList :: Prelude.Maybe [AwsIamPolicyVersion],
    -- | When the policy was most recently updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    updateDate :: Prelude.Maybe Prelude.Text
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
-- 'attachmentCount', 'awsIamPolicyDetails_attachmentCount' - The number of users, groups, and roles that the policy is attached to.
--
-- 'createDate', 'awsIamPolicyDetails_createDate' - When the policy was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
--
-- 'defaultVersionId', 'awsIamPolicyDetails_defaultVersionId' - The identifier of the default version of the policy.
--
-- 'description', 'awsIamPolicyDetails_description' - A description of the policy.
--
-- 'isAttachable', 'awsIamPolicyDetails_isAttachable' - Whether the policy can be attached to a user, group, or role.
--
-- 'path', 'awsIamPolicyDetails_path' - The path to the policy.
--
-- 'permissionsBoundaryUsageCount', 'awsIamPolicyDetails_permissionsBoundaryUsageCount' - The number of users and roles that use the policy to set the permissions
-- boundary.
--
-- 'policyId', 'awsIamPolicyDetails_policyId' - The unique identifier of the policy.
--
-- 'policyName', 'awsIamPolicyDetails_policyName' - The name of the policy.
--
-- 'policyVersionList', 'awsIamPolicyDetails_policyVersionList' - List of versions of the policy.
--
-- 'updateDate', 'awsIamPolicyDetails_updateDate' - When the policy was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
newAwsIamPolicyDetails ::
  AwsIamPolicyDetails
newAwsIamPolicyDetails =
  AwsIamPolicyDetails'
    { attachmentCount =
        Prelude.Nothing,
      createDate = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      description = Prelude.Nothing,
      isAttachable = Prelude.Nothing,
      path = Prelude.Nothing,
      permissionsBoundaryUsageCount = Prelude.Nothing,
      policyId = Prelude.Nothing,
      policyName = Prelude.Nothing,
      policyVersionList = Prelude.Nothing,
      updateDate = Prelude.Nothing
    }

-- | The number of users, groups, and roles that the policy is attached to.
awsIamPolicyDetails_attachmentCount :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Int)
awsIamPolicyDetails_attachmentCount = Lens.lens (\AwsIamPolicyDetails' {attachmentCount} -> attachmentCount) (\s@AwsIamPolicyDetails' {} a -> s {attachmentCount = a} :: AwsIamPolicyDetails)

-- | When the policy was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsIamPolicyDetails_createDate :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_createDate = Lens.lens (\AwsIamPolicyDetails' {createDate} -> createDate) (\s@AwsIamPolicyDetails' {} a -> s {createDate = a} :: AwsIamPolicyDetails)

-- | The identifier of the default version of the policy.
awsIamPolicyDetails_defaultVersionId :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_defaultVersionId = Lens.lens (\AwsIamPolicyDetails' {defaultVersionId} -> defaultVersionId) (\s@AwsIamPolicyDetails' {} a -> s {defaultVersionId = a} :: AwsIamPolicyDetails)

-- | A description of the policy.
awsIamPolicyDetails_description :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_description = Lens.lens (\AwsIamPolicyDetails' {description} -> description) (\s@AwsIamPolicyDetails' {} a -> s {description = a} :: AwsIamPolicyDetails)

-- | Whether the policy can be attached to a user, group, or role.
awsIamPolicyDetails_isAttachable :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Bool)
awsIamPolicyDetails_isAttachable = Lens.lens (\AwsIamPolicyDetails' {isAttachable} -> isAttachable) (\s@AwsIamPolicyDetails' {} a -> s {isAttachable = a} :: AwsIamPolicyDetails)

-- | The path to the policy.
awsIamPolicyDetails_path :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_path = Lens.lens (\AwsIamPolicyDetails' {path} -> path) (\s@AwsIamPolicyDetails' {} a -> s {path = a} :: AwsIamPolicyDetails)

-- | The number of users and roles that use the policy to set the permissions
-- boundary.
awsIamPolicyDetails_permissionsBoundaryUsageCount :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Int)
awsIamPolicyDetails_permissionsBoundaryUsageCount = Lens.lens (\AwsIamPolicyDetails' {permissionsBoundaryUsageCount} -> permissionsBoundaryUsageCount) (\s@AwsIamPolicyDetails' {} a -> s {permissionsBoundaryUsageCount = a} :: AwsIamPolicyDetails)

-- | The unique identifier of the policy.
awsIamPolicyDetails_policyId :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_policyId = Lens.lens (\AwsIamPolicyDetails' {policyId} -> policyId) (\s@AwsIamPolicyDetails' {} a -> s {policyId = a} :: AwsIamPolicyDetails)

-- | The name of the policy.
awsIamPolicyDetails_policyName :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_policyName = Lens.lens (\AwsIamPolicyDetails' {policyName} -> policyName) (\s@AwsIamPolicyDetails' {} a -> s {policyName = a} :: AwsIamPolicyDetails)

-- | List of versions of the policy.
awsIamPolicyDetails_policyVersionList :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe [AwsIamPolicyVersion])
awsIamPolicyDetails_policyVersionList = Lens.lens (\AwsIamPolicyDetails' {policyVersionList} -> policyVersionList) (\s@AwsIamPolicyDetails' {} a -> s {policyVersionList = a} :: AwsIamPolicyDetails) Prelude.. Lens.mapping Lens.coerced

-- | When the policy was most recently updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsIamPolicyDetails_updateDate :: Lens.Lens' AwsIamPolicyDetails (Prelude.Maybe Prelude.Text)
awsIamPolicyDetails_updateDate = Lens.lens (\AwsIamPolicyDetails' {updateDate} -> updateDate) (\s@AwsIamPolicyDetails' {} a -> s {updateDate = a} :: AwsIamPolicyDetails)

instance Data.FromJSON AwsIamPolicyDetails where
  parseJSON =
    Data.withObject
      "AwsIamPolicyDetails"
      ( \x ->
          AwsIamPolicyDetails'
            Prelude.<$> (x Data..:? "AttachmentCount")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "DefaultVersionId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "IsAttachable")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "PermissionsBoundaryUsageCount")
            Prelude.<*> (x Data..:? "PolicyId")
            Prelude.<*> (x Data..:? "PolicyName")
            Prelude.<*> ( x
                            Data..:? "PolicyVersionList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UpdateDate")
      )

instance Prelude.Hashable AwsIamPolicyDetails where
  hashWithSalt _salt AwsIamPolicyDetails' {..} =
    _salt
      `Prelude.hashWithSalt` attachmentCount
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` defaultVersionId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isAttachable
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` permissionsBoundaryUsageCount
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyVersionList
      `Prelude.hashWithSalt` updateDate

instance Prelude.NFData AwsIamPolicyDetails where
  rnf AwsIamPolicyDetails' {..} =
    Prelude.rnf attachmentCount
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isAttachable
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf permissionsBoundaryUsageCount
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyVersionList
      `Prelude.seq` Prelude.rnf updateDate

instance Data.ToJSON AwsIamPolicyDetails where
  toJSON AwsIamPolicyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttachmentCount" Data..=)
              Prelude.<$> attachmentCount,
            ("CreateDate" Data..=) Prelude.<$> createDate,
            ("DefaultVersionId" Data..=)
              Prelude.<$> defaultVersionId,
            ("Description" Data..=) Prelude.<$> description,
            ("IsAttachable" Data..=) Prelude.<$> isAttachable,
            ("Path" Data..=) Prelude.<$> path,
            ("PermissionsBoundaryUsageCount" Data..=)
              Prelude.<$> permissionsBoundaryUsageCount,
            ("PolicyId" Data..=) Prelude.<$> policyId,
            ("PolicyName" Data..=) Prelude.<$> policyName,
            ("PolicyVersionList" Data..=)
              Prelude.<$> policyVersionList,
            ("UpdateDate" Data..=) Prelude.<$> updateDate
          ]
      )
