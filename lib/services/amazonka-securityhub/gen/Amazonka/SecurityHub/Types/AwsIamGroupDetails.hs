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
-- Module      : Amazonka.SecurityHub.Types.AwsIamGroupDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamGroupPolicy

-- | Contains details about an IAM group.
--
-- /See:/ 'newAwsIamGroupDetails' smart constructor.
data AwsIamGroupDetails = AwsIamGroupDetails'
  { -- | A list of the managed policies that are attached to the IAM group.
    attachedManagedPolicies :: Prelude.Maybe [AwsIamAttachedManagedPolicy],
    -- | Indicates when the IAM group was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the IAM group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The list of inline policies that are embedded in the group.
    groupPolicyList :: Prelude.Maybe [AwsIamGroupPolicy],
    -- | The path to the group.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedManagedPolicies', 'awsIamGroupDetails_attachedManagedPolicies' - A list of the managed policies that are attached to the IAM group.
--
-- 'createDate', 'awsIamGroupDetails_createDate' - Indicates when the IAM group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'groupId', 'awsIamGroupDetails_groupId' - The identifier of the IAM group.
--
-- 'groupName', 'awsIamGroupDetails_groupName' - The name of the IAM group.
--
-- 'groupPolicyList', 'awsIamGroupDetails_groupPolicyList' - The list of inline policies that are embedded in the group.
--
-- 'path', 'awsIamGroupDetails_path' - The path to the group.
newAwsIamGroupDetails ::
  AwsIamGroupDetails
newAwsIamGroupDetails =
  AwsIamGroupDetails'
    { attachedManagedPolicies =
        Prelude.Nothing,
      createDate = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      groupPolicyList = Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | A list of the managed policies that are attached to the IAM group.
awsIamGroupDetails_attachedManagedPolicies :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe [AwsIamAttachedManagedPolicy])
awsIamGroupDetails_attachedManagedPolicies = Lens.lens (\AwsIamGroupDetails' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@AwsIamGroupDetails' {} a -> s {attachedManagedPolicies = a} :: AwsIamGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the IAM group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamGroupDetails_createDate :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_createDate = Lens.lens (\AwsIamGroupDetails' {createDate} -> createDate) (\s@AwsIamGroupDetails' {} a -> s {createDate = a} :: AwsIamGroupDetails)

-- | The identifier of the IAM group.
awsIamGroupDetails_groupId :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_groupId = Lens.lens (\AwsIamGroupDetails' {groupId} -> groupId) (\s@AwsIamGroupDetails' {} a -> s {groupId = a} :: AwsIamGroupDetails)

-- | The name of the IAM group.
awsIamGroupDetails_groupName :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_groupName = Lens.lens (\AwsIamGroupDetails' {groupName} -> groupName) (\s@AwsIamGroupDetails' {} a -> s {groupName = a} :: AwsIamGroupDetails)

-- | The list of inline policies that are embedded in the group.
awsIamGroupDetails_groupPolicyList :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe [AwsIamGroupPolicy])
awsIamGroupDetails_groupPolicyList = Lens.lens (\AwsIamGroupDetails' {groupPolicyList} -> groupPolicyList) (\s@AwsIamGroupDetails' {} a -> s {groupPolicyList = a} :: AwsIamGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | The path to the group.
awsIamGroupDetails_path :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_path = Lens.lens (\AwsIamGroupDetails' {path} -> path) (\s@AwsIamGroupDetails' {} a -> s {path = a} :: AwsIamGroupDetails)

instance Data.FromJSON AwsIamGroupDetails where
  parseJSON =
    Data.withObject
      "AwsIamGroupDetails"
      ( \x ->
          AwsIamGroupDetails'
            Prelude.<$> ( x Data..:? "AttachedManagedPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "GroupId")
            Prelude.<*> (x Data..:? "GroupName")
            Prelude.<*> ( x Data..:? "GroupPolicyList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Path")
      )

instance Prelude.Hashable AwsIamGroupDetails where
  hashWithSalt _salt AwsIamGroupDetails' {..} =
    _salt
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupPolicyList
      `Prelude.hashWithSalt` path

instance Prelude.NFData AwsIamGroupDetails where
  rnf AwsIamGroupDetails' {..} =
    Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupPolicyList
      `Prelude.seq` Prelude.rnf path

instance Data.ToJSON AwsIamGroupDetails where
  toJSON AwsIamGroupDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttachedManagedPolicies" Data..=)
              Prelude.<$> attachedManagedPolicies,
            ("CreateDate" Data..=) Prelude.<$> createDate,
            ("GroupId" Data..=) Prelude.<$> groupId,
            ("GroupName" Data..=) Prelude.<$> groupName,
            ("GroupPolicyList" Data..=)
              Prelude.<$> groupPolicyList,
            ("Path" Data..=) Prelude.<$> path
          ]
      )
