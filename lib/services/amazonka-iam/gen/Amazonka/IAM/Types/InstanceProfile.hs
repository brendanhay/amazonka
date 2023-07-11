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
-- Module      : Amazonka.IAM.Types.InstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.InstanceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.Role
import Amazonka.IAM.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an instance profile.
--
-- This data type is used as a response element in the following
-- operations:
--
-- -   CreateInstanceProfile
--
-- -   GetInstanceProfile
--
-- -   ListInstanceProfiles
--
-- -   ListInstanceProfilesForRole
--
-- /See:/ 'newInstanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { -- | A list of tags that are attached to the instance profile. For more
    -- information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The path to the instance profile. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Text,
    -- | The name identifying the instance profile.
    instanceProfileName :: Prelude.Text,
    -- | The stable and unique string identifying the instance profile. For more
    -- information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    instanceProfileId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) specifying the instance profile. For more
    -- information about ARNs and how to use them in policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    arn :: Prelude.Text,
    -- | The date when the instance profile was created.
    createDate :: Data.ISO8601,
    -- | The role associated with the instance profile.
    roles :: [Role]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'instanceProfile_tags' - A list of tags that are attached to the instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'path', 'instanceProfile_path' - The path to the instance profile. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'instanceProfileName', 'instanceProfile_instanceProfileName' - The name identifying the instance profile.
--
-- 'instanceProfileId', 'instanceProfile_instanceProfileId' - The stable and unique string identifying the instance profile. For more
-- information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'arn', 'instanceProfile_arn' - The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'createDate', 'instanceProfile_createDate' - The date when the instance profile was created.
--
-- 'roles', 'instanceProfile_roles' - The role associated with the instance profile.
newInstanceProfile ::
  -- | 'path'
  Prelude.Text ->
  -- | 'instanceProfileName'
  Prelude.Text ->
  -- | 'instanceProfileId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createDate'
  Prelude.UTCTime ->
  InstanceProfile
newInstanceProfile
  pPath_
  pInstanceProfileName_
  pInstanceProfileId_
  pArn_
  pCreateDate_ =
    InstanceProfile'
      { tags = Prelude.Nothing,
        path = pPath_,
        instanceProfileName = pInstanceProfileName_,
        instanceProfileId = pInstanceProfileId_,
        arn = pArn_,
        createDate = Data._Time Lens.# pCreateDate_,
        roles = Prelude.mempty
      }

-- | A list of tags that are attached to the instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
instanceProfile_tags :: Lens.Lens' InstanceProfile (Prelude.Maybe [Tag])
instanceProfile_tags = Lens.lens (\InstanceProfile' {tags} -> tags) (\s@InstanceProfile' {} a -> s {tags = a} :: InstanceProfile) Prelude.. Lens.mapping Lens.coerced

-- | The path to the instance profile. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
instanceProfile_path :: Lens.Lens' InstanceProfile Prelude.Text
instanceProfile_path = Lens.lens (\InstanceProfile' {path} -> path) (\s@InstanceProfile' {} a -> s {path = a} :: InstanceProfile)

-- | The name identifying the instance profile.
instanceProfile_instanceProfileName :: Lens.Lens' InstanceProfile Prelude.Text
instanceProfile_instanceProfileName = Lens.lens (\InstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@InstanceProfile' {} a -> s {instanceProfileName = a} :: InstanceProfile)

-- | The stable and unique string identifying the instance profile. For more
-- information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
instanceProfile_instanceProfileId :: Lens.Lens' InstanceProfile Prelude.Text
instanceProfile_instanceProfileId = Lens.lens (\InstanceProfile' {instanceProfileId} -> instanceProfileId) (\s@InstanceProfile' {} a -> s {instanceProfileId = a} :: InstanceProfile)

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
instanceProfile_arn :: Lens.Lens' InstanceProfile Prelude.Text
instanceProfile_arn = Lens.lens (\InstanceProfile' {arn} -> arn) (\s@InstanceProfile' {} a -> s {arn = a} :: InstanceProfile)

-- | The date when the instance profile was created.
instanceProfile_createDate :: Lens.Lens' InstanceProfile Prelude.UTCTime
instanceProfile_createDate = Lens.lens (\InstanceProfile' {createDate} -> createDate) (\s@InstanceProfile' {} a -> s {createDate = a} :: InstanceProfile) Prelude.. Data._Time

-- | The role associated with the instance profile.
instanceProfile_roles :: Lens.Lens' InstanceProfile [Role]
instanceProfile_roles = Lens.lens (\InstanceProfile' {roles} -> roles) (\s@InstanceProfile' {} a -> s {roles = a} :: InstanceProfile) Prelude.. Lens.coerced

instance Data.FromXML InstanceProfile where
  parseXML x =
    InstanceProfile'
      Prelude.<$> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@ "Path")
      Prelude.<*> (x Data..@ "InstanceProfileName")
      Prelude.<*> (x Data..@ "InstanceProfileId")
      Prelude.<*> (x Data..@ "Arn")
      Prelude.<*> (x Data..@ "CreateDate")
      Prelude.<*> ( x
                      Data..@? "Roles"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList "member"
                  )

instance Prelude.Hashable InstanceProfile where
  hashWithSalt _salt InstanceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` instanceProfileName
      `Prelude.hashWithSalt` instanceProfileId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` roles

instance Prelude.NFData InstanceProfile where
  rnf InstanceProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf instanceProfileName
      `Prelude.seq` Prelude.rnf instanceProfileId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf roles
