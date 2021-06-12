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
-- Module      : Network.AWS.IAM.Types.InstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.InstanceProfile where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.Role
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens

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
    tags :: Core.Maybe [Tag],
    -- | The path to the instance profile. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Text,
    -- | The name identifying the instance profile.
    instanceProfileName :: Core.Text,
    -- | The stable and unique string identifying the instance profile. For more
    -- information about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    instanceProfileId :: Core.Text,
    -- | The Amazon Resource Name (ARN) specifying the instance profile. For more
    -- information about ARNs and how to use them in policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    arn :: Core.Text,
    -- | The date when the instance profile was created.
    createDate :: Core.ISO8601,
    -- | The role associated with the instance profile.
    roles :: [Role]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'instanceProfileName'
  Core.Text ->
  -- | 'instanceProfileId'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  -- | 'createDate'
  Core.UTCTime ->
  InstanceProfile
newInstanceProfile
  pPath_
  pInstanceProfileName_
  pInstanceProfileId_
  pArn_
  pCreateDate_ =
    InstanceProfile'
      { tags = Core.Nothing,
        path = pPath_,
        instanceProfileName = pInstanceProfileName_,
        instanceProfileId = pInstanceProfileId_,
        arn = pArn_,
        createDate = Core._Time Lens.# pCreateDate_,
        roles = Core.mempty
      }

-- | A list of tags that are attached to the instance profile. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
instanceProfile_tags :: Lens.Lens' InstanceProfile (Core.Maybe [Tag])
instanceProfile_tags = Lens.lens (\InstanceProfile' {tags} -> tags) (\s@InstanceProfile' {} a -> s {tags = a} :: InstanceProfile) Core.. Lens.mapping Lens._Coerce

-- | The path to the instance profile. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
instanceProfile_path :: Lens.Lens' InstanceProfile Core.Text
instanceProfile_path = Lens.lens (\InstanceProfile' {path} -> path) (\s@InstanceProfile' {} a -> s {path = a} :: InstanceProfile)

-- | The name identifying the instance profile.
instanceProfile_instanceProfileName :: Lens.Lens' InstanceProfile Core.Text
instanceProfile_instanceProfileName = Lens.lens (\InstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@InstanceProfile' {} a -> s {instanceProfileName = a} :: InstanceProfile)

-- | The stable and unique string identifying the instance profile. For more
-- information about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
instanceProfile_instanceProfileId :: Lens.Lens' InstanceProfile Core.Text
instanceProfile_instanceProfileId = Lens.lens (\InstanceProfile' {instanceProfileId} -> instanceProfileId) (\s@InstanceProfile' {} a -> s {instanceProfileId = a} :: InstanceProfile)

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more
-- information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
instanceProfile_arn :: Lens.Lens' InstanceProfile Core.Text
instanceProfile_arn = Lens.lens (\InstanceProfile' {arn} -> arn) (\s@InstanceProfile' {} a -> s {arn = a} :: InstanceProfile)

-- | The date when the instance profile was created.
instanceProfile_createDate :: Lens.Lens' InstanceProfile Core.UTCTime
instanceProfile_createDate = Lens.lens (\InstanceProfile' {createDate} -> createDate) (\s@InstanceProfile' {} a -> s {createDate = a} :: InstanceProfile) Core.. Core._Time

-- | The role associated with the instance profile.
instanceProfile_roles :: Lens.Lens' InstanceProfile [Role]
instanceProfile_roles = Lens.lens (\InstanceProfile' {roles} -> roles) (\s@InstanceProfile' {} a -> s {roles = a} :: InstanceProfile) Core.. Lens._Coerce

instance Core.FromXML InstanceProfile where
  parseXML x =
    InstanceProfile'
      Core.<$> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@ "Path")
      Core.<*> (x Core..@ "InstanceProfileName")
      Core.<*> (x Core..@ "InstanceProfileId")
      Core.<*> (x Core..@ "Arn")
      Core.<*> (x Core..@ "CreateDate")
      Core.<*> ( x Core..@? "Roles" Core..!@ Core.mempty
                   Core.>>= Core.parseXMLList "member"
               )

instance Core.Hashable InstanceProfile

instance Core.NFData InstanceProfile
