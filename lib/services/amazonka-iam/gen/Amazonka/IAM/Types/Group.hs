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
-- Module      : Amazonka.IAM.Types.Group
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an IAM group entity.
--
-- This data type is used as a response element in the following
-- operations:
--
-- -   CreateGroup
--
-- -   GetGroup
--
-- -   ListGroups
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The path to the group. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Text,
    -- | The friendly name that identifies the group.
    groupName :: Prelude.Text,
    -- | The stable and unique string identifying the group. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    groupId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) specifying the group. For more
    -- information about ARNs and how to use them in policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    arn :: Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- group was created.
    createDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'group_path' - The path to the group. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'groupName', 'group_groupName' - The friendly name that identifies the group.
--
-- 'groupId', 'group_groupId' - The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'arn', 'group_arn' - The Amazon Resource Name (ARN) specifying the group. For more
-- information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'createDate', 'group_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
newGroup ::
  -- | 'path'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createDate'
  Prelude.UTCTime ->
  Group
newGroup
  pPath_
  pGroupName_
  pGroupId_
  pArn_
  pCreateDate_ =
    Group'
      { path = pPath_,
        groupName = pGroupName_,
        groupId = pGroupId_,
        arn = pArn_,
        createDate = Data._Time Lens.# pCreateDate_
      }

-- | The path to the group. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
group_path :: Lens.Lens' Group Prelude.Text
group_path = Lens.lens (\Group' {path} -> path) (\s@Group' {} a -> s {path = a} :: Group)

-- | The friendly name that identifies the group.
group_groupName :: Lens.Lens' Group Prelude.Text
group_groupName = Lens.lens (\Group' {groupName} -> groupName) (\s@Group' {} a -> s {groupName = a} :: Group)

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
group_groupId :: Lens.Lens' Group Prelude.Text
group_groupId = Lens.lens (\Group' {groupId} -> groupId) (\s@Group' {} a -> s {groupId = a} :: Group)

-- | The Amazon Resource Name (ARN) specifying the group. For more
-- information about ARNs and how to use them in policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
group_arn :: Lens.Lens' Group Prelude.Text
group_arn = Lens.lens (\Group' {arn} -> arn) (\s@Group' {} a -> s {arn = a} :: Group)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
group_createDate :: Lens.Lens' Group Prelude.UTCTime
group_createDate = Lens.lens (\Group' {createDate} -> createDate) (\s@Group' {} a -> s {createDate = a} :: Group) Prelude.. Data._Time

instance Data.FromXML Group where
  parseXML x =
    Group'
      Prelude.<$> (x Data..@ "Path")
      Prelude.<*> (x Data..@ "GroupName")
      Prelude.<*> (x Data..@ "GroupId")
      Prelude.<*> (x Data..@ "Arn")
      Prelude.<*> (x Data..@ "CreateDate")

instance Prelude.Hashable Group where
  hashWithSalt _salt Group' {..} =
    _salt
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createDate

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createDate
