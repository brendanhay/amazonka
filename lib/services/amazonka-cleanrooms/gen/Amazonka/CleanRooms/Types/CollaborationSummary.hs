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
-- Module      : Amazonka.CleanRooms.Types.CollaborationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.CollaborationSummary where

import Amazonka.CleanRooms.Types.MemberStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata of the collaboration.
--
-- /See:/ 'newCollaborationSummary' smart constructor.
data CollaborationSummary = CollaborationSummary'
  { -- | The ARN of a member in a collaboration.
    membershipArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a member in a collaboration.
    membershipId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the collaboration.
    id :: Prelude.Text,
    -- | The ARN of the collaboration.
    arn :: Prelude.Text,
    -- | A human-readable identifier provided by the collaboration owner. Display
    -- names are not unique.
    name :: Prelude.Text,
    -- | The identifier used to reference members of the collaboration. Currently
    -- only supports AWS Account ID.
    creatorAccountId :: Prelude.Text,
    -- | The display name of the collaboration creator.
    creatorDisplayName :: Prelude.Text,
    -- | The time when the collaboration was created.
    createTime :: Data.POSIX,
    -- | The time the collaboration metadata was last updated.
    updateTime :: Data.POSIX,
    -- | The status of a member in a collaboration.
    memberStatus :: MemberStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CollaborationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipArn', 'collaborationSummary_membershipArn' - The ARN of a member in a collaboration.
--
-- 'membershipId', 'collaborationSummary_membershipId' - The identifier of a member in a collaboration.
--
-- 'id', 'collaborationSummary_id' - The identifier for the collaboration.
--
-- 'arn', 'collaborationSummary_arn' - The ARN of the collaboration.
--
-- 'name', 'collaborationSummary_name' - A human-readable identifier provided by the collaboration owner. Display
-- names are not unique.
--
-- 'creatorAccountId', 'collaborationSummary_creatorAccountId' - The identifier used to reference members of the collaboration. Currently
-- only supports AWS Account ID.
--
-- 'creatorDisplayName', 'collaborationSummary_creatorDisplayName' - The display name of the collaboration creator.
--
-- 'createTime', 'collaborationSummary_createTime' - The time when the collaboration was created.
--
-- 'updateTime', 'collaborationSummary_updateTime' - The time the collaboration metadata was last updated.
--
-- 'memberStatus', 'collaborationSummary_memberStatus' - The status of a member in a collaboration.
newCollaborationSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'creatorAccountId'
  Prelude.Text ->
  -- | 'creatorDisplayName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'memberStatus'
  MemberStatus ->
  CollaborationSummary
newCollaborationSummary
  pId_
  pArn_
  pName_
  pCreatorAccountId_
  pCreatorDisplayName_
  pCreateTime_
  pUpdateTime_
  pMemberStatus_ =
    CollaborationSummary'
      { membershipArn =
          Prelude.Nothing,
        membershipId = Prelude.Nothing,
        id = pId_,
        arn = pArn_,
        name = pName_,
        creatorAccountId = pCreatorAccountId_,
        creatorDisplayName = pCreatorDisplayName_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        memberStatus = pMemberStatus_
      }

-- | The ARN of a member in a collaboration.
collaborationSummary_membershipArn :: Lens.Lens' CollaborationSummary (Prelude.Maybe Prelude.Text)
collaborationSummary_membershipArn = Lens.lens (\CollaborationSummary' {membershipArn} -> membershipArn) (\s@CollaborationSummary' {} a -> s {membershipArn = a} :: CollaborationSummary)

-- | The identifier of a member in a collaboration.
collaborationSummary_membershipId :: Lens.Lens' CollaborationSummary (Prelude.Maybe Prelude.Text)
collaborationSummary_membershipId = Lens.lens (\CollaborationSummary' {membershipId} -> membershipId) (\s@CollaborationSummary' {} a -> s {membershipId = a} :: CollaborationSummary)

-- | The identifier for the collaboration.
collaborationSummary_id :: Lens.Lens' CollaborationSummary Prelude.Text
collaborationSummary_id = Lens.lens (\CollaborationSummary' {id} -> id) (\s@CollaborationSummary' {} a -> s {id = a} :: CollaborationSummary)

-- | The ARN of the collaboration.
collaborationSummary_arn :: Lens.Lens' CollaborationSummary Prelude.Text
collaborationSummary_arn = Lens.lens (\CollaborationSummary' {arn} -> arn) (\s@CollaborationSummary' {} a -> s {arn = a} :: CollaborationSummary)

-- | A human-readable identifier provided by the collaboration owner. Display
-- names are not unique.
collaborationSummary_name :: Lens.Lens' CollaborationSummary Prelude.Text
collaborationSummary_name = Lens.lens (\CollaborationSummary' {name} -> name) (\s@CollaborationSummary' {} a -> s {name = a} :: CollaborationSummary)

-- | The identifier used to reference members of the collaboration. Currently
-- only supports AWS Account ID.
collaborationSummary_creatorAccountId :: Lens.Lens' CollaborationSummary Prelude.Text
collaborationSummary_creatorAccountId = Lens.lens (\CollaborationSummary' {creatorAccountId} -> creatorAccountId) (\s@CollaborationSummary' {} a -> s {creatorAccountId = a} :: CollaborationSummary)

-- | The display name of the collaboration creator.
collaborationSummary_creatorDisplayName :: Lens.Lens' CollaborationSummary Prelude.Text
collaborationSummary_creatorDisplayName = Lens.lens (\CollaborationSummary' {creatorDisplayName} -> creatorDisplayName) (\s@CollaborationSummary' {} a -> s {creatorDisplayName = a} :: CollaborationSummary)

-- | The time when the collaboration was created.
collaborationSummary_createTime :: Lens.Lens' CollaborationSummary Prelude.UTCTime
collaborationSummary_createTime = Lens.lens (\CollaborationSummary' {createTime} -> createTime) (\s@CollaborationSummary' {} a -> s {createTime = a} :: CollaborationSummary) Prelude.. Data._Time

-- | The time the collaboration metadata was last updated.
collaborationSummary_updateTime :: Lens.Lens' CollaborationSummary Prelude.UTCTime
collaborationSummary_updateTime = Lens.lens (\CollaborationSummary' {updateTime} -> updateTime) (\s@CollaborationSummary' {} a -> s {updateTime = a} :: CollaborationSummary) Prelude.. Data._Time

-- | The status of a member in a collaboration.
collaborationSummary_memberStatus :: Lens.Lens' CollaborationSummary MemberStatus
collaborationSummary_memberStatus = Lens.lens (\CollaborationSummary' {memberStatus} -> memberStatus) (\s@CollaborationSummary' {} a -> s {memberStatus = a} :: CollaborationSummary)

instance Data.FromJSON CollaborationSummary where
  parseJSON =
    Data.withObject
      "CollaborationSummary"
      ( \x ->
          CollaborationSummary'
            Prelude.<$> (x Data..:? "membershipArn")
            Prelude.<*> (x Data..:? "membershipId")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "creatorAccountId")
            Prelude.<*> (x Data..: "creatorDisplayName")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "memberStatus")
      )

instance Prelude.Hashable CollaborationSummary where
  hashWithSalt _salt CollaborationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` membershipArn
      `Prelude.hashWithSalt` membershipId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creatorAccountId
      `Prelude.hashWithSalt` creatorDisplayName
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` memberStatus

instance Prelude.NFData CollaborationSummary where
  rnf CollaborationSummary' {..} =
    Prelude.rnf membershipArn
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf creatorAccountId
      `Prelude.seq` Prelude.rnf creatorDisplayName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf memberStatus
