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
-- Module      : Amazonka.CleanRooms.Types.Collaboration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.Collaboration where

import Amazonka.CleanRooms.Types.CollaborationQueryLogStatus
import Amazonka.CleanRooms.Types.DataEncryptionMetadata
import Amazonka.CleanRooms.Types.MemberStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The multi-party data share environment. The collaboration contains
-- metadata about its purpose and participants.
--
-- /See:/ 'newCollaboration' smart constructor.
data Collaboration = Collaboration'
  { -- | The settings for client-side encryption for cryptographic computing.
    dataEncryptionMetadata :: Prelude.Maybe DataEncryptionMetadata,
    -- | A description of the collaboration provided by the collaboration owner.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique ARN for your membership within the collaboration.
    membershipArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for your membership within the collaboration.
    membershipId :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the collaboration.
    id :: Prelude.Text,
    -- | The unique ARN for the collaboration.
    arn :: Prelude.Text,
    -- | A human-readable identifier provided by the collaboration owner. Display
    -- names are not unique.
    name :: Prelude.Text,
    -- | The identifier used to reference members of the collaboration. Currently
    -- only supports AWS account ID.
    creatorAccountId :: Prelude.Text,
    -- | A display name of the collaboration creator.
    creatorDisplayName :: Prelude.Text,
    -- | The time when the collaboration was created.
    createTime :: Data.POSIX,
    -- | The time the collaboration metadata was last updated.
    updateTime :: Data.POSIX,
    -- | The status of a member in a collaboration.
    memberStatus :: MemberStatus,
    -- | An indicator as to whether query logging has been enabled or disabled
    -- for the collaboration.
    queryLogStatus :: CollaborationQueryLogStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Collaboration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataEncryptionMetadata', 'collaboration_dataEncryptionMetadata' - The settings for client-side encryption for cryptographic computing.
--
-- 'description', 'collaboration_description' - A description of the collaboration provided by the collaboration owner.
--
-- 'membershipArn', 'collaboration_membershipArn' - The unique ARN for your membership within the collaboration.
--
-- 'membershipId', 'collaboration_membershipId' - The unique ID for your membership within the collaboration.
--
-- 'id', 'collaboration_id' - The unique ID for the collaboration.
--
-- 'arn', 'collaboration_arn' - The unique ARN for the collaboration.
--
-- 'name', 'collaboration_name' - A human-readable identifier provided by the collaboration owner. Display
-- names are not unique.
--
-- 'creatorAccountId', 'collaboration_creatorAccountId' - The identifier used to reference members of the collaboration. Currently
-- only supports AWS account ID.
--
-- 'creatorDisplayName', 'collaboration_creatorDisplayName' - A display name of the collaboration creator.
--
-- 'createTime', 'collaboration_createTime' - The time when the collaboration was created.
--
-- 'updateTime', 'collaboration_updateTime' - The time the collaboration metadata was last updated.
--
-- 'memberStatus', 'collaboration_memberStatus' - The status of a member in a collaboration.
--
-- 'queryLogStatus', 'collaboration_queryLogStatus' - An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
newCollaboration ::
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
  -- | 'queryLogStatus'
  CollaborationQueryLogStatus ->
  Collaboration
newCollaboration
  pId_
  pArn_
  pName_
  pCreatorAccountId_
  pCreatorDisplayName_
  pCreateTime_
  pUpdateTime_
  pMemberStatus_
  pQueryLogStatus_ =
    Collaboration'
      { dataEncryptionMetadata =
          Prelude.Nothing,
        description = Prelude.Nothing,
        membershipArn = Prelude.Nothing,
        membershipId = Prelude.Nothing,
        id = pId_,
        arn = pArn_,
        name = pName_,
        creatorAccountId = pCreatorAccountId_,
        creatorDisplayName = pCreatorDisplayName_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        memberStatus = pMemberStatus_,
        queryLogStatus = pQueryLogStatus_
      }

-- | The settings for client-side encryption for cryptographic computing.
collaboration_dataEncryptionMetadata :: Lens.Lens' Collaboration (Prelude.Maybe DataEncryptionMetadata)
collaboration_dataEncryptionMetadata = Lens.lens (\Collaboration' {dataEncryptionMetadata} -> dataEncryptionMetadata) (\s@Collaboration' {} a -> s {dataEncryptionMetadata = a} :: Collaboration)

-- | A description of the collaboration provided by the collaboration owner.
collaboration_description :: Lens.Lens' Collaboration (Prelude.Maybe Prelude.Text)
collaboration_description = Lens.lens (\Collaboration' {description} -> description) (\s@Collaboration' {} a -> s {description = a} :: Collaboration)

-- | The unique ARN for your membership within the collaboration.
collaboration_membershipArn :: Lens.Lens' Collaboration (Prelude.Maybe Prelude.Text)
collaboration_membershipArn = Lens.lens (\Collaboration' {membershipArn} -> membershipArn) (\s@Collaboration' {} a -> s {membershipArn = a} :: Collaboration)

-- | The unique ID for your membership within the collaboration.
collaboration_membershipId :: Lens.Lens' Collaboration (Prelude.Maybe Prelude.Text)
collaboration_membershipId = Lens.lens (\Collaboration' {membershipId} -> membershipId) (\s@Collaboration' {} a -> s {membershipId = a} :: Collaboration)

-- | The unique ID for the collaboration.
collaboration_id :: Lens.Lens' Collaboration Prelude.Text
collaboration_id = Lens.lens (\Collaboration' {id} -> id) (\s@Collaboration' {} a -> s {id = a} :: Collaboration)

-- | The unique ARN for the collaboration.
collaboration_arn :: Lens.Lens' Collaboration Prelude.Text
collaboration_arn = Lens.lens (\Collaboration' {arn} -> arn) (\s@Collaboration' {} a -> s {arn = a} :: Collaboration)

-- | A human-readable identifier provided by the collaboration owner. Display
-- names are not unique.
collaboration_name :: Lens.Lens' Collaboration Prelude.Text
collaboration_name = Lens.lens (\Collaboration' {name} -> name) (\s@Collaboration' {} a -> s {name = a} :: Collaboration)

-- | The identifier used to reference members of the collaboration. Currently
-- only supports AWS account ID.
collaboration_creatorAccountId :: Lens.Lens' Collaboration Prelude.Text
collaboration_creatorAccountId = Lens.lens (\Collaboration' {creatorAccountId} -> creatorAccountId) (\s@Collaboration' {} a -> s {creatorAccountId = a} :: Collaboration)

-- | A display name of the collaboration creator.
collaboration_creatorDisplayName :: Lens.Lens' Collaboration Prelude.Text
collaboration_creatorDisplayName = Lens.lens (\Collaboration' {creatorDisplayName} -> creatorDisplayName) (\s@Collaboration' {} a -> s {creatorDisplayName = a} :: Collaboration)

-- | The time when the collaboration was created.
collaboration_createTime :: Lens.Lens' Collaboration Prelude.UTCTime
collaboration_createTime = Lens.lens (\Collaboration' {createTime} -> createTime) (\s@Collaboration' {} a -> s {createTime = a} :: Collaboration) Prelude.. Data._Time

-- | The time the collaboration metadata was last updated.
collaboration_updateTime :: Lens.Lens' Collaboration Prelude.UTCTime
collaboration_updateTime = Lens.lens (\Collaboration' {updateTime} -> updateTime) (\s@Collaboration' {} a -> s {updateTime = a} :: Collaboration) Prelude.. Data._Time

-- | The status of a member in a collaboration.
collaboration_memberStatus :: Lens.Lens' Collaboration MemberStatus
collaboration_memberStatus = Lens.lens (\Collaboration' {memberStatus} -> memberStatus) (\s@Collaboration' {} a -> s {memberStatus = a} :: Collaboration)

-- | An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
collaboration_queryLogStatus :: Lens.Lens' Collaboration CollaborationQueryLogStatus
collaboration_queryLogStatus = Lens.lens (\Collaboration' {queryLogStatus} -> queryLogStatus) (\s@Collaboration' {} a -> s {queryLogStatus = a} :: Collaboration)

instance Data.FromJSON Collaboration where
  parseJSON =
    Data.withObject
      "Collaboration"
      ( \x ->
          Collaboration'
            Prelude.<$> (x Data..:? "dataEncryptionMetadata")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "membershipArn")
            Prelude.<*> (x Data..:? "membershipId")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "creatorAccountId")
            Prelude.<*> (x Data..: "creatorDisplayName")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "memberStatus")
            Prelude.<*> (x Data..: "queryLogStatus")
      )

instance Prelude.Hashable Collaboration where
  hashWithSalt _salt Collaboration' {..} =
    _salt
      `Prelude.hashWithSalt` dataEncryptionMetadata
      `Prelude.hashWithSalt` description
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
      `Prelude.hashWithSalt` queryLogStatus

instance Prelude.NFData Collaboration where
  rnf Collaboration' {..} =
    Prelude.rnf dataEncryptionMetadata
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf membershipArn
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf creatorAccountId
      `Prelude.seq` Prelude.rnf creatorDisplayName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf memberStatus
      `Prelude.seq` Prelude.rnf queryLogStatus
