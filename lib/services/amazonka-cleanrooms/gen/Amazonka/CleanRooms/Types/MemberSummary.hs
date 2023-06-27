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
-- Module      : Amazonka.CleanRooms.Types.MemberSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.MemberSummary where

import Amazonka.CleanRooms.Types.MemberAbility
import Amazonka.CleanRooms.Types.MemberStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The member object listed by the request.
--
-- /See:/ 'newMemberSummary' smart constructor.
data MemberSummary = MemberSummary'
  { -- | The unique ARN for the member\'s associated membership, if present.
    membershipArn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the member\'s associated membership, if present.
    membershipId :: Prelude.Maybe Prelude.Text,
    -- | The identifier used to reference members of the collaboration. Currently
    -- only supports AWS Account ID.
    accountId :: Prelude.Text,
    -- | The status of the member. Valid values are \`INVITED\`, \`ACTIVE\`,
    -- \`LEFT\`, and \`REMOVED\`.
    status :: MemberStatus,
    -- | The member\'s display name.
    displayName :: Prelude.Text,
    -- | The abilities granted to the collaboration member.
    abilities :: [MemberAbility],
    -- | The time when the member was created.
    createTime :: Data.POSIX,
    -- | The time the member metadata was last updated.
    updateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipArn', 'memberSummary_membershipArn' - The unique ARN for the member\'s associated membership, if present.
--
-- 'membershipId', 'memberSummary_membershipId' - The unique ID for the member\'s associated membership, if present.
--
-- 'accountId', 'memberSummary_accountId' - The identifier used to reference members of the collaboration. Currently
-- only supports AWS Account ID.
--
-- 'status', 'memberSummary_status' - The status of the member. Valid values are \`INVITED\`, \`ACTIVE\`,
-- \`LEFT\`, and \`REMOVED\`.
--
-- 'displayName', 'memberSummary_displayName' - The member\'s display name.
--
-- 'abilities', 'memberSummary_abilities' - The abilities granted to the collaboration member.
--
-- 'createTime', 'memberSummary_createTime' - The time when the member was created.
--
-- 'updateTime', 'memberSummary_updateTime' - The time the member metadata was last updated.
newMemberSummary ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'status'
  MemberStatus ->
  -- | 'displayName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  MemberSummary
newMemberSummary
  pAccountId_
  pStatus_
  pDisplayName_
  pCreateTime_
  pUpdateTime_ =
    MemberSummary'
      { membershipArn = Prelude.Nothing,
        membershipId = Prelude.Nothing,
        accountId = pAccountId_,
        status = pStatus_,
        displayName = pDisplayName_,
        abilities = Prelude.mempty,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The unique ARN for the member\'s associated membership, if present.
memberSummary_membershipArn :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.Text)
memberSummary_membershipArn = Lens.lens (\MemberSummary' {membershipArn} -> membershipArn) (\s@MemberSummary' {} a -> s {membershipArn = a} :: MemberSummary)

-- | The unique ID for the member\'s associated membership, if present.
memberSummary_membershipId :: Lens.Lens' MemberSummary (Prelude.Maybe Prelude.Text)
memberSummary_membershipId = Lens.lens (\MemberSummary' {membershipId} -> membershipId) (\s@MemberSummary' {} a -> s {membershipId = a} :: MemberSummary)

-- | The identifier used to reference members of the collaboration. Currently
-- only supports AWS Account ID.
memberSummary_accountId :: Lens.Lens' MemberSummary Prelude.Text
memberSummary_accountId = Lens.lens (\MemberSummary' {accountId} -> accountId) (\s@MemberSummary' {} a -> s {accountId = a} :: MemberSummary)

-- | The status of the member. Valid values are \`INVITED\`, \`ACTIVE\`,
-- \`LEFT\`, and \`REMOVED\`.
memberSummary_status :: Lens.Lens' MemberSummary MemberStatus
memberSummary_status = Lens.lens (\MemberSummary' {status} -> status) (\s@MemberSummary' {} a -> s {status = a} :: MemberSummary)

-- | The member\'s display name.
memberSummary_displayName :: Lens.Lens' MemberSummary Prelude.Text
memberSummary_displayName = Lens.lens (\MemberSummary' {displayName} -> displayName) (\s@MemberSummary' {} a -> s {displayName = a} :: MemberSummary)

-- | The abilities granted to the collaboration member.
memberSummary_abilities :: Lens.Lens' MemberSummary [MemberAbility]
memberSummary_abilities = Lens.lens (\MemberSummary' {abilities} -> abilities) (\s@MemberSummary' {} a -> s {abilities = a} :: MemberSummary) Prelude.. Lens.coerced

-- | The time when the member was created.
memberSummary_createTime :: Lens.Lens' MemberSummary Prelude.UTCTime
memberSummary_createTime = Lens.lens (\MemberSummary' {createTime} -> createTime) (\s@MemberSummary' {} a -> s {createTime = a} :: MemberSummary) Prelude.. Data._Time

-- | The time the member metadata was last updated.
memberSummary_updateTime :: Lens.Lens' MemberSummary Prelude.UTCTime
memberSummary_updateTime = Lens.lens (\MemberSummary' {updateTime} -> updateTime) (\s@MemberSummary' {} a -> s {updateTime = a} :: MemberSummary) Prelude.. Data._Time

instance Data.FromJSON MemberSummary where
  parseJSON =
    Data.withObject
      "MemberSummary"
      ( \x ->
          MemberSummary'
            Prelude.<$> (x Data..:? "membershipArn")
            Prelude.<*> (x Data..:? "membershipId")
            Prelude.<*> (x Data..: "accountId")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "displayName")
            Prelude.<*> (x Data..:? "abilities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable MemberSummary where
  hashWithSalt _salt MemberSummary' {..} =
    _salt
      `Prelude.hashWithSalt` membershipArn
      `Prelude.hashWithSalt` membershipId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` abilities
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData MemberSummary where
  rnf MemberSummary' {..} =
    Prelude.rnf membershipArn
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf abilities
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
