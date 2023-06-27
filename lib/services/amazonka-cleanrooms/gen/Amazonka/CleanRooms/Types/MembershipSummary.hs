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
-- Module      : Amazonka.CleanRooms.Types.MembershipSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.MembershipSummary where

import Amazonka.CleanRooms.Types.MemberAbility
import Amazonka.CleanRooms.Types.MembershipStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The membership object listed by the request.
--
-- /See:/ 'newMembershipSummary' smart constructor.
data MembershipSummary = MembershipSummary'
  { -- | The unique ID for the membership\'s collaboration.
    id :: Prelude.Text,
    -- | The unique ARN for the membership.
    arn :: Prelude.Text,
    -- | The unique ARN for the membership\'s associated collaboration.
    collaborationArn :: Prelude.Text,
    -- | The unique ID for the membership\'s collaboration.
    collaborationId :: Prelude.Text,
    -- | The identifier of the AWS principal that created the collaboration.
    -- Currently only supports AWS account ID.
    collaborationCreatorAccountId :: Prelude.Text,
    -- | The display name of the collaboration creator.
    collaborationCreatorDisplayName :: Prelude.Text,
    -- | The name for the membership\'s collaboration.
    collaborationName :: Prelude.Text,
    -- | The time when the membership was created.
    createTime :: Data.POSIX,
    -- | The time the membership metadata was last updated.
    updateTime :: Data.POSIX,
    -- | The status of the membership. Valid values are \`ACTIVE\`, \`REMOVED\`,
    -- and \`COLLABORATION_DELETED\`.
    status :: MembershipStatus,
    -- | The abilities granted to the collaboration member.
    memberAbilities :: [MemberAbility]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MembershipSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'membershipSummary_id' - The unique ID for the membership\'s collaboration.
--
-- 'arn', 'membershipSummary_arn' - The unique ARN for the membership.
--
-- 'collaborationArn', 'membershipSummary_collaborationArn' - The unique ARN for the membership\'s associated collaboration.
--
-- 'collaborationId', 'membershipSummary_collaborationId' - The unique ID for the membership\'s collaboration.
--
-- 'collaborationCreatorAccountId', 'membershipSummary_collaborationCreatorAccountId' - The identifier of the AWS principal that created the collaboration.
-- Currently only supports AWS account ID.
--
-- 'collaborationCreatorDisplayName', 'membershipSummary_collaborationCreatorDisplayName' - The display name of the collaboration creator.
--
-- 'collaborationName', 'membershipSummary_collaborationName' - The name for the membership\'s collaboration.
--
-- 'createTime', 'membershipSummary_createTime' - The time when the membership was created.
--
-- 'updateTime', 'membershipSummary_updateTime' - The time the membership metadata was last updated.
--
-- 'status', 'membershipSummary_status' - The status of the membership. Valid values are \`ACTIVE\`, \`REMOVED\`,
-- and \`COLLABORATION_DELETED\`.
--
-- 'memberAbilities', 'membershipSummary_memberAbilities' - The abilities granted to the collaboration member.
newMembershipSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'collaborationArn'
  Prelude.Text ->
  -- | 'collaborationId'
  Prelude.Text ->
  -- | 'collaborationCreatorAccountId'
  Prelude.Text ->
  -- | 'collaborationCreatorDisplayName'
  Prelude.Text ->
  -- | 'collaborationName'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'status'
  MembershipStatus ->
  MembershipSummary
newMembershipSummary
  pId_
  pArn_
  pCollaborationArn_
  pCollaborationId_
  pCollaborationCreatorAccountId_
  pCollaborationCreatorDisplayName_
  pCollaborationName_
  pCreateTime_
  pUpdateTime_
  pStatus_ =
    MembershipSummary'
      { id = pId_,
        arn = pArn_,
        collaborationArn = pCollaborationArn_,
        collaborationId = pCollaborationId_,
        collaborationCreatorAccountId =
          pCollaborationCreatorAccountId_,
        collaborationCreatorDisplayName =
          pCollaborationCreatorDisplayName_,
        collaborationName = pCollaborationName_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        status = pStatus_,
        memberAbilities = Prelude.mempty
      }

-- | The unique ID for the membership\'s collaboration.
membershipSummary_id :: Lens.Lens' MembershipSummary Prelude.Text
membershipSummary_id = Lens.lens (\MembershipSummary' {id} -> id) (\s@MembershipSummary' {} a -> s {id = a} :: MembershipSummary)

-- | The unique ARN for the membership.
membershipSummary_arn :: Lens.Lens' MembershipSummary Prelude.Text
membershipSummary_arn = Lens.lens (\MembershipSummary' {arn} -> arn) (\s@MembershipSummary' {} a -> s {arn = a} :: MembershipSummary)

-- | The unique ARN for the membership\'s associated collaboration.
membershipSummary_collaborationArn :: Lens.Lens' MembershipSummary Prelude.Text
membershipSummary_collaborationArn = Lens.lens (\MembershipSummary' {collaborationArn} -> collaborationArn) (\s@MembershipSummary' {} a -> s {collaborationArn = a} :: MembershipSummary)

-- | The unique ID for the membership\'s collaboration.
membershipSummary_collaborationId :: Lens.Lens' MembershipSummary Prelude.Text
membershipSummary_collaborationId = Lens.lens (\MembershipSummary' {collaborationId} -> collaborationId) (\s@MembershipSummary' {} a -> s {collaborationId = a} :: MembershipSummary)

-- | The identifier of the AWS principal that created the collaboration.
-- Currently only supports AWS account ID.
membershipSummary_collaborationCreatorAccountId :: Lens.Lens' MembershipSummary Prelude.Text
membershipSummary_collaborationCreatorAccountId = Lens.lens (\MembershipSummary' {collaborationCreatorAccountId} -> collaborationCreatorAccountId) (\s@MembershipSummary' {} a -> s {collaborationCreatorAccountId = a} :: MembershipSummary)

-- | The display name of the collaboration creator.
membershipSummary_collaborationCreatorDisplayName :: Lens.Lens' MembershipSummary Prelude.Text
membershipSummary_collaborationCreatorDisplayName = Lens.lens (\MembershipSummary' {collaborationCreatorDisplayName} -> collaborationCreatorDisplayName) (\s@MembershipSummary' {} a -> s {collaborationCreatorDisplayName = a} :: MembershipSummary)

-- | The name for the membership\'s collaboration.
membershipSummary_collaborationName :: Lens.Lens' MembershipSummary Prelude.Text
membershipSummary_collaborationName = Lens.lens (\MembershipSummary' {collaborationName} -> collaborationName) (\s@MembershipSummary' {} a -> s {collaborationName = a} :: MembershipSummary)

-- | The time when the membership was created.
membershipSummary_createTime :: Lens.Lens' MembershipSummary Prelude.UTCTime
membershipSummary_createTime = Lens.lens (\MembershipSummary' {createTime} -> createTime) (\s@MembershipSummary' {} a -> s {createTime = a} :: MembershipSummary) Prelude.. Data._Time

-- | The time the membership metadata was last updated.
membershipSummary_updateTime :: Lens.Lens' MembershipSummary Prelude.UTCTime
membershipSummary_updateTime = Lens.lens (\MembershipSummary' {updateTime} -> updateTime) (\s@MembershipSummary' {} a -> s {updateTime = a} :: MembershipSummary) Prelude.. Data._Time

-- | The status of the membership. Valid values are \`ACTIVE\`, \`REMOVED\`,
-- and \`COLLABORATION_DELETED\`.
membershipSummary_status :: Lens.Lens' MembershipSummary MembershipStatus
membershipSummary_status = Lens.lens (\MembershipSummary' {status} -> status) (\s@MembershipSummary' {} a -> s {status = a} :: MembershipSummary)

-- | The abilities granted to the collaboration member.
membershipSummary_memberAbilities :: Lens.Lens' MembershipSummary [MemberAbility]
membershipSummary_memberAbilities = Lens.lens (\MembershipSummary' {memberAbilities} -> memberAbilities) (\s@MembershipSummary' {} a -> s {memberAbilities = a} :: MembershipSummary) Prelude.. Lens.coerced

instance Data.FromJSON MembershipSummary where
  parseJSON =
    Data.withObject
      "MembershipSummary"
      ( \x ->
          MembershipSummary'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "collaborationArn")
            Prelude.<*> (x Data..: "collaborationId")
            Prelude.<*> (x Data..: "collaborationCreatorAccountId")
            Prelude.<*> (x Data..: "collaborationCreatorDisplayName")
            Prelude.<*> (x Data..: "collaborationName")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> ( x
                            Data..:? "memberAbilities"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MembershipSummary where
  hashWithSalt _salt MembershipSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` collaborationArn
      `Prelude.hashWithSalt` collaborationId
      `Prelude.hashWithSalt` collaborationCreatorAccountId
      `Prelude.hashWithSalt` collaborationCreatorDisplayName
      `Prelude.hashWithSalt` collaborationName
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` memberAbilities

instance Prelude.NFData MembershipSummary where
  rnf MembershipSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf collaborationArn
      `Prelude.seq` Prelude.rnf collaborationId
      `Prelude.seq` Prelude.rnf collaborationCreatorAccountId
      `Prelude.seq` Prelude.rnf collaborationCreatorDisplayName
      `Prelude.seq` Prelude.rnf collaborationName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf memberAbilities
