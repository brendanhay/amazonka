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
-- Module      : Amazonka.CleanRooms.Types.Membership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.Membership where

import Amazonka.CleanRooms.Types.MemberAbility
import Amazonka.CleanRooms.Types.MembershipQueryLogStatus
import Amazonka.CleanRooms.Types.MembershipStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The membership object.
--
-- /See:/ 'newMembership' smart constructor.
data Membership = Membership'
  { -- | The unique ID of the membership.
    id :: Prelude.Text,
    -- | The unique ARN for the membership.
    arn :: Prelude.Text,
    -- | The unique ARN for the membership\'s associated collaboration.
    collaborationArn :: Prelude.Text,
    -- | The unique ID for the membership\'s collaboration.
    collaborationId :: Prelude.Text,
    -- | The identifier used to reference members of the collaboration. Currently
    -- only supports AWS account ID.
    collaborationCreatorAccountId :: Prelude.Text,
    -- | The display name of the collaboration creator.
    collaborationCreatorDisplayName :: Prelude.Text,
    -- | The name of the membership\'s collaboration.
    collaborationName :: Prelude.Text,
    -- | The time when the membership was created.
    createTime :: Data.POSIX,
    -- | The time the membership metadata was last updated.
    updateTime :: Data.POSIX,
    -- | The status of the membership. Valid values are \`ACTIVE\`, \`REMOVED\`,
    -- and \`COLLABORATION_DELETED\`.
    status :: MembershipStatus,
    -- | The abilities granted to the collaboration member.
    memberAbilities :: [MemberAbility],
    -- | An indicator as to whether query logging has been enabled or disabled
    -- for the collaboration.
    queryLogStatus :: MembershipQueryLogStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Membership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'membership_id' - The unique ID of the membership.
--
-- 'arn', 'membership_arn' - The unique ARN for the membership.
--
-- 'collaborationArn', 'membership_collaborationArn' - The unique ARN for the membership\'s associated collaboration.
--
-- 'collaborationId', 'membership_collaborationId' - The unique ID for the membership\'s collaboration.
--
-- 'collaborationCreatorAccountId', 'membership_collaborationCreatorAccountId' - The identifier used to reference members of the collaboration. Currently
-- only supports AWS account ID.
--
-- 'collaborationCreatorDisplayName', 'membership_collaborationCreatorDisplayName' - The display name of the collaboration creator.
--
-- 'collaborationName', 'membership_collaborationName' - The name of the membership\'s collaboration.
--
-- 'createTime', 'membership_createTime' - The time when the membership was created.
--
-- 'updateTime', 'membership_updateTime' - The time the membership metadata was last updated.
--
-- 'status', 'membership_status' - The status of the membership. Valid values are \`ACTIVE\`, \`REMOVED\`,
-- and \`COLLABORATION_DELETED\`.
--
-- 'memberAbilities', 'membership_memberAbilities' - The abilities granted to the collaboration member.
--
-- 'queryLogStatus', 'membership_queryLogStatus' - An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
newMembership ::
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
  -- | 'queryLogStatus'
  MembershipQueryLogStatus ->
  Membership
newMembership
  pId_
  pArn_
  pCollaborationArn_
  pCollaborationId_
  pCollaborationCreatorAccountId_
  pCollaborationCreatorDisplayName_
  pCollaborationName_
  pCreateTime_
  pUpdateTime_
  pStatus_
  pQueryLogStatus_ =
    Membership'
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
        memberAbilities = Prelude.mempty,
        queryLogStatus = pQueryLogStatus_
      }

-- | The unique ID of the membership.
membership_id :: Lens.Lens' Membership Prelude.Text
membership_id = Lens.lens (\Membership' {id} -> id) (\s@Membership' {} a -> s {id = a} :: Membership)

-- | The unique ARN for the membership.
membership_arn :: Lens.Lens' Membership Prelude.Text
membership_arn = Lens.lens (\Membership' {arn} -> arn) (\s@Membership' {} a -> s {arn = a} :: Membership)

-- | The unique ARN for the membership\'s associated collaboration.
membership_collaborationArn :: Lens.Lens' Membership Prelude.Text
membership_collaborationArn = Lens.lens (\Membership' {collaborationArn} -> collaborationArn) (\s@Membership' {} a -> s {collaborationArn = a} :: Membership)

-- | The unique ID for the membership\'s collaboration.
membership_collaborationId :: Lens.Lens' Membership Prelude.Text
membership_collaborationId = Lens.lens (\Membership' {collaborationId} -> collaborationId) (\s@Membership' {} a -> s {collaborationId = a} :: Membership)

-- | The identifier used to reference members of the collaboration. Currently
-- only supports AWS account ID.
membership_collaborationCreatorAccountId :: Lens.Lens' Membership Prelude.Text
membership_collaborationCreatorAccountId = Lens.lens (\Membership' {collaborationCreatorAccountId} -> collaborationCreatorAccountId) (\s@Membership' {} a -> s {collaborationCreatorAccountId = a} :: Membership)

-- | The display name of the collaboration creator.
membership_collaborationCreatorDisplayName :: Lens.Lens' Membership Prelude.Text
membership_collaborationCreatorDisplayName = Lens.lens (\Membership' {collaborationCreatorDisplayName} -> collaborationCreatorDisplayName) (\s@Membership' {} a -> s {collaborationCreatorDisplayName = a} :: Membership)

-- | The name of the membership\'s collaboration.
membership_collaborationName :: Lens.Lens' Membership Prelude.Text
membership_collaborationName = Lens.lens (\Membership' {collaborationName} -> collaborationName) (\s@Membership' {} a -> s {collaborationName = a} :: Membership)

-- | The time when the membership was created.
membership_createTime :: Lens.Lens' Membership Prelude.UTCTime
membership_createTime = Lens.lens (\Membership' {createTime} -> createTime) (\s@Membership' {} a -> s {createTime = a} :: Membership) Prelude.. Data._Time

-- | The time the membership metadata was last updated.
membership_updateTime :: Lens.Lens' Membership Prelude.UTCTime
membership_updateTime = Lens.lens (\Membership' {updateTime} -> updateTime) (\s@Membership' {} a -> s {updateTime = a} :: Membership) Prelude.. Data._Time

-- | The status of the membership. Valid values are \`ACTIVE\`, \`REMOVED\`,
-- and \`COLLABORATION_DELETED\`.
membership_status :: Lens.Lens' Membership MembershipStatus
membership_status = Lens.lens (\Membership' {status} -> status) (\s@Membership' {} a -> s {status = a} :: Membership)

-- | The abilities granted to the collaboration member.
membership_memberAbilities :: Lens.Lens' Membership [MemberAbility]
membership_memberAbilities = Lens.lens (\Membership' {memberAbilities} -> memberAbilities) (\s@Membership' {} a -> s {memberAbilities = a} :: Membership) Prelude.. Lens.coerced

-- | An indicator as to whether query logging has been enabled or disabled
-- for the collaboration.
membership_queryLogStatus :: Lens.Lens' Membership MembershipQueryLogStatus
membership_queryLogStatus = Lens.lens (\Membership' {queryLogStatus} -> queryLogStatus) (\s@Membership' {} a -> s {queryLogStatus = a} :: Membership)

instance Data.FromJSON Membership where
  parseJSON =
    Data.withObject
      "Membership"
      ( \x ->
          Membership'
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
            Prelude.<*> (x Data..: "queryLogStatus")
      )

instance Prelude.Hashable Membership where
  hashWithSalt _salt Membership' {..} =
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
      `Prelude.hashWithSalt` queryLogStatus

instance Prelude.NFData Membership where
  rnf Membership' {..} =
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
      `Prelude.seq` Prelude.rnf queryLogStatus
