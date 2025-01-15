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
-- Module      : Amazonka.IdentityStore.Types.GroupMembershipExistenceResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.GroupMembershipExistenceResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types.MemberId
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether a resource is a member of a group in the identity
-- store.
--
-- /See:/ 'newGroupMembershipExistenceResult' smart constructor.
data GroupMembershipExistenceResult = GroupMembershipExistenceResult'
  { -- | The identifier for a group in the identity store.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the identifier of a group member. Setting the
    -- @UserID@ field to the specific identifier for a user indicates that the
    -- user is a member of the group.
    memberId :: Prelude.Maybe MemberId,
    -- | Indicates whether a membership relation exists or not.
    membershipExists :: Prelude.Maybe (Data.Sensitive Prelude.Bool)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupMembershipExistenceResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'groupMembershipExistenceResult_groupId' - The identifier for a group in the identity store.
--
-- 'memberId', 'groupMembershipExistenceResult_memberId' - An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
--
-- 'membershipExists', 'groupMembershipExistenceResult_membershipExists' - Indicates whether a membership relation exists or not.
newGroupMembershipExistenceResult ::
  GroupMembershipExistenceResult
newGroupMembershipExistenceResult =
  GroupMembershipExistenceResult'
    { groupId =
        Prelude.Nothing,
      memberId = Prelude.Nothing,
      membershipExists = Prelude.Nothing
    }

-- | The identifier for a group in the identity store.
groupMembershipExistenceResult_groupId :: Lens.Lens' GroupMembershipExistenceResult (Prelude.Maybe Prelude.Text)
groupMembershipExistenceResult_groupId = Lens.lens (\GroupMembershipExistenceResult' {groupId} -> groupId) (\s@GroupMembershipExistenceResult' {} a -> s {groupId = a} :: GroupMembershipExistenceResult)

-- | An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
groupMembershipExistenceResult_memberId :: Lens.Lens' GroupMembershipExistenceResult (Prelude.Maybe MemberId)
groupMembershipExistenceResult_memberId = Lens.lens (\GroupMembershipExistenceResult' {memberId} -> memberId) (\s@GroupMembershipExistenceResult' {} a -> s {memberId = a} :: GroupMembershipExistenceResult)

-- | Indicates whether a membership relation exists or not.
groupMembershipExistenceResult_membershipExists :: Lens.Lens' GroupMembershipExistenceResult (Prelude.Maybe Prelude.Bool)
groupMembershipExistenceResult_membershipExists = Lens.lens (\GroupMembershipExistenceResult' {membershipExists} -> membershipExists) (\s@GroupMembershipExistenceResult' {} a -> s {membershipExists = a} :: GroupMembershipExistenceResult) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON GroupMembershipExistenceResult where
  parseJSON =
    Data.withObject
      "GroupMembershipExistenceResult"
      ( \x ->
          GroupMembershipExistenceResult'
            Prelude.<$> (x Data..:? "GroupId")
            Prelude.<*> (x Data..:? "MemberId")
            Prelude.<*> (x Data..:? "MembershipExists")
      )

instance
  Prelude.Hashable
    GroupMembershipExistenceResult
  where
  hashWithSalt
    _salt
    GroupMembershipExistenceResult' {..} =
      _salt
        `Prelude.hashWithSalt` groupId
        `Prelude.hashWithSalt` memberId
        `Prelude.hashWithSalt` membershipExists

instance
  Prelude.NFData
    GroupMembershipExistenceResult
  where
  rnf GroupMembershipExistenceResult' {..} =
    Prelude.rnf groupId `Prelude.seq`
      Prelude.rnf memberId `Prelude.seq`
        Prelude.rnf membershipExists
