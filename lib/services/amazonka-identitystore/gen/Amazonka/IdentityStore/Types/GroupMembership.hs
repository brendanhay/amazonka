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
-- Module      : Amazonka.IdentityStore.Types.GroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.GroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types.MemberId
import qualified Amazonka.Prelude as Prelude

-- | Contains the identifiers for a group, a group member, and a
-- @GroupMembership@ object in the identity store.
--
-- /See:/ 'newGroupMembership' smart constructor.
data GroupMembership = GroupMembership'
  { -- | The identifier for a group in the identity store.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the identifier of a group member. Setting the
    -- @UserID@ field to the specific identifier for a user indicates that the
    -- user is a member of the group.
    memberId :: Prelude.Maybe MemberId,
    -- | The identifier for a @GroupMembership@ object in an identity store.
    membershipId :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'groupMembership_groupId' - The identifier for a group in the identity store.
--
-- 'memberId', 'groupMembership_memberId' - An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
--
-- 'membershipId', 'groupMembership_membershipId' - The identifier for a @GroupMembership@ object in an identity store.
--
-- 'identityStoreId', 'groupMembership_identityStoreId' - The globally unique identifier for the identity store.
newGroupMembership ::
  -- | 'identityStoreId'
  Prelude.Text ->
  GroupMembership
newGroupMembership pIdentityStoreId_ =
  GroupMembership'
    { groupId = Prelude.Nothing,
      memberId = Prelude.Nothing,
      membershipId = Prelude.Nothing,
      identityStoreId = pIdentityStoreId_
    }

-- | The identifier for a group in the identity store.
groupMembership_groupId :: Lens.Lens' GroupMembership (Prelude.Maybe Prelude.Text)
groupMembership_groupId = Lens.lens (\GroupMembership' {groupId} -> groupId) (\s@GroupMembership' {} a -> s {groupId = a} :: GroupMembership)

-- | An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
groupMembership_memberId :: Lens.Lens' GroupMembership (Prelude.Maybe MemberId)
groupMembership_memberId = Lens.lens (\GroupMembership' {memberId} -> memberId) (\s@GroupMembership' {} a -> s {memberId = a} :: GroupMembership)

-- | The identifier for a @GroupMembership@ object in an identity store.
groupMembership_membershipId :: Lens.Lens' GroupMembership (Prelude.Maybe Prelude.Text)
groupMembership_membershipId = Lens.lens (\GroupMembership' {membershipId} -> membershipId) (\s@GroupMembership' {} a -> s {membershipId = a} :: GroupMembership)

-- | The globally unique identifier for the identity store.
groupMembership_identityStoreId :: Lens.Lens' GroupMembership Prelude.Text
groupMembership_identityStoreId = Lens.lens (\GroupMembership' {identityStoreId} -> identityStoreId) (\s@GroupMembership' {} a -> s {identityStoreId = a} :: GroupMembership)

instance Data.FromJSON GroupMembership where
  parseJSON =
    Data.withObject
      "GroupMembership"
      ( \x ->
          GroupMembership'
            Prelude.<$> (x Data..:? "GroupId")
            Prelude.<*> (x Data..:? "MemberId")
            Prelude.<*> (x Data..:? "MembershipId")
            Prelude.<*> (x Data..: "IdentityStoreId")
      )

instance Prelude.Hashable GroupMembership where
  hashWithSalt _salt GroupMembership' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` membershipId
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData GroupMembership where
  rnf GroupMembership' {..} =
    Prelude.rnf groupId `Prelude.seq`
      Prelude.rnf memberId `Prelude.seq`
        Prelude.rnf membershipId `Prelude.seq`
          Prelude.rnf identityStoreId
