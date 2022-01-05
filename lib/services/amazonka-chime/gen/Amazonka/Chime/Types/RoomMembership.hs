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
-- Module      : Amazonka.Chime.Types.RoomMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.RoomMembership where

import Amazonka.Chime.Types.Member
import Amazonka.Chime.Types.RoomMembershipRole
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The room membership details.
--
-- /See:/ 'newRoomMembership' smart constructor.
data RoomMembership = RoomMembership'
  { -- | The room membership update timestamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The membership role.
    role' :: Prelude.Maybe RoomMembershipRole,
    -- | The room ID.
    roomId :: Prelude.Maybe Prelude.Text,
    member :: Prelude.Maybe Member,
    -- | The identifier of the user that invited the room member.
    invitedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoomMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updatedTimestamp', 'roomMembership_updatedTimestamp' - The room membership update timestamp, in ISO 8601 format.
--
-- 'role'', 'roomMembership_role' - The membership role.
--
-- 'roomId', 'roomMembership_roomId' - The room ID.
--
-- 'member', 'roomMembership_member' - Undocumented member.
--
-- 'invitedBy', 'roomMembership_invitedBy' - The identifier of the user that invited the room member.
newRoomMembership ::
  RoomMembership
newRoomMembership =
  RoomMembership'
    { updatedTimestamp = Prelude.Nothing,
      role' = Prelude.Nothing,
      roomId = Prelude.Nothing,
      member = Prelude.Nothing,
      invitedBy = Prelude.Nothing
    }

-- | The room membership update timestamp, in ISO 8601 format.
roomMembership_updatedTimestamp :: Lens.Lens' RoomMembership (Prelude.Maybe Prelude.UTCTime)
roomMembership_updatedTimestamp = Lens.lens (\RoomMembership' {updatedTimestamp} -> updatedTimestamp) (\s@RoomMembership' {} a -> s {updatedTimestamp = a} :: RoomMembership) Prelude.. Lens.mapping Core._Time

-- | The membership role.
roomMembership_role :: Lens.Lens' RoomMembership (Prelude.Maybe RoomMembershipRole)
roomMembership_role = Lens.lens (\RoomMembership' {role'} -> role') (\s@RoomMembership' {} a -> s {role' = a} :: RoomMembership)

-- | The room ID.
roomMembership_roomId :: Lens.Lens' RoomMembership (Prelude.Maybe Prelude.Text)
roomMembership_roomId = Lens.lens (\RoomMembership' {roomId} -> roomId) (\s@RoomMembership' {} a -> s {roomId = a} :: RoomMembership)

-- | Undocumented member.
roomMembership_member :: Lens.Lens' RoomMembership (Prelude.Maybe Member)
roomMembership_member = Lens.lens (\RoomMembership' {member} -> member) (\s@RoomMembership' {} a -> s {member = a} :: RoomMembership)

-- | The identifier of the user that invited the room member.
roomMembership_invitedBy :: Lens.Lens' RoomMembership (Prelude.Maybe Prelude.Text)
roomMembership_invitedBy = Lens.lens (\RoomMembership' {invitedBy} -> invitedBy) (\s@RoomMembership' {} a -> s {invitedBy = a} :: RoomMembership)

instance Core.FromJSON RoomMembership where
  parseJSON =
    Core.withObject
      "RoomMembership"
      ( \x ->
          RoomMembership'
            Prelude.<$> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "Role")
            Prelude.<*> (x Core..:? "RoomId")
            Prelude.<*> (x Core..:? "Member")
            Prelude.<*> (x Core..:? "InvitedBy")
      )

instance Prelude.Hashable RoomMembership where
  hashWithSalt _salt RoomMembership' {..} =
    _salt `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` roomId
      `Prelude.hashWithSalt` member
      `Prelude.hashWithSalt` invitedBy

instance Prelude.NFData RoomMembership where
  rnf RoomMembership' {..} =
    Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf roomId
      `Prelude.seq` Prelude.rnf member
      `Prelude.seq` Prelude.rnf invitedBy
