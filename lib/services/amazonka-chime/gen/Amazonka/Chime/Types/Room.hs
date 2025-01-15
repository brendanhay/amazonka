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
-- Module      : Amazonka.Chime.Types.Room
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Room where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Chime chat room details.
--
-- /See:/ 'newRoom' smart constructor.
data Room = Room'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the room creator.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The room creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The room name.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The room ID.
    roomId :: Prelude.Maybe Prelude.Text,
    -- | The room update timestamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Room' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'room_accountId' - The Amazon Chime account ID.
--
-- 'createdBy', 'room_createdBy' - The identifier of the room creator.
--
-- 'createdTimestamp', 'room_createdTimestamp' - The room creation timestamp, in ISO 8601 format.
--
-- 'name', 'room_name' - The room name.
--
-- 'roomId', 'room_roomId' - The room ID.
--
-- 'updatedTimestamp', 'room_updatedTimestamp' - The room update timestamp, in ISO 8601 format.
newRoom ::
  Room
newRoom =
  Room'
    { accountId = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      roomId = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The Amazon Chime account ID.
room_accountId :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_accountId = Lens.lens (\Room' {accountId} -> accountId) (\s@Room' {} a -> s {accountId = a} :: Room)

-- | The identifier of the room creator.
room_createdBy :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_createdBy = Lens.lens (\Room' {createdBy} -> createdBy) (\s@Room' {} a -> s {createdBy = a} :: Room)

-- | The room creation timestamp, in ISO 8601 format.
room_createdTimestamp :: Lens.Lens' Room (Prelude.Maybe Prelude.UTCTime)
room_createdTimestamp = Lens.lens (\Room' {createdTimestamp} -> createdTimestamp) (\s@Room' {} a -> s {createdTimestamp = a} :: Room) Prelude.. Lens.mapping Data._Time

-- | The room name.
room_name :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_name = Lens.lens (\Room' {name} -> name) (\s@Room' {} a -> s {name = a} :: Room) Prelude.. Lens.mapping Data._Sensitive

-- | The room ID.
room_roomId :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_roomId = Lens.lens (\Room' {roomId} -> roomId) (\s@Room' {} a -> s {roomId = a} :: Room)

-- | The room update timestamp, in ISO 8601 format.
room_updatedTimestamp :: Lens.Lens' Room (Prelude.Maybe Prelude.UTCTime)
room_updatedTimestamp = Lens.lens (\Room' {updatedTimestamp} -> updatedTimestamp) (\s@Room' {} a -> s {updatedTimestamp = a} :: Room) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Room where
  parseJSON =
    Data.withObject
      "Room"
      ( \x ->
          Room'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RoomId")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable Room where
  hashWithSalt _salt Room' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roomId
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData Room where
  rnf Room' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf createdBy `Prelude.seq`
        Prelude.rnf createdTimestamp `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf roomId `Prelude.seq`
              Prelude.rnf updatedTimestamp
