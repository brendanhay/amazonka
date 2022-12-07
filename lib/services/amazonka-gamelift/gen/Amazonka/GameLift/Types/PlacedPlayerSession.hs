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
-- Module      : Amazonka.GameLift.Types.PlacedPlayerSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.PlacedPlayerSession where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a player session that was created as part of a
-- StartGameSessionPlacement request. This object contains only the player
-- ID and player session ID. To retrieve full details on a player session,
-- call DescribePlayerSessions with the player session ID.
--
-- __Related actions__
--
-- CreatePlayerSession | CreatePlayerSessions | DescribePlayerSessions |
-- StartGameSessionPlacement | DescribeGameSessionPlacement |
-- StopGameSessionPlacement |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newPlacedPlayerSession' smart constructor.
data PlacedPlayerSession = PlacedPlayerSession'
  { -- | A unique identifier for a player session.
    playerSessionId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player that is associated with this player
    -- session.
    playerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlacedPlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerSessionId', 'placedPlayerSession_playerSessionId' - A unique identifier for a player session.
--
-- 'playerId', 'placedPlayerSession_playerId' - A unique identifier for a player that is associated with this player
-- session.
newPlacedPlayerSession ::
  PlacedPlayerSession
newPlacedPlayerSession =
  PlacedPlayerSession'
    { playerSessionId =
        Prelude.Nothing,
      playerId = Prelude.Nothing
    }

-- | A unique identifier for a player session.
placedPlayerSession_playerSessionId :: Lens.Lens' PlacedPlayerSession (Prelude.Maybe Prelude.Text)
placedPlayerSession_playerSessionId = Lens.lens (\PlacedPlayerSession' {playerSessionId} -> playerSessionId) (\s@PlacedPlayerSession' {} a -> s {playerSessionId = a} :: PlacedPlayerSession)

-- | A unique identifier for a player that is associated with this player
-- session.
placedPlayerSession_playerId :: Lens.Lens' PlacedPlayerSession (Prelude.Maybe Prelude.Text)
placedPlayerSession_playerId = Lens.lens (\PlacedPlayerSession' {playerId} -> playerId) (\s@PlacedPlayerSession' {} a -> s {playerId = a} :: PlacedPlayerSession)

instance Data.FromJSON PlacedPlayerSession where
  parseJSON =
    Data.withObject
      "PlacedPlayerSession"
      ( \x ->
          PlacedPlayerSession'
            Prelude.<$> (x Data..:? "PlayerSessionId")
            Prelude.<*> (x Data..:? "PlayerId")
      )

instance Prelude.Hashable PlacedPlayerSession where
  hashWithSalt _salt PlacedPlayerSession' {..} =
    _salt `Prelude.hashWithSalt` playerSessionId
      `Prelude.hashWithSalt` playerId

instance Prelude.NFData PlacedPlayerSession where
  rnf PlacedPlayerSession' {..} =
    Prelude.rnf playerSessionId
      `Prelude.seq` Prelude.rnf playerId
