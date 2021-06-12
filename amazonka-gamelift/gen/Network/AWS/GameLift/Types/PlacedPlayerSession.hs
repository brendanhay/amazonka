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
-- Module      : Network.AWS.GameLift.Types.PlacedPlayerSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlacedPlayerSession where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a player session that was created as part of a
-- StartGameSessionPlacement request. This object contains only the player
-- ID and player session ID. To retrieve full details on a player session,
-- call DescribePlayerSessions with the player session ID.
--
-- -   CreatePlayerSession
--
-- -   CreatePlayerSessions
--
-- -   DescribePlayerSessions
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
--
-- /See:/ 'newPlacedPlayerSession' smart constructor.
data PlacedPlayerSession = PlacedPlayerSession'
  { -- | A unique identifier for a player that is associated with this player
    -- session.
    playerId :: Core.Maybe Core.Text,
    -- | A unique identifier for a player session.
    playerSessionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PlacedPlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerId', 'placedPlayerSession_playerId' - A unique identifier for a player that is associated with this player
-- session.
--
-- 'playerSessionId', 'placedPlayerSession_playerSessionId' - A unique identifier for a player session.
newPlacedPlayerSession ::
  PlacedPlayerSession
newPlacedPlayerSession =
  PlacedPlayerSession'
    { playerId = Core.Nothing,
      playerSessionId = Core.Nothing
    }

-- | A unique identifier for a player that is associated with this player
-- session.
placedPlayerSession_playerId :: Lens.Lens' PlacedPlayerSession (Core.Maybe Core.Text)
placedPlayerSession_playerId = Lens.lens (\PlacedPlayerSession' {playerId} -> playerId) (\s@PlacedPlayerSession' {} a -> s {playerId = a} :: PlacedPlayerSession)

-- | A unique identifier for a player session.
placedPlayerSession_playerSessionId :: Lens.Lens' PlacedPlayerSession (Core.Maybe Core.Text)
placedPlayerSession_playerSessionId = Lens.lens (\PlacedPlayerSession' {playerSessionId} -> playerSessionId) (\s@PlacedPlayerSession' {} a -> s {playerSessionId = a} :: PlacedPlayerSession)

instance Core.FromJSON PlacedPlayerSession where
  parseJSON =
    Core.withObject
      "PlacedPlayerSession"
      ( \x ->
          PlacedPlayerSession'
            Core.<$> (x Core..:? "PlayerId")
            Core.<*> (x Core..:? "PlayerSessionId")
      )

instance Core.Hashable PlacedPlayerSession

instance Core.NFData PlacedPlayerSession
