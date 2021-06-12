{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreatePlayerSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves open slots in a game session for a group of players. Before
-- players can be added, a game session must have an @ACTIVE@ status, have
-- a creation policy of @ALLOW_ALL@, and have an open player slot. To add a
-- single player to a game session, use CreatePlayerSession. When a player
-- connects to the game server and references a player session ID, the game
-- server contacts the Amazon GameLift service to validate the player
-- reservation and accept the player.
--
-- To create player sessions, specify a game session ID, a list of player
-- IDs, and optionally a set of player data strings. If successful, a slot
-- is reserved in the game session for each player and a set of new
-- PlayerSession objects is returned. Player sessions cannot be updated.
--
-- /Available in Amazon GameLift Local./
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
module Network.AWS.GameLift.CreatePlayerSessions
  ( -- * Creating a Request
    CreatePlayerSessions (..),
    newCreatePlayerSessions,

    -- * Request Lenses
    createPlayerSessions_playerDataMap,
    createPlayerSessions_gameSessionId,
    createPlayerSessions_playerIds,

    -- * Destructuring the Response
    CreatePlayerSessionsResponse (..),
    newCreatePlayerSessionsResponse,

    -- * Response Lenses
    createPlayerSessionsResponse_playerSessions,
    createPlayerSessionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreatePlayerSessions' smart constructor.
data CreatePlayerSessions = CreatePlayerSessions'
  { -- | Map of string pairs, each specifying a player ID and a set of
    -- developer-defined information related to the player. Amazon GameLift
    -- does not use this data, so it can be formatted as needed for use in the
    -- game. Player data strings for player IDs not included in the @PlayerIds@
    -- parameter are ignored.
    playerDataMap :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A unique identifier for the game session to add players to.
    gameSessionId :: Core.Text,
    -- | List of unique identifiers for the players to be added.
    playerIds :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePlayerSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerDataMap', 'createPlayerSessions_playerDataMap' - Map of string pairs, each specifying a player ID and a set of
-- developer-defined information related to the player. Amazon GameLift
-- does not use this data, so it can be formatted as needed for use in the
-- game. Player data strings for player IDs not included in the @PlayerIds@
-- parameter are ignored.
--
-- 'gameSessionId', 'createPlayerSessions_gameSessionId' - A unique identifier for the game session to add players to.
--
-- 'playerIds', 'createPlayerSessions_playerIds' - List of unique identifiers for the players to be added.
newCreatePlayerSessions ::
  -- | 'gameSessionId'
  Core.Text ->
  -- | 'playerIds'
  Core.NonEmpty Core.Text ->
  CreatePlayerSessions
newCreatePlayerSessions pGameSessionId_ pPlayerIds_ =
  CreatePlayerSessions'
    { playerDataMap = Core.Nothing,
      gameSessionId = pGameSessionId_,
      playerIds = Lens._Coerce Lens.# pPlayerIds_
    }

-- | Map of string pairs, each specifying a player ID and a set of
-- developer-defined information related to the player. Amazon GameLift
-- does not use this data, so it can be formatted as needed for use in the
-- game. Player data strings for player IDs not included in the @PlayerIds@
-- parameter are ignored.
createPlayerSessions_playerDataMap :: Lens.Lens' CreatePlayerSessions (Core.Maybe (Core.HashMap Core.Text Core.Text))
createPlayerSessions_playerDataMap = Lens.lens (\CreatePlayerSessions' {playerDataMap} -> playerDataMap) (\s@CreatePlayerSessions' {} a -> s {playerDataMap = a} :: CreatePlayerSessions) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for the game session to add players to.
createPlayerSessions_gameSessionId :: Lens.Lens' CreatePlayerSessions Core.Text
createPlayerSessions_gameSessionId = Lens.lens (\CreatePlayerSessions' {gameSessionId} -> gameSessionId) (\s@CreatePlayerSessions' {} a -> s {gameSessionId = a} :: CreatePlayerSessions)

-- | List of unique identifiers for the players to be added.
createPlayerSessions_playerIds :: Lens.Lens' CreatePlayerSessions (Core.NonEmpty Core.Text)
createPlayerSessions_playerIds = Lens.lens (\CreatePlayerSessions' {playerIds} -> playerIds) (\s@CreatePlayerSessions' {} a -> s {playerIds = a} :: CreatePlayerSessions) Core.. Lens._Coerce

instance Core.AWSRequest CreatePlayerSessions where
  type
    AWSResponse CreatePlayerSessions =
      CreatePlayerSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlayerSessionsResponse'
            Core.<$> (x Core..?> "PlayerSessions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePlayerSessions

instance Core.NFData CreatePlayerSessions

instance Core.ToHeaders CreatePlayerSessions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.CreatePlayerSessions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePlayerSessions where
  toJSON CreatePlayerSessions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PlayerDataMap" Core..=) Core.<$> playerDataMap,
            Core.Just ("GameSessionId" Core..= gameSessionId),
            Core.Just ("PlayerIds" Core..= playerIds)
          ]
      )

instance Core.ToPath CreatePlayerSessions where
  toPath = Core.const "/"

instance Core.ToQuery CreatePlayerSessions where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreatePlayerSessionsResponse' smart constructor.
data CreatePlayerSessionsResponse = CreatePlayerSessionsResponse'
  { -- | A collection of player session objects created for the added players.
    playerSessions :: Core.Maybe [PlayerSession],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePlayerSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerSessions', 'createPlayerSessionsResponse_playerSessions' - A collection of player session objects created for the added players.
--
-- 'httpStatus', 'createPlayerSessionsResponse_httpStatus' - The response's http status code.
newCreatePlayerSessionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePlayerSessionsResponse
newCreatePlayerSessionsResponse pHttpStatus_ =
  CreatePlayerSessionsResponse'
    { playerSessions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of player session objects created for the added players.
createPlayerSessionsResponse_playerSessions :: Lens.Lens' CreatePlayerSessionsResponse (Core.Maybe [PlayerSession])
createPlayerSessionsResponse_playerSessions = Lens.lens (\CreatePlayerSessionsResponse' {playerSessions} -> playerSessions) (\s@CreatePlayerSessionsResponse' {} a -> s {playerSessions = a} :: CreatePlayerSessionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createPlayerSessionsResponse_httpStatus :: Lens.Lens' CreatePlayerSessionsResponse Core.Int
createPlayerSessionsResponse_httpStatus = Lens.lens (\CreatePlayerSessionsResponse' {httpStatus} -> httpStatus) (\s@CreatePlayerSessionsResponse' {} a -> s {httpStatus = a} :: CreatePlayerSessionsResponse)

instance Core.NFData CreatePlayerSessionsResponse
