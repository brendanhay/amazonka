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
-- Module      : Network.AWS.GameLift.CreatePlayerSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves an open player slot in an active game session. Before a player
-- can be added, a game session must have an @ACTIVE@ status, have a
-- creation policy of @ALLOW_ALL@, and have an open player slot. To add a
-- group of players to a game session, use CreatePlayerSessions. When the
-- player connects to the game server and references a player session ID,
-- the game server contacts the Amazon GameLift service to validate the
-- player reservation and accept the player.
--
-- To create a player session, specify a game session ID, player ID, and
-- optionally a string of player data. If successful, a slot is reserved in
-- the game session for the player and a new PlayerSession object is
-- returned. Player sessions cannot be updated.
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
module Network.AWS.GameLift.CreatePlayerSession
  ( -- * Creating a Request
    CreatePlayerSession (..),
    newCreatePlayerSession,

    -- * Request Lenses
    createPlayerSession_playerData,
    createPlayerSession_gameSessionId,
    createPlayerSession_playerId,

    -- * Destructuring the Response
    CreatePlayerSessionResponse (..),
    newCreatePlayerSessionResponse,

    -- * Response Lenses
    createPlayerSessionResponse_playerSession,
    createPlayerSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreatePlayerSession' smart constructor.
data CreatePlayerSession = CreatePlayerSession'
  { -- | Developer-defined information related to a player. Amazon GameLift does
    -- not use this data, so it can be formatted as needed for use in the game.
    playerData :: Core.Maybe Core.Text,
    -- | A unique identifier for the game session to add a player to.
    gameSessionId :: Core.Text,
    -- | A unique identifier for a player. Player IDs are developer-defined.
    playerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerData', 'createPlayerSession_playerData' - Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
--
-- 'gameSessionId', 'createPlayerSession_gameSessionId' - A unique identifier for the game session to add a player to.
--
-- 'playerId', 'createPlayerSession_playerId' - A unique identifier for a player. Player IDs are developer-defined.
newCreatePlayerSession ::
  -- | 'gameSessionId'
  Core.Text ->
  -- | 'playerId'
  Core.Text ->
  CreatePlayerSession
newCreatePlayerSession pGameSessionId_ pPlayerId_ =
  CreatePlayerSession'
    { playerData = Core.Nothing,
      gameSessionId = pGameSessionId_,
      playerId = pPlayerId_
    }

-- | Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
createPlayerSession_playerData :: Lens.Lens' CreatePlayerSession (Core.Maybe Core.Text)
createPlayerSession_playerData = Lens.lens (\CreatePlayerSession' {playerData} -> playerData) (\s@CreatePlayerSession' {} a -> s {playerData = a} :: CreatePlayerSession)

-- | A unique identifier for the game session to add a player to.
createPlayerSession_gameSessionId :: Lens.Lens' CreatePlayerSession Core.Text
createPlayerSession_gameSessionId = Lens.lens (\CreatePlayerSession' {gameSessionId} -> gameSessionId) (\s@CreatePlayerSession' {} a -> s {gameSessionId = a} :: CreatePlayerSession)

-- | A unique identifier for a player. Player IDs are developer-defined.
createPlayerSession_playerId :: Lens.Lens' CreatePlayerSession Core.Text
createPlayerSession_playerId = Lens.lens (\CreatePlayerSession' {playerId} -> playerId) (\s@CreatePlayerSession' {} a -> s {playerId = a} :: CreatePlayerSession)

instance Core.AWSRequest CreatePlayerSession where
  type
    AWSResponse CreatePlayerSession =
      CreatePlayerSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlayerSessionResponse'
            Core.<$> (x Core..?> "PlayerSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePlayerSession

instance Core.NFData CreatePlayerSession

instance Core.ToHeaders CreatePlayerSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.CreatePlayerSession" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePlayerSession where
  toJSON CreatePlayerSession' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PlayerData" Core..=) Core.<$> playerData,
            Core.Just ("GameSessionId" Core..= gameSessionId),
            Core.Just ("PlayerId" Core..= playerId)
          ]
      )

instance Core.ToPath CreatePlayerSession where
  toPath = Core.const "/"

instance Core.ToQuery CreatePlayerSession where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreatePlayerSessionResponse' smart constructor.
data CreatePlayerSessionResponse = CreatePlayerSessionResponse'
  { -- | Object that describes the newly created player session record.
    playerSession :: Core.Maybe PlayerSession,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePlayerSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerSession', 'createPlayerSessionResponse_playerSession' - Object that describes the newly created player session record.
--
-- 'httpStatus', 'createPlayerSessionResponse_httpStatus' - The response's http status code.
newCreatePlayerSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePlayerSessionResponse
newCreatePlayerSessionResponse pHttpStatus_ =
  CreatePlayerSessionResponse'
    { playerSession =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly created player session record.
createPlayerSessionResponse_playerSession :: Lens.Lens' CreatePlayerSessionResponse (Core.Maybe PlayerSession)
createPlayerSessionResponse_playerSession = Lens.lens (\CreatePlayerSessionResponse' {playerSession} -> playerSession) (\s@CreatePlayerSessionResponse' {} a -> s {playerSession = a} :: CreatePlayerSessionResponse)

-- | The response's http status code.
createPlayerSessionResponse_httpStatus :: Lens.Lens' CreatePlayerSessionResponse Core.Int
createPlayerSessionResponse_httpStatus = Lens.lens (\CreatePlayerSessionResponse' {httpStatus} -> httpStatus) (\s@CreatePlayerSessionResponse' {} a -> s {httpStatus = a} :: CreatePlayerSessionResponse)

instance Core.NFData CreatePlayerSessionResponse
