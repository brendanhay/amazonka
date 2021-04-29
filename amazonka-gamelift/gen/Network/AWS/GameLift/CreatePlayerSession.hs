{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreatePlayerSession' smart constructor.
data CreatePlayerSession = CreatePlayerSession'
  { -- | Developer-defined information related to a player. Amazon GameLift does
    -- not use this data, so it can be formatted as needed for use in the game.
    playerData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session to add a player to.
    gameSessionId :: Prelude.Text,
    -- | A unique identifier for a player. Player IDs are developer-defined.
    playerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'playerId'
  Prelude.Text ->
  CreatePlayerSession
newCreatePlayerSession pGameSessionId_ pPlayerId_ =
  CreatePlayerSession'
    { playerData = Prelude.Nothing,
      gameSessionId = pGameSessionId_,
      playerId = pPlayerId_
    }

-- | Developer-defined information related to a player. Amazon GameLift does
-- not use this data, so it can be formatted as needed for use in the game.
createPlayerSession_playerData :: Lens.Lens' CreatePlayerSession (Prelude.Maybe Prelude.Text)
createPlayerSession_playerData = Lens.lens (\CreatePlayerSession' {playerData} -> playerData) (\s@CreatePlayerSession' {} a -> s {playerData = a} :: CreatePlayerSession)

-- | A unique identifier for the game session to add a player to.
createPlayerSession_gameSessionId :: Lens.Lens' CreatePlayerSession Prelude.Text
createPlayerSession_gameSessionId = Lens.lens (\CreatePlayerSession' {gameSessionId} -> gameSessionId) (\s@CreatePlayerSession' {} a -> s {gameSessionId = a} :: CreatePlayerSession)

-- | A unique identifier for a player. Player IDs are developer-defined.
createPlayerSession_playerId :: Lens.Lens' CreatePlayerSession Prelude.Text
createPlayerSession_playerId = Lens.lens (\CreatePlayerSession' {playerId} -> playerId) (\s@CreatePlayerSession' {} a -> s {playerId = a} :: CreatePlayerSession)

instance Prelude.AWSRequest CreatePlayerSession where
  type
    Rs CreatePlayerSession =
      CreatePlayerSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlayerSessionResponse'
            Prelude.<$> (x Prelude..?> "PlayerSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlayerSession

instance Prelude.NFData CreatePlayerSession

instance Prelude.ToHeaders CreatePlayerSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.CreatePlayerSession" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePlayerSession where
  toJSON CreatePlayerSession' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PlayerData" Prelude..=) Prelude.<$> playerData,
            Prelude.Just
              ("GameSessionId" Prelude..= gameSessionId),
            Prelude.Just ("PlayerId" Prelude..= playerId)
          ]
      )

instance Prelude.ToPath CreatePlayerSession where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePlayerSession where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreatePlayerSessionResponse' smart constructor.
data CreatePlayerSessionResponse = CreatePlayerSessionResponse'
  { -- | Object that describes the newly created player session record.
    playerSession :: Prelude.Maybe PlayerSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreatePlayerSessionResponse
newCreatePlayerSessionResponse pHttpStatus_ =
  CreatePlayerSessionResponse'
    { playerSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly created player session record.
createPlayerSessionResponse_playerSession :: Lens.Lens' CreatePlayerSessionResponse (Prelude.Maybe PlayerSession)
createPlayerSessionResponse_playerSession = Lens.lens (\CreatePlayerSessionResponse' {playerSession} -> playerSession) (\s@CreatePlayerSessionResponse' {} a -> s {playerSession = a} :: CreatePlayerSessionResponse)

-- | The response's http status code.
createPlayerSessionResponse_httpStatus :: Lens.Lens' CreatePlayerSessionResponse Prelude.Int
createPlayerSessionResponse_httpStatus = Lens.lens (\CreatePlayerSessionResponse' {httpStatus} -> httpStatus) (\s@CreatePlayerSessionResponse' {} a -> s {httpStatus = a} :: CreatePlayerSessionResponse)

instance Prelude.NFData CreatePlayerSessionResponse
