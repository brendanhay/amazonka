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
-- Module      : Amazonka.GameLift.CreatePlayerSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves an open player slot in a game session for a player. New player
-- sessions can be created in any game session with an open slot that is in
-- @ACTIVE@ status and has a player creation policy of @ACCEPT_ALL@. You
-- can add a group of players to a game session with
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_CreatePlayerSessions.html CreatePlayerSessions>
-- .
--
-- To create a player session, specify a game session ID, player ID, and
-- optionally a set of player data.
--
-- If successful, a slot is reserved in the game session for the player and
-- a new @PlayerSessions@ object is returned with a player session ID. The
-- player references the player session ID when sending a connection
-- request to the game session, and the game server can use it to validate
-- the player reservation with the GameLift service. Player sessions cannot
-- be updated.
--
-- The maximum number of players per game session is 200. It is not
-- adjustable.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreatePlayerSession
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePlayerSession' smart constructor.
data CreatePlayerSession = CreatePlayerSession'
  { -- | Developer-defined information related to a player. GameLift does not use
    -- this data, so it can be formatted as needed for use in the game.
    playerData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the game session to add a player to.
    gameSessionId :: Prelude.Text,
    -- | A unique identifier for a player. Player IDs are developer-defined.
    playerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePlayerSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerData', 'createPlayerSession_playerData' - Developer-defined information related to a player. GameLift does not use
-- this data, so it can be formatted as needed for use in the game.
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

-- | Developer-defined information related to a player. GameLift does not use
-- this data, so it can be formatted as needed for use in the game.
createPlayerSession_playerData :: Lens.Lens' CreatePlayerSession (Prelude.Maybe Prelude.Text)
createPlayerSession_playerData = Lens.lens (\CreatePlayerSession' {playerData} -> playerData) (\s@CreatePlayerSession' {} a -> s {playerData = a} :: CreatePlayerSession)

-- | A unique identifier for the game session to add a player to.
createPlayerSession_gameSessionId :: Lens.Lens' CreatePlayerSession Prelude.Text
createPlayerSession_gameSessionId = Lens.lens (\CreatePlayerSession' {gameSessionId} -> gameSessionId) (\s@CreatePlayerSession' {} a -> s {gameSessionId = a} :: CreatePlayerSession)

-- | A unique identifier for a player. Player IDs are developer-defined.
createPlayerSession_playerId :: Lens.Lens' CreatePlayerSession Prelude.Text
createPlayerSession_playerId = Lens.lens (\CreatePlayerSession' {playerId} -> playerId) (\s@CreatePlayerSession' {} a -> s {playerId = a} :: CreatePlayerSession)

instance Core.AWSRequest CreatePlayerSession where
  type
    AWSResponse CreatePlayerSession =
      CreatePlayerSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlayerSessionResponse'
            Prelude.<$> (x Data..?> "PlayerSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlayerSession where
  hashWithSalt _salt CreatePlayerSession' {..} =
    _salt `Prelude.hashWithSalt` playerData
      `Prelude.hashWithSalt` gameSessionId
      `Prelude.hashWithSalt` playerId

instance Prelude.NFData CreatePlayerSession where
  rnf CreatePlayerSession' {..} =
    Prelude.rnf playerData
      `Prelude.seq` Prelude.rnf gameSessionId
      `Prelude.seq` Prelude.rnf playerId

instance Data.ToHeaders CreatePlayerSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.CreatePlayerSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePlayerSession where
  toJSON CreatePlayerSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlayerData" Data..=) Prelude.<$> playerData,
            Prelude.Just ("GameSessionId" Data..= gameSessionId),
            Prelude.Just ("PlayerId" Data..= playerId)
          ]
      )

instance Data.ToPath CreatePlayerSession where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePlayerSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePlayerSessionResponse' smart constructor.
data CreatePlayerSessionResponse = CreatePlayerSessionResponse'
  { -- | Object that describes the newly created player session record.
    playerSession :: Prelude.Maybe PlayerSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreatePlayerSessionResponse where
  rnf CreatePlayerSessionResponse' {..} =
    Prelude.rnf playerSession
      `Prelude.seq` Prelude.rnf httpStatus
