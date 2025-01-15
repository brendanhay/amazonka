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
-- Module      : Amazonka.GameLift.CreatePlayerSessions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves open slots in a game session for a group of players. New player
-- sessions can be created in any game session with an open slot that is in
-- @ACTIVE@ status and has a player creation policy of @ACCEPT_ALL@. To add
-- a single player to a game session, use
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_CreatePlayerSession.html CreatePlayerSession>
--
-- To create player sessions, specify a game session ID and a list of
-- player IDs. Optionally, provide a set of player data for each player ID.
--
-- If successful, a slot is reserved in the game session for each player,
-- and new @PlayerSession@ objects are returned with player session IDs.
-- Each player references their player session ID when sending a connection
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
module Amazonka.GameLift.CreatePlayerSessions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePlayerSessions' smart constructor.
data CreatePlayerSessions = CreatePlayerSessions'
  { -- | Map of string pairs, each specifying a player ID and a set of
    -- developer-defined information related to the player. Amazon GameLift
    -- does not use this data, so it can be formatted as needed for use in the
    -- game. Any player data strings for player IDs that are not included in
    -- the @PlayerIds@ parameter are ignored.
    playerDataMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier for the game session to add players to.
    gameSessionId :: Prelude.Text,
    -- | List of unique identifiers for the players to be added.
    playerIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- game. Any player data strings for player IDs that are not included in
-- the @PlayerIds@ parameter are ignored.
--
-- 'gameSessionId', 'createPlayerSessions_gameSessionId' - A unique identifier for the game session to add players to.
--
-- 'playerIds', 'createPlayerSessions_playerIds' - List of unique identifiers for the players to be added.
newCreatePlayerSessions ::
  -- | 'gameSessionId'
  Prelude.Text ->
  -- | 'playerIds'
  Prelude.NonEmpty Prelude.Text ->
  CreatePlayerSessions
newCreatePlayerSessions pGameSessionId_ pPlayerIds_ =
  CreatePlayerSessions'
    { playerDataMap =
        Prelude.Nothing,
      gameSessionId = pGameSessionId_,
      playerIds = Lens.coerced Lens.# pPlayerIds_
    }

-- | Map of string pairs, each specifying a player ID and a set of
-- developer-defined information related to the player. Amazon GameLift
-- does not use this data, so it can be formatted as needed for use in the
-- game. Any player data strings for player IDs that are not included in
-- the @PlayerIds@ parameter are ignored.
createPlayerSessions_playerDataMap :: Lens.Lens' CreatePlayerSessions (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPlayerSessions_playerDataMap = Lens.lens (\CreatePlayerSessions' {playerDataMap} -> playerDataMap) (\s@CreatePlayerSessions' {} a -> s {playerDataMap = a} :: CreatePlayerSessions) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the game session to add players to.
createPlayerSessions_gameSessionId :: Lens.Lens' CreatePlayerSessions Prelude.Text
createPlayerSessions_gameSessionId = Lens.lens (\CreatePlayerSessions' {gameSessionId} -> gameSessionId) (\s@CreatePlayerSessions' {} a -> s {gameSessionId = a} :: CreatePlayerSessions)

-- | List of unique identifiers for the players to be added.
createPlayerSessions_playerIds :: Lens.Lens' CreatePlayerSessions (Prelude.NonEmpty Prelude.Text)
createPlayerSessions_playerIds = Lens.lens (\CreatePlayerSessions' {playerIds} -> playerIds) (\s@CreatePlayerSessions' {} a -> s {playerIds = a} :: CreatePlayerSessions) Prelude.. Lens.coerced

instance Core.AWSRequest CreatePlayerSessions where
  type
    AWSResponse CreatePlayerSessions =
      CreatePlayerSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlayerSessionsResponse'
            Prelude.<$> (x Data..?> "PlayerSessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlayerSessions where
  hashWithSalt _salt CreatePlayerSessions' {..} =
    _salt
      `Prelude.hashWithSalt` playerDataMap
      `Prelude.hashWithSalt` gameSessionId
      `Prelude.hashWithSalt` playerIds

instance Prelude.NFData CreatePlayerSessions where
  rnf CreatePlayerSessions' {..} =
    Prelude.rnf playerDataMap `Prelude.seq`
      Prelude.rnf gameSessionId `Prelude.seq`
        Prelude.rnf playerIds

instance Data.ToHeaders CreatePlayerSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.CreatePlayerSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePlayerSessions where
  toJSON CreatePlayerSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PlayerDataMap" Data..=) Prelude.<$> playerDataMap,
            Prelude.Just ("GameSessionId" Data..= gameSessionId),
            Prelude.Just ("PlayerIds" Data..= playerIds)
          ]
      )

instance Data.ToPath CreatePlayerSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePlayerSessions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePlayerSessionsResponse' smart constructor.
data CreatePlayerSessionsResponse = CreatePlayerSessionsResponse'
  { -- | A collection of player session objects created for the added players.
    playerSessions :: Prelude.Maybe [PlayerSession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreatePlayerSessionsResponse
newCreatePlayerSessionsResponse pHttpStatus_ =
  CreatePlayerSessionsResponse'
    { playerSessions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of player session objects created for the added players.
createPlayerSessionsResponse_playerSessions :: Lens.Lens' CreatePlayerSessionsResponse (Prelude.Maybe [PlayerSession])
createPlayerSessionsResponse_playerSessions = Lens.lens (\CreatePlayerSessionsResponse' {playerSessions} -> playerSessions) (\s@CreatePlayerSessionsResponse' {} a -> s {playerSessions = a} :: CreatePlayerSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createPlayerSessionsResponse_httpStatus :: Lens.Lens' CreatePlayerSessionsResponse Prelude.Int
createPlayerSessionsResponse_httpStatus = Lens.lens (\CreatePlayerSessionsResponse' {httpStatus} -> httpStatus) (\s@CreatePlayerSessionsResponse' {} a -> s {httpStatus = a} :: CreatePlayerSessionsResponse)

instance Prelude.NFData CreatePlayerSessionsResponse where
  rnf CreatePlayerSessionsResponse' {..} =
    Prelude.rnf playerSessions `Prelude.seq`
      Prelude.rnf httpStatus
