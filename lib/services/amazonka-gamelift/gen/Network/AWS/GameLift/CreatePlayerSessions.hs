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
-- Reserves open slots in a game session for a group of players. New player
-- sessions can be created in any game session with an open slot that is in
-- @ACTIVE@ status and has a player creation policy of @ACCEPT_ALL@. To add
-- a single player to a game session, use CreatePlayerSession.
--
-- To create player sessions, specify a game session ID and a list of
-- player IDs. Optionally, provide a set of player data for each player ID.
--
-- If successful, a slot is reserved in the game session for each player,
-- and new PlayerSession objects are returned with player session IDs. Each
-- player references their player session ID when sending a connection
-- request to the game session, and the game server can use it to validate
-- the player reservation with the GameLift service. Player sessions cannot
-- be updated.
--
-- /Available in Amazon GameLift Local./
--
-- __Related actions__
--
-- CreatePlayerSession | CreatePlayerSessions | DescribePlayerSessions |
-- StartGameSessionPlacement | DescribeGameSessionPlacement |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreatePlayerSessions' smart constructor.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePlayerSessionsResponse'
            Prelude.<$> (x Core..?> "PlayerSessions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePlayerSessions

instance Prelude.NFData CreatePlayerSessions

instance Core.ToHeaders CreatePlayerSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.CreatePlayerSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePlayerSessions where
  toJSON CreatePlayerSessions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PlayerDataMap" Core..=) Prelude.<$> playerDataMap,
            Prelude.Just ("GameSessionId" Core..= gameSessionId),
            Prelude.Just ("PlayerIds" Core..= playerIds)
          ]
      )

instance Core.ToPath CreatePlayerSessions where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePlayerSessions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreatePlayerSessionsResponse' smart constructor.
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

instance Prelude.NFData CreatePlayerSessionsResponse
