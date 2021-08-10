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
-- Module      : Network.AWS.GameLift.CreateGameSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multiplayer game session for players. This operation creates a
-- game session record and assigns an available server process in the
-- specified fleet to host the game session. A fleet must have an @ACTIVE@
-- status before a game session can be created in it.
--
-- To create a game session, specify either fleet ID or alias ID and
-- indicate a maximum number of players to allow in the game session. You
-- can also provide a name and game-specific properties for this game
-- session. If successful, a GameSession object is returned containing the
-- game session properties and other settings you specified.
--
-- __Idempotency tokens.__ You can add a token that uniquely identifies
-- game session requests. This is useful for ensuring that game session
-- requests are idempotent. Multiple requests with the same idempotency
-- token are processed only once; subsequent requests return the original
-- result. All response values are the same with the exception of game
-- session status, which may change.
--
-- __Resource creation limits.__ If you are creating a game session on a
-- fleet with a resource creation limit policy in force, then you must
-- specify a creator ID. Without this ID, Amazon GameLift has no way to
-- evaluate the policy for this new game session request.
--
-- __Player acceptance policy.__ By default, newly created game sessions
-- are open to new players. You can restrict new player access by using
-- UpdateGameSession to change the game session\'s player session creation
-- policy.
--
-- __Game session logs.__ Logs are retained for all active game sessions
-- for 14 days. To access the logs, call GetGameSessionLogUrl to download
-- the log files.
--
-- /Available in Amazon GameLift Local./
--
-- -   CreateGameSession
--
-- -   DescribeGameSessions
--
-- -   DescribeGameSessionDetails
--
-- -   SearchGameSessions
--
-- -   UpdateGameSession
--
-- -   GetGameSessionLogUrl
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
module Network.AWS.GameLift.CreateGameSession
  ( -- * Creating a Request
    CreateGameSession (..),
    newCreateGameSession,

    -- * Request Lenses
    createGameSession_gameProperties,
    createGameSession_idempotencyToken,
    createGameSession_creatorId,
    createGameSession_fleetId,
    createGameSession_gameSessionData,
    createGameSession_gameSessionId,
    createGameSession_name,
    createGameSession_aliasId,
    createGameSession_maximumPlayerSessionCount,

    -- * Destructuring the Response
    CreateGameSessionResponse (..),
    newCreateGameSessionResponse,

    -- * Response Lenses
    createGameSessionResponse_gameSession,
    createGameSessionResponse_httpStatus,
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
-- /See:/ 'newCreateGameSession' smart constructor.
data CreateGameSession = CreateGameSession'
  { -- | Set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | Custom string that uniquely identifies a request for a new game session.
    -- Maximum token length is 48 characters. If provided, this string is
    -- included in the new game session\'s ID. (A game session ARN has the
    -- following format:
    -- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.)
    -- Idempotency tokens remain in use for 30 days after a game session has
    -- ended; game session objects are retained for this time period and then
    -- deleted.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player or entity creating the game session.
    -- This ID is used to enforce a resource protection policy (if one exists)
    -- that limits the number of concurrent active game sessions one player can
    -- have.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a fleet to create a game session in. You can use
    -- either the fleet ID or ARN value. Each request must reference either a
    -- fleet ID or alias ID, but not both.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | /This parameter is no longer preferred. Please use @IdempotencyToken@
    -- instead./ Custom string that uniquely identifies a request for a new
    -- game session. Maximum token length is 48 characters. If provided, this
    -- string is included in the new game session\'s ID. (A game session ARN
    -- has the following format:
    -- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.)
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an alias associated with the fleet to create a
    -- game session in. You can use either the alias ID or ARN value. Each
    -- request must reference either a fleet ID or alias ID, but not both.
    aliasId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGameSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameProperties', 'createGameSession_gameProperties' - Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'idempotencyToken', 'createGameSession_idempotencyToken' - Custom string that uniquely identifies a request for a new game session.
-- Maximum token length is 48 characters. If provided, this string is
-- included in the new game session\'s ID. (A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.)
-- Idempotency tokens remain in use for 30 days after a game session has
-- ended; game session objects are retained for this time period and then
-- deleted.
--
-- 'creatorId', 'createGameSession_creatorId' - A unique identifier for a player or entity creating the game session.
-- This ID is used to enforce a resource protection policy (if one exists)
-- that limits the number of concurrent active game sessions one player can
-- have.
--
-- 'fleetId', 'createGameSession_fleetId' - A unique identifier for a fleet to create a game session in. You can use
-- either the fleet ID or ARN value. Each request must reference either a
-- fleet ID or alias ID, but not both.
--
-- 'gameSessionData', 'createGameSession_gameSessionData' - Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'gameSessionId', 'createGameSession_gameSessionId' - /This parameter is no longer preferred. Please use @IdempotencyToken@
-- instead./ Custom string that uniquely identifies a request for a new
-- game session. Maximum token length is 48 characters. If provided, this
-- string is included in the new game session\'s ID. (A game session ARN
-- has the following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.)
--
-- 'name', 'createGameSession_name' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'aliasId', 'createGameSession_aliasId' - A unique identifier for an alias associated with the fleet to create a
-- game session in. You can use either the alias ID or ARN value. Each
-- request must reference either a fleet ID or alias ID, but not both.
--
-- 'maximumPlayerSessionCount', 'createGameSession_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
newCreateGameSession ::
  -- | 'maximumPlayerSessionCount'
  Prelude.Natural ->
  CreateGameSession
newCreateGameSession pMaximumPlayerSessionCount_ =
  CreateGameSession'
    { gameProperties =
        Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      creatorId = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      name = Prelude.Nothing,
      aliasId = Prelude.Nothing,
      maximumPlayerSessionCount =
        pMaximumPlayerSessionCount_
    }

-- | Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
createGameSession_gameProperties :: Lens.Lens' CreateGameSession (Prelude.Maybe [GameProperty])
createGameSession_gameProperties = Lens.lens (\CreateGameSession' {gameProperties} -> gameProperties) (\s@CreateGameSession' {} a -> s {gameProperties = a} :: CreateGameSession) Prelude.. Lens.mapping Lens._Coerce

-- | Custom string that uniquely identifies a request for a new game session.
-- Maximum token length is 48 characters. If provided, this string is
-- included in the new game session\'s ID. (A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.)
-- Idempotency tokens remain in use for 30 days after a game session has
-- ended; game session objects are retained for this time period and then
-- deleted.
createGameSession_idempotencyToken :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_idempotencyToken = Lens.lens (\CreateGameSession' {idempotencyToken} -> idempotencyToken) (\s@CreateGameSession' {} a -> s {idempotencyToken = a} :: CreateGameSession)

-- | A unique identifier for a player or entity creating the game session.
-- This ID is used to enforce a resource protection policy (if one exists)
-- that limits the number of concurrent active game sessions one player can
-- have.
createGameSession_creatorId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_creatorId = Lens.lens (\CreateGameSession' {creatorId} -> creatorId) (\s@CreateGameSession' {} a -> s {creatorId = a} :: CreateGameSession)

-- | A unique identifier for a fleet to create a game session in. You can use
-- either the fleet ID or ARN value. Each request must reference either a
-- fleet ID or alias ID, but not both.
createGameSession_fleetId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_fleetId = Lens.lens (\CreateGameSession' {fleetId} -> fleetId) (\s@CreateGameSession' {} a -> s {fleetId = a} :: CreateGameSession)

-- | Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
createGameSession_gameSessionData :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_gameSessionData = Lens.lens (\CreateGameSession' {gameSessionData} -> gameSessionData) (\s@CreateGameSession' {} a -> s {gameSessionData = a} :: CreateGameSession)

-- | /This parameter is no longer preferred. Please use @IdempotencyToken@
-- instead./ Custom string that uniquely identifies a request for a new
-- game session. Maximum token length is 48 characters. If provided, this
-- string is included in the new game session\'s ID. (A game session ARN
-- has the following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.)
createGameSession_gameSessionId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_gameSessionId = Lens.lens (\CreateGameSession' {gameSessionId} -> gameSessionId) (\s@CreateGameSession' {} a -> s {gameSessionId = a} :: CreateGameSession)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
createGameSession_name :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_name = Lens.lens (\CreateGameSession' {name} -> name) (\s@CreateGameSession' {} a -> s {name = a} :: CreateGameSession)

-- | A unique identifier for an alias associated with the fleet to create a
-- game session in. You can use either the alias ID or ARN value. Each
-- request must reference either a fleet ID or alias ID, but not both.
createGameSession_aliasId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_aliasId = Lens.lens (\CreateGameSession' {aliasId} -> aliasId) (\s@CreateGameSession' {} a -> s {aliasId = a} :: CreateGameSession)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
createGameSession_maximumPlayerSessionCount :: Lens.Lens' CreateGameSession Prelude.Natural
createGameSession_maximumPlayerSessionCount = Lens.lens (\CreateGameSession' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@CreateGameSession' {} a -> s {maximumPlayerSessionCount = a} :: CreateGameSession)

instance Core.AWSRequest CreateGameSession where
  type
    AWSResponse CreateGameSession =
      CreateGameSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGameSessionResponse'
            Prelude.<$> (x Core..?> "GameSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGameSession

instance Prelude.NFData CreateGameSession

instance Core.ToHeaders CreateGameSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.CreateGameSession" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateGameSession where
  toJSON CreateGameSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GameProperties" Core..=)
              Prelude.<$> gameProperties,
            ("IdempotencyToken" Core..=)
              Prelude.<$> idempotencyToken,
            ("CreatorId" Core..=) Prelude.<$> creatorId,
            ("FleetId" Core..=) Prelude.<$> fleetId,
            ("GameSessionData" Core..=)
              Prelude.<$> gameSessionData,
            ("GameSessionId" Core..=) Prelude.<$> gameSessionId,
            ("Name" Core..=) Prelude.<$> name,
            ("AliasId" Core..=) Prelude.<$> aliasId,
            Prelude.Just
              ( "MaximumPlayerSessionCount"
                  Core..= maximumPlayerSessionCount
              )
          ]
      )

instance Core.ToPath CreateGameSession where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateGameSession where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateGameSessionResponse' smart constructor.
data CreateGameSessionResponse = CreateGameSessionResponse'
  { -- | Object that describes the newly created game session record.
    gameSession :: Prelude.Maybe GameSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGameSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSession', 'createGameSessionResponse_gameSession' - Object that describes the newly created game session record.
--
-- 'httpStatus', 'createGameSessionResponse_httpStatus' - The response's http status code.
newCreateGameSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGameSessionResponse
newCreateGameSessionResponse pHttpStatus_ =
  CreateGameSessionResponse'
    { gameSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly created game session record.
createGameSessionResponse_gameSession :: Lens.Lens' CreateGameSessionResponse (Prelude.Maybe GameSession)
createGameSessionResponse_gameSession = Lens.lens (\CreateGameSessionResponse' {gameSession} -> gameSession) (\s@CreateGameSessionResponse' {} a -> s {gameSession = a} :: CreateGameSessionResponse)

-- | The response's http status code.
createGameSessionResponse_httpStatus :: Lens.Lens' CreateGameSessionResponse Prelude.Int
createGameSessionResponse_httpStatus = Lens.lens (\CreateGameSessionResponse' {httpStatus} -> httpStatus) (\s@CreateGameSessionResponse' {} a -> s {httpStatus = a} :: CreateGameSessionResponse)

instance Prelude.NFData CreateGameSessionResponse
