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
-- Module      : Amazonka.GameLift.CreateGameSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multiplayer game session for players in a specific fleet
-- location. This operation prompts an available server process to start a
-- game session and retrieves connection information for the new game
-- session. As an alternative, consider using the GameLift game session
-- placement feature with
--
-- with StartGameSessionPlacement, which uses FleetIQ algorithms and queues
-- to optimize the placement process.
--
-- When creating a game session, you specify exactly where you want to
-- place it and provide a set of game session configuration settings. The
-- fleet must be in @ACTIVE@ status before a game session can be created in
-- it.
--
-- This operation can be used in the following ways:
--
-- -   To create a game session on an instance in a fleet\'s home Region,
--     provide a fleet or alias ID along with your game session
--     configuration.
--
-- -   To create a game session on an instance in a fleet\'s remote
--     location, provide a fleet or alias ID and a location name, along
--     with your game session configuration.
--
-- If successful, a workflow is initiated to start a new game session. A
-- @GameSession@ object is returned containing the game session
-- configuration and status. When the status is @ACTIVE@, game session
-- connection information is provided and player sessions can be created
-- for the game session. By default, newly created game sessions are open
-- to new players. You can restrict new player access by using
-- UpdateGameSession to change the game session\'s player session creation
-- policy.
--
-- Game session logs are retained for all active game sessions for 14 days.
-- To access the logs, call GetGameSessionLogUrl to download the log files.
--
-- /Available in Amazon GameLift Local./
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a game session>
--
-- __Related actions__
--
-- CreateGameSession | DescribeGameSessions | DescribeGameSessionDetails |
-- SearchGameSessions | UpdateGameSession | GetGameSessionLogUrl |
-- StartGameSessionPlacement | DescribeGameSessionPlacement |
-- StopGameSessionPlacement |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.CreateGameSession
  ( -- * Creating a Request
    CreateGameSession (..),
    newCreateGameSession,

    -- * Request Lenses
    createGameSession_gameSessionId,
    createGameSession_fleetId,
    createGameSession_creatorId,
    createGameSession_name,
    createGameSession_gameSessionData,
    createGameSession_aliasId,
    createGameSession_idempotencyToken,
    createGameSession_location,
    createGameSession_gameProperties,
    createGameSession_maximumPlayerSessionCount,

    -- * Destructuring the Response
    CreateGameSessionResponse (..),
    newCreateGameSessionResponse,

    -- * Response Lenses
    createGameSessionResponse_gameSession,
    createGameSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newCreateGameSession' smart constructor.
data CreateGameSession = CreateGameSession'
  { -- | /This parameter is no longer preferred. Please use @IdempotencyToken@
    -- instead./ Custom string that uniquely identifies a request for a new
    -- game session. Maximum token length is 48 characters. If provided, this
    -- string is included in the new game session\'s ID.
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to create a game session in. You can
    -- use either the fleet ID or ARN value. Each request must reference either
    -- a fleet ID or alias ID, but not both.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a player or entity creating the game session.
    -- This parameter is required when requesting a new game session on a fleet
    -- with a resource creation limit policy. This type of policy limits the
    -- number of concurrent active game sessions that one player can create
    -- within a certain time span. GameLift uses the CreatorId to evaluate the
    -- new request against the policy.
    creatorId :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the alias associated with the fleet to create a
    -- game session in. You can use either the alias ID or ARN value. Each
    -- request must reference either a fleet ID or alias ID, but not both.
    aliasId :: Prelude.Maybe Prelude.Text,
    -- | Custom string that uniquely identifies the new game session request.
    -- This is useful for ensuring that game session requests with the same
    -- idempotency token are processed only once. Subsequent requests with the
    -- same string return the original @GameSession@ object, with an updated
    -- status. Maximum token length is 48 characters. If provided, this string
    -- is included in the new game session\'s ID. A game session ARN has the
    -- following format:
    -- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
    -- Idempotency tokens remain in use for 30 days after a game session has
    -- ended; game session objects are retained for this time period and then
    -- deleted.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | A fleet\'s remote location to place the new game session in. If this
    -- parameter is not set, the new game session is placed in the fleet\'s
    -- home Region. Specify a remote location with an Amazon Web Services
    -- Region code such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | A set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameProperties :: Prelude.Maybe [GameProperty],
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
-- 'gameSessionId', 'createGameSession_gameSessionId' - /This parameter is no longer preferred. Please use @IdempotencyToken@
-- instead./ Custom string that uniquely identifies a request for a new
-- game session. Maximum token length is 48 characters. If provided, this
-- string is included in the new game session\'s ID.
--
-- 'fleetId', 'createGameSession_fleetId' - A unique identifier for the fleet to create a game session in. You can
-- use either the fleet ID or ARN value. Each request must reference either
-- a fleet ID or alias ID, but not both.
--
-- 'creatorId', 'createGameSession_creatorId' - A unique identifier for a player or entity creating the game session.
-- This parameter is required when requesting a new game session on a fleet
-- with a resource creation limit policy. This type of policy limits the
-- number of concurrent active game sessions that one player can create
-- within a certain time span. GameLift uses the CreatorId to evaluate the
-- new request against the policy.
--
-- 'name', 'createGameSession_name' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'gameSessionData', 'createGameSession_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'aliasId', 'createGameSession_aliasId' - A unique identifier for the alias associated with the fleet to create a
-- game session in. You can use either the alias ID or ARN value. Each
-- request must reference either a fleet ID or alias ID, but not both.
--
-- 'idempotencyToken', 'createGameSession_idempotencyToken' - Custom string that uniquely identifies the new game session request.
-- This is useful for ensuring that game session requests with the same
-- idempotency token are processed only once. Subsequent requests with the
-- same string return the original @GameSession@ object, with an updated
-- status. Maximum token length is 48 characters. If provided, this string
-- is included in the new game session\'s ID. A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
-- Idempotency tokens remain in use for 30 days after a game session has
-- ended; game session objects are retained for this time period and then
-- deleted.
--
-- 'location', 'createGameSession_location' - A fleet\'s remote location to place the new game session in. If this
-- parameter is not set, the new game session is placed in the fleet\'s
-- home Region. Specify a remote location with an Amazon Web Services
-- Region code such as @us-west-2@.
--
-- 'gameProperties', 'createGameSession_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'maximumPlayerSessionCount', 'createGameSession_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
newCreateGameSession ::
  -- | 'maximumPlayerSessionCount'
  Prelude.Natural ->
  CreateGameSession
newCreateGameSession pMaximumPlayerSessionCount_ =
  CreateGameSession'
    { gameSessionId = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      creatorId = Prelude.Nothing,
      name = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      aliasId = Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      location = Prelude.Nothing,
      gameProperties = Prelude.Nothing,
      maximumPlayerSessionCount =
        pMaximumPlayerSessionCount_
    }

-- | /This parameter is no longer preferred. Please use @IdempotencyToken@
-- instead./ Custom string that uniquely identifies a request for a new
-- game session. Maximum token length is 48 characters. If provided, this
-- string is included in the new game session\'s ID.
createGameSession_gameSessionId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_gameSessionId = Lens.lens (\CreateGameSession' {gameSessionId} -> gameSessionId) (\s@CreateGameSession' {} a -> s {gameSessionId = a} :: CreateGameSession)

-- | A unique identifier for the fleet to create a game session in. You can
-- use either the fleet ID or ARN value. Each request must reference either
-- a fleet ID or alias ID, but not both.
createGameSession_fleetId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_fleetId = Lens.lens (\CreateGameSession' {fleetId} -> fleetId) (\s@CreateGameSession' {} a -> s {fleetId = a} :: CreateGameSession)

-- | A unique identifier for a player or entity creating the game session.
-- This parameter is required when requesting a new game session on a fleet
-- with a resource creation limit policy. This type of policy limits the
-- number of concurrent active game sessions that one player can create
-- within a certain time span. GameLift uses the CreatorId to evaluate the
-- new request against the policy.
createGameSession_creatorId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_creatorId = Lens.lens (\CreateGameSession' {creatorId} -> creatorId) (\s@CreateGameSession' {} a -> s {creatorId = a} :: CreateGameSession)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
createGameSession_name :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_name = Lens.lens (\CreateGameSession' {name} -> name) (\s@CreateGameSession' {} a -> s {name = a} :: CreateGameSession)

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
createGameSession_gameSessionData :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_gameSessionData = Lens.lens (\CreateGameSession' {gameSessionData} -> gameSessionData) (\s@CreateGameSession' {} a -> s {gameSessionData = a} :: CreateGameSession)

-- | A unique identifier for the alias associated with the fleet to create a
-- game session in. You can use either the alias ID or ARN value. Each
-- request must reference either a fleet ID or alias ID, but not both.
createGameSession_aliasId :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_aliasId = Lens.lens (\CreateGameSession' {aliasId} -> aliasId) (\s@CreateGameSession' {} a -> s {aliasId = a} :: CreateGameSession)

-- | Custom string that uniquely identifies the new game session request.
-- This is useful for ensuring that game session requests with the same
-- idempotency token are processed only once. Subsequent requests with the
-- same string return the original @GameSession@ object, with an updated
-- status. Maximum token length is 48 characters. If provided, this string
-- is included in the new game session\'s ID. A game session ARN has the
-- following format:
-- @arn:aws:gamelift:\<region>::gamesession\/\<fleet ID>\/\<custom ID string or idempotency token>@.
-- Idempotency tokens remain in use for 30 days after a game session has
-- ended; game session objects are retained for this time period and then
-- deleted.
createGameSession_idempotencyToken :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_idempotencyToken = Lens.lens (\CreateGameSession' {idempotencyToken} -> idempotencyToken) (\s@CreateGameSession' {} a -> s {idempotencyToken = a} :: CreateGameSession)

-- | A fleet\'s remote location to place the new game session in. If this
-- parameter is not set, the new game session is placed in the fleet\'s
-- home Region. Specify a remote location with an Amazon Web Services
-- Region code such as @us-west-2@.
createGameSession_location :: Lens.Lens' CreateGameSession (Prelude.Maybe Prelude.Text)
createGameSession_location = Lens.lens (\CreateGameSession' {location} -> location) (\s@CreateGameSession' {} a -> s {location = a} :: CreateGameSession)

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
createGameSession_gameProperties :: Lens.Lens' CreateGameSession (Prelude.Maybe [GameProperty])
createGameSession_gameProperties = Lens.lens (\CreateGameSession' {gameProperties} -> gameProperties) (\s@CreateGameSession' {} a -> s {gameProperties = a} :: CreateGameSession) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
createGameSession_maximumPlayerSessionCount :: Lens.Lens' CreateGameSession Prelude.Natural
createGameSession_maximumPlayerSessionCount = Lens.lens (\CreateGameSession' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@CreateGameSession' {} a -> s {maximumPlayerSessionCount = a} :: CreateGameSession)

instance Core.AWSRequest CreateGameSession where
  type
    AWSResponse CreateGameSession =
      CreateGameSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGameSessionResponse'
            Prelude.<$> (x Data..?> "GameSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGameSession where
  hashWithSalt _salt CreateGameSession' {..} =
    _salt `Prelude.hashWithSalt` gameSessionId
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` creatorId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` gameSessionData
      `Prelude.hashWithSalt` aliasId
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` gameProperties
      `Prelude.hashWithSalt` maximumPlayerSessionCount

instance Prelude.NFData CreateGameSession where
  rnf CreateGameSession' {..} =
    Prelude.rnf gameSessionId
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf creatorId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf gameSessionData
      `Prelude.seq` Prelude.rnf aliasId
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf gameProperties
      `Prelude.seq` Prelude.rnf maximumPlayerSessionCount

instance Data.ToHeaders CreateGameSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.CreateGameSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGameSession where
  toJSON CreateGameSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GameSessionId" Data..=) Prelude.<$> gameSessionId,
            ("FleetId" Data..=) Prelude.<$> fleetId,
            ("CreatorId" Data..=) Prelude.<$> creatorId,
            ("Name" Data..=) Prelude.<$> name,
            ("GameSessionData" Data..=)
              Prelude.<$> gameSessionData,
            ("AliasId" Data..=) Prelude.<$> aliasId,
            ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("Location" Data..=) Prelude.<$> location,
            ("GameProperties" Data..=)
              Prelude.<$> gameProperties,
            Prelude.Just
              ( "MaximumPlayerSessionCount"
                  Data..= maximumPlayerSessionCount
              )
          ]
      )

instance Data.ToPath CreateGameSession where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGameSession where
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

instance Prelude.NFData CreateGameSessionResponse where
  rnf CreateGameSessionResponse' {..} =
    Prelude.rnf gameSession
      `Prelude.seq` Prelude.rnf httpStatus
