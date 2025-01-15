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
-- Module      : Amazonka.GameLift.StartGameSessionPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places a request for a new game session in a queue. When processing a
-- placement request, Amazon GameLift searches for available resources on
-- the queue\'s destinations, scanning each until it finds resources or the
-- placement request times out.
--
-- A game session placement request can also request player sessions. When
-- a new game session is successfully created, Amazon GameLift creates a
-- player session for each player included in the request.
--
-- When placing a game session, by default Amazon GameLift tries each fleet
-- in the order they are listed in the queue configuration. Ideally, a
-- queue\'s destinations are listed in preference order.
--
-- Alternatively, when requesting a game session with players, you can also
-- provide latency data for each player in relevant Regions. Latency data
-- indicates the performance lag a player experiences when connected to a
-- fleet in the Region. Amazon GameLift uses latency data to reorder the
-- list of destinations to place the game session in a Region with minimal
-- lag. If latency data is provided for multiple players, Amazon GameLift
-- calculates each Region\'s average lag for all players and reorders to
-- get the best game play across all players.
--
-- To place a new game session request, specify the following:
--
-- -   The queue name and a set of game session properties and settings
--
-- -   A unique ID (such as a UUID) for the placement. You use this ID to
--     track the status of the placement request
--
-- -   (Optional) A set of player data and a unique player ID for each
--     player that you are joining to the new game session (player data is
--     optional, but if you include it, you must also provide a unique ID
--     for each player)
--
-- -   Latency data for all players (if you want to optimize game play for
--     the players)
--
-- If successful, a new game session placement is created.
--
-- To track the status of a placement request, call
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeGameSessionPlacement.html DescribeGameSessionPlacement>
-- and check the request\'s status. If the status is @FULFILLED@, a new
-- game session has been created and a game session ARN and Region are
-- referenced. If the placement request times out, you can resubmit the
-- request or retry it with a different queue.
module Amazonka.GameLift.StartGameSessionPlacement
  ( -- * Creating a Request
    StartGameSessionPlacement (..),
    newStartGameSessionPlacement,

    -- * Request Lenses
    startGameSessionPlacement_desiredPlayerSessions,
    startGameSessionPlacement_gameProperties,
    startGameSessionPlacement_gameSessionData,
    startGameSessionPlacement_gameSessionName,
    startGameSessionPlacement_playerLatencies,
    startGameSessionPlacement_placementId,
    startGameSessionPlacement_gameSessionQueueName,
    startGameSessionPlacement_maximumPlayerSessionCount,

    -- * Destructuring the Response
    StartGameSessionPlacementResponse (..),
    newStartGameSessionPlacementResponse,

    -- * Response Lenses
    startGameSessionPlacementResponse_gameSessionPlacement,
    startGameSessionPlacementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartGameSessionPlacement' smart constructor.
data StartGameSessionPlacement = StartGameSessionPlacement'
  { -- | Set of information on each player to create a player session for.
    desiredPlayerSessions :: Prelude.Maybe [DesiredPlayerSession],
    -- | A set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process with a
    -- request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the @GameSession@
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    gameSessionName :: Prelude.Maybe Prelude.Text,
    -- | A set of values, expressed in milliseconds, that indicates the amount of
    -- latency that a player experiences when connected to Amazon Web Services
    -- Regions. This information is used to try to place the new game session
    -- where it can offer the best possible gameplay experience for the
    -- players.
    playerLatencies :: Prelude.Maybe [PlayerLatency],
    -- | A unique identifier to assign to the new game session placement. This
    -- value is developer-defined. The value must be unique across all Regions
    -- and cannot be reused.
    placementId :: Prelude.Text,
    -- | Name of the queue to use to place the new game session. You can use
    -- either the queue name or ARN value.
    gameSessionQueueName :: Prelude.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartGameSessionPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredPlayerSessions', 'startGameSessionPlacement_desiredPlayerSessions' - Set of information on each player to create a player session for.
--
-- 'gameProperties', 'startGameSessionPlacement_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'gameSessionData', 'startGameSessionPlacement_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the @GameSession@
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'gameSessionName', 'startGameSessionPlacement_gameSessionName' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'playerLatencies', 'startGameSessionPlacement_playerLatencies' - A set of values, expressed in milliseconds, that indicates the amount of
-- latency that a player experiences when connected to Amazon Web Services
-- Regions. This information is used to try to place the new game session
-- where it can offer the best possible gameplay experience for the
-- players.
--
-- 'placementId', 'startGameSessionPlacement_placementId' - A unique identifier to assign to the new game session placement. This
-- value is developer-defined. The value must be unique across all Regions
-- and cannot be reused.
--
-- 'gameSessionQueueName', 'startGameSessionPlacement_gameSessionQueueName' - Name of the queue to use to place the new game session. You can use
-- either the queue name or ARN value.
--
-- 'maximumPlayerSessionCount', 'startGameSessionPlacement_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
newStartGameSessionPlacement ::
  -- | 'placementId'
  Prelude.Text ->
  -- | 'gameSessionQueueName'
  Prelude.Text ->
  -- | 'maximumPlayerSessionCount'
  Prelude.Natural ->
  StartGameSessionPlacement
newStartGameSessionPlacement
  pPlacementId_
  pGameSessionQueueName_
  pMaximumPlayerSessionCount_ =
    StartGameSessionPlacement'
      { desiredPlayerSessions =
          Prelude.Nothing,
        gameProperties = Prelude.Nothing,
        gameSessionData = Prelude.Nothing,
        gameSessionName = Prelude.Nothing,
        playerLatencies = Prelude.Nothing,
        placementId = pPlacementId_,
        gameSessionQueueName = pGameSessionQueueName_,
        maximumPlayerSessionCount =
          pMaximumPlayerSessionCount_
      }

-- | Set of information on each player to create a player session for.
startGameSessionPlacement_desiredPlayerSessions :: Lens.Lens' StartGameSessionPlacement (Prelude.Maybe [DesiredPlayerSession])
startGameSessionPlacement_desiredPlayerSessions = Lens.lens (\StartGameSessionPlacement' {desiredPlayerSessions} -> desiredPlayerSessions) (\s@StartGameSessionPlacement' {} a -> s {desiredPlayerSessions = a} :: StartGameSessionPlacement) Prelude.. Lens.mapping Lens.coerced

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process with a
-- request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
startGameSessionPlacement_gameProperties :: Lens.Lens' StartGameSessionPlacement (Prelude.Maybe [GameProperty])
startGameSessionPlacement_gameProperties = Lens.lens (\StartGameSessionPlacement' {gameProperties} -> gameProperties) (\s@StartGameSessionPlacement' {} a -> s {gameProperties = a} :: StartGameSessionPlacement) Prelude.. Lens.mapping Lens.coerced

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the @GameSession@
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
startGameSessionPlacement_gameSessionData :: Lens.Lens' StartGameSessionPlacement (Prelude.Maybe Prelude.Text)
startGameSessionPlacement_gameSessionData = Lens.lens (\StartGameSessionPlacement' {gameSessionData} -> gameSessionData) (\s@StartGameSessionPlacement' {} a -> s {gameSessionData = a} :: StartGameSessionPlacement)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
startGameSessionPlacement_gameSessionName :: Lens.Lens' StartGameSessionPlacement (Prelude.Maybe Prelude.Text)
startGameSessionPlacement_gameSessionName = Lens.lens (\StartGameSessionPlacement' {gameSessionName} -> gameSessionName) (\s@StartGameSessionPlacement' {} a -> s {gameSessionName = a} :: StartGameSessionPlacement)

-- | A set of values, expressed in milliseconds, that indicates the amount of
-- latency that a player experiences when connected to Amazon Web Services
-- Regions. This information is used to try to place the new game session
-- where it can offer the best possible gameplay experience for the
-- players.
startGameSessionPlacement_playerLatencies :: Lens.Lens' StartGameSessionPlacement (Prelude.Maybe [PlayerLatency])
startGameSessionPlacement_playerLatencies = Lens.lens (\StartGameSessionPlacement' {playerLatencies} -> playerLatencies) (\s@StartGameSessionPlacement' {} a -> s {playerLatencies = a} :: StartGameSessionPlacement) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier to assign to the new game session placement. This
-- value is developer-defined. The value must be unique across all Regions
-- and cannot be reused.
startGameSessionPlacement_placementId :: Lens.Lens' StartGameSessionPlacement Prelude.Text
startGameSessionPlacement_placementId = Lens.lens (\StartGameSessionPlacement' {placementId} -> placementId) (\s@StartGameSessionPlacement' {} a -> s {placementId = a} :: StartGameSessionPlacement)

-- | Name of the queue to use to place the new game session. You can use
-- either the queue name or ARN value.
startGameSessionPlacement_gameSessionQueueName :: Lens.Lens' StartGameSessionPlacement Prelude.Text
startGameSessionPlacement_gameSessionQueueName = Lens.lens (\StartGameSessionPlacement' {gameSessionQueueName} -> gameSessionQueueName) (\s@StartGameSessionPlacement' {} a -> s {gameSessionQueueName = a} :: StartGameSessionPlacement)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
startGameSessionPlacement_maximumPlayerSessionCount :: Lens.Lens' StartGameSessionPlacement Prelude.Natural
startGameSessionPlacement_maximumPlayerSessionCount = Lens.lens (\StartGameSessionPlacement' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@StartGameSessionPlacement' {} a -> s {maximumPlayerSessionCount = a} :: StartGameSessionPlacement)

instance Core.AWSRequest StartGameSessionPlacement where
  type
    AWSResponse StartGameSessionPlacement =
      StartGameSessionPlacementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartGameSessionPlacementResponse'
            Prelude.<$> (x Data..?> "GameSessionPlacement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartGameSessionPlacement where
  hashWithSalt _salt StartGameSessionPlacement' {..} =
    _salt
      `Prelude.hashWithSalt` desiredPlayerSessions
      `Prelude.hashWithSalt` gameProperties
      `Prelude.hashWithSalt` gameSessionData
      `Prelude.hashWithSalt` gameSessionName
      `Prelude.hashWithSalt` playerLatencies
      `Prelude.hashWithSalt` placementId
      `Prelude.hashWithSalt` gameSessionQueueName
      `Prelude.hashWithSalt` maximumPlayerSessionCount

instance Prelude.NFData StartGameSessionPlacement where
  rnf StartGameSessionPlacement' {..} =
    Prelude.rnf desiredPlayerSessions `Prelude.seq`
      Prelude.rnf gameProperties `Prelude.seq`
        Prelude.rnf gameSessionData `Prelude.seq`
          Prelude.rnf gameSessionName `Prelude.seq`
            Prelude.rnf playerLatencies `Prelude.seq`
              Prelude.rnf placementId `Prelude.seq`
                Prelude.rnf gameSessionQueueName `Prelude.seq`
                  Prelude.rnf maximumPlayerSessionCount

instance Data.ToHeaders StartGameSessionPlacement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.StartGameSessionPlacement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartGameSessionPlacement where
  toJSON StartGameSessionPlacement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredPlayerSessions" Data..=)
              Prelude.<$> desiredPlayerSessions,
            ("GameProperties" Data..=)
              Prelude.<$> gameProperties,
            ("GameSessionData" Data..=)
              Prelude.<$> gameSessionData,
            ("GameSessionName" Data..=)
              Prelude.<$> gameSessionName,
            ("PlayerLatencies" Data..=)
              Prelude.<$> playerLatencies,
            Prelude.Just ("PlacementId" Data..= placementId),
            Prelude.Just
              ( "GameSessionQueueName"
                  Data..= gameSessionQueueName
              ),
            Prelude.Just
              ( "MaximumPlayerSessionCount"
                  Data..= maximumPlayerSessionCount
              )
          ]
      )

instance Data.ToPath StartGameSessionPlacement where
  toPath = Prelude.const "/"

instance Data.ToQuery StartGameSessionPlacement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartGameSessionPlacementResponse' smart constructor.
data StartGameSessionPlacementResponse = StartGameSessionPlacementResponse'
  { -- | Object that describes the newly created game session placement. This
    -- object includes all the information provided in the request, as well as
    -- start\/end time stamps and placement status.
    gameSessionPlacement :: Prelude.Maybe GameSessionPlacement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartGameSessionPlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionPlacement', 'startGameSessionPlacementResponse_gameSessionPlacement' - Object that describes the newly created game session placement. This
-- object includes all the information provided in the request, as well as
-- start\/end time stamps and placement status.
--
-- 'httpStatus', 'startGameSessionPlacementResponse_httpStatus' - The response's http status code.
newStartGameSessionPlacementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartGameSessionPlacementResponse
newStartGameSessionPlacementResponse pHttpStatus_ =
  StartGameSessionPlacementResponse'
    { gameSessionPlacement =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly created game session placement. This
-- object includes all the information provided in the request, as well as
-- start\/end time stamps and placement status.
startGameSessionPlacementResponse_gameSessionPlacement :: Lens.Lens' StartGameSessionPlacementResponse (Prelude.Maybe GameSessionPlacement)
startGameSessionPlacementResponse_gameSessionPlacement = Lens.lens (\StartGameSessionPlacementResponse' {gameSessionPlacement} -> gameSessionPlacement) (\s@StartGameSessionPlacementResponse' {} a -> s {gameSessionPlacement = a} :: StartGameSessionPlacementResponse)

-- | The response's http status code.
startGameSessionPlacementResponse_httpStatus :: Lens.Lens' StartGameSessionPlacementResponse Prelude.Int
startGameSessionPlacementResponse_httpStatus = Lens.lens (\StartGameSessionPlacementResponse' {httpStatus} -> httpStatus) (\s@StartGameSessionPlacementResponse' {} a -> s {httpStatus = a} :: StartGameSessionPlacementResponse)

instance
  Prelude.NFData
    StartGameSessionPlacementResponse
  where
  rnf StartGameSessionPlacementResponse' {..} =
    Prelude.rnf gameSessionPlacement `Prelude.seq`
      Prelude.rnf httpStatus
