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
-- Module      : Network.AWS.GameLift.StartGameSessionPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places a request for a new game session in a queue (see
-- CreateGameSessionQueue). When processing a placement request, Amazon
-- GameLift searches for available resources on the queue\'s destinations,
-- scanning each until it finds resources or the placement request times
-- out.
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
-- DescribeGameSessionPlacement and check the request\'s status. If the
-- status is @FULFILLED@, a new game session has been created and a game
-- session ARN and Region are referenced. If the placement request times
-- out, you can resubmit the request or retry it with a different queue.
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
module Network.AWS.GameLift.StartGameSessionPlacement
  ( -- * Creating a Request
    StartGameSessionPlacement (..),
    newStartGameSessionPlacement,

    -- * Request Lenses
    startGameSessionPlacement_gameProperties,
    startGameSessionPlacement_gameSessionData,
    startGameSessionPlacement_gameSessionName,
    startGameSessionPlacement_desiredPlayerSessions,
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

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newStartGameSessionPlacement' smart constructor.
data StartGameSessionPlacement = StartGameSessionPlacement'
  { -- | Set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameProperties :: Core.Maybe [GameProperty],
    -- | Set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Core.Maybe Core.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    gameSessionName :: Core.Maybe Core.Text,
    -- | Set of information on each player to create a player session for.
    desiredPlayerSessions :: Core.Maybe [DesiredPlayerSession],
    -- | Set of values, expressed in milliseconds, indicating the amount of
    -- latency that a player experiences when connected to AWS Regions. This
    -- information is used to try to place the new game session where it can
    -- offer the best possible gameplay experience for the players.
    playerLatencies :: Core.Maybe [PlayerLatency],
    -- | A unique identifier to assign to the new game session placement. This
    -- value is developer-defined. The value must be unique across all Regions
    -- and cannot be reused unless you are resubmitting a canceled or timed-out
    -- placement request.
    placementId :: Core.Text,
    -- | Name of the queue to use to place the new game session. You can use
    -- either the queue name or ARN value.
    gameSessionQueueName :: Core.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartGameSessionPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameProperties', 'startGameSessionPlacement_gameProperties' - Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'gameSessionData', 'startGameSessionPlacement_gameSessionData' - Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'gameSessionName', 'startGameSessionPlacement_gameSessionName' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'desiredPlayerSessions', 'startGameSessionPlacement_desiredPlayerSessions' - Set of information on each player to create a player session for.
--
-- 'playerLatencies', 'startGameSessionPlacement_playerLatencies' - Set of values, expressed in milliseconds, indicating the amount of
-- latency that a player experiences when connected to AWS Regions. This
-- information is used to try to place the new game session where it can
-- offer the best possible gameplay experience for the players.
--
-- 'placementId', 'startGameSessionPlacement_placementId' - A unique identifier to assign to the new game session placement. This
-- value is developer-defined. The value must be unique across all Regions
-- and cannot be reused unless you are resubmitting a canceled or timed-out
-- placement request.
--
-- 'gameSessionQueueName', 'startGameSessionPlacement_gameSessionQueueName' - Name of the queue to use to place the new game session. You can use
-- either the queue name or ARN value.
--
-- 'maximumPlayerSessionCount', 'startGameSessionPlacement_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
newStartGameSessionPlacement ::
  -- | 'placementId'
  Core.Text ->
  -- | 'gameSessionQueueName'
  Core.Text ->
  -- | 'maximumPlayerSessionCount'
  Core.Natural ->
  StartGameSessionPlacement
newStartGameSessionPlacement
  pPlacementId_
  pGameSessionQueueName_
  pMaximumPlayerSessionCount_ =
    StartGameSessionPlacement'
      { gameProperties =
          Core.Nothing,
        gameSessionData = Core.Nothing,
        gameSessionName = Core.Nothing,
        desiredPlayerSessions = Core.Nothing,
        playerLatencies = Core.Nothing,
        placementId = pPlacementId_,
        gameSessionQueueName = pGameSessionQueueName_,
        maximumPlayerSessionCount =
          pMaximumPlayerSessionCount_
      }

-- | Set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
startGameSessionPlacement_gameProperties :: Lens.Lens' StartGameSessionPlacement (Core.Maybe [GameProperty])
startGameSessionPlacement_gameProperties = Lens.lens (\StartGameSessionPlacement' {gameProperties} -> gameProperties) (\s@StartGameSessionPlacement' {} a -> s {gameProperties = a} :: StartGameSessionPlacement) Core.. Lens.mapping Lens._Coerce

-- | Set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
startGameSessionPlacement_gameSessionData :: Lens.Lens' StartGameSessionPlacement (Core.Maybe Core.Text)
startGameSessionPlacement_gameSessionData = Lens.lens (\StartGameSessionPlacement' {gameSessionData} -> gameSessionData) (\s@StartGameSessionPlacement' {} a -> s {gameSessionData = a} :: StartGameSessionPlacement)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
startGameSessionPlacement_gameSessionName :: Lens.Lens' StartGameSessionPlacement (Core.Maybe Core.Text)
startGameSessionPlacement_gameSessionName = Lens.lens (\StartGameSessionPlacement' {gameSessionName} -> gameSessionName) (\s@StartGameSessionPlacement' {} a -> s {gameSessionName = a} :: StartGameSessionPlacement)

-- | Set of information on each player to create a player session for.
startGameSessionPlacement_desiredPlayerSessions :: Lens.Lens' StartGameSessionPlacement (Core.Maybe [DesiredPlayerSession])
startGameSessionPlacement_desiredPlayerSessions = Lens.lens (\StartGameSessionPlacement' {desiredPlayerSessions} -> desiredPlayerSessions) (\s@StartGameSessionPlacement' {} a -> s {desiredPlayerSessions = a} :: StartGameSessionPlacement) Core.. Lens.mapping Lens._Coerce

-- | Set of values, expressed in milliseconds, indicating the amount of
-- latency that a player experiences when connected to AWS Regions. This
-- information is used to try to place the new game session where it can
-- offer the best possible gameplay experience for the players.
startGameSessionPlacement_playerLatencies :: Lens.Lens' StartGameSessionPlacement (Core.Maybe [PlayerLatency])
startGameSessionPlacement_playerLatencies = Lens.lens (\StartGameSessionPlacement' {playerLatencies} -> playerLatencies) (\s@StartGameSessionPlacement' {} a -> s {playerLatencies = a} :: StartGameSessionPlacement) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier to assign to the new game session placement. This
-- value is developer-defined. The value must be unique across all Regions
-- and cannot be reused unless you are resubmitting a canceled or timed-out
-- placement request.
startGameSessionPlacement_placementId :: Lens.Lens' StartGameSessionPlacement Core.Text
startGameSessionPlacement_placementId = Lens.lens (\StartGameSessionPlacement' {placementId} -> placementId) (\s@StartGameSessionPlacement' {} a -> s {placementId = a} :: StartGameSessionPlacement)

-- | Name of the queue to use to place the new game session. You can use
-- either the queue name or ARN value.
startGameSessionPlacement_gameSessionQueueName :: Lens.Lens' StartGameSessionPlacement Core.Text
startGameSessionPlacement_gameSessionQueueName = Lens.lens (\StartGameSessionPlacement' {gameSessionQueueName} -> gameSessionQueueName) (\s@StartGameSessionPlacement' {} a -> s {gameSessionQueueName = a} :: StartGameSessionPlacement)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
startGameSessionPlacement_maximumPlayerSessionCount :: Lens.Lens' StartGameSessionPlacement Core.Natural
startGameSessionPlacement_maximumPlayerSessionCount = Lens.lens (\StartGameSessionPlacement' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@StartGameSessionPlacement' {} a -> s {maximumPlayerSessionCount = a} :: StartGameSessionPlacement)

instance Core.AWSRequest StartGameSessionPlacement where
  type
    AWSResponse StartGameSessionPlacement =
      StartGameSessionPlacementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartGameSessionPlacementResponse'
            Core.<$> (x Core..?> "GameSessionPlacement")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartGameSessionPlacement

instance Core.NFData StartGameSessionPlacement

instance Core.ToHeaders StartGameSessionPlacement where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.StartGameSessionPlacement" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartGameSessionPlacement where
  toJSON StartGameSessionPlacement' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GameProperties" Core..=) Core.<$> gameProperties,
            ("GameSessionData" Core..=) Core.<$> gameSessionData,
            ("GameSessionName" Core..=) Core.<$> gameSessionName,
            ("DesiredPlayerSessions" Core..=)
              Core.<$> desiredPlayerSessions,
            ("PlayerLatencies" Core..=) Core.<$> playerLatencies,
            Core.Just ("PlacementId" Core..= placementId),
            Core.Just
              ( "GameSessionQueueName"
                  Core..= gameSessionQueueName
              ),
            Core.Just
              ( "MaximumPlayerSessionCount"
                  Core..= maximumPlayerSessionCount
              )
          ]
      )

instance Core.ToPath StartGameSessionPlacement where
  toPath = Core.const "/"

instance Core.ToQuery StartGameSessionPlacement where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newStartGameSessionPlacementResponse' smart constructor.
data StartGameSessionPlacementResponse = StartGameSessionPlacementResponse'
  { -- | Object that describes the newly created game session placement. This
    -- object includes all the information provided in the request, as well as
    -- start\/end time stamps and placement status.
    gameSessionPlacement :: Core.Maybe GameSessionPlacement,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StartGameSessionPlacementResponse
newStartGameSessionPlacementResponse pHttpStatus_ =
  StartGameSessionPlacementResponse'
    { gameSessionPlacement =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the newly created game session placement. This
-- object includes all the information provided in the request, as well as
-- start\/end time stamps and placement status.
startGameSessionPlacementResponse_gameSessionPlacement :: Lens.Lens' StartGameSessionPlacementResponse (Core.Maybe GameSessionPlacement)
startGameSessionPlacementResponse_gameSessionPlacement = Lens.lens (\StartGameSessionPlacementResponse' {gameSessionPlacement} -> gameSessionPlacement) (\s@StartGameSessionPlacementResponse' {} a -> s {gameSessionPlacement = a} :: StartGameSessionPlacementResponse)

-- | The response's http status code.
startGameSessionPlacementResponse_httpStatus :: Lens.Lens' StartGameSessionPlacementResponse Core.Int
startGameSessionPlacementResponse_httpStatus = Lens.lens (\StartGameSessionPlacementResponse' {httpStatus} -> httpStatus) (\s@StartGameSessionPlacementResponse' {} a -> s {httpStatus = a} :: StartGameSessionPlacementResponse)

instance
  Core.NFData
    StartGameSessionPlacementResponse
