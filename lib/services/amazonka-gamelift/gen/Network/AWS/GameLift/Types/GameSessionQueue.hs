{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionQueue where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.FilterConfiguration
import Network.AWS.GameLift.Types.GameSessionQueueDestination
import Network.AWS.GameLift.Types.PlayerLatencyPolicy
import Network.AWS.GameLift.Types.PriorityConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration for a game session placement mechanism that processes
-- requests for new game sessions. A queue can be used on its own or as
-- part of a matchmaking solution.
--
-- __Related actions__
--
-- CreateGameSessionQueue | DescribeGameSessionQueues |
-- UpdateGameSessionQueue
--
-- /See:/ 'newGameSessionQueue' smart constructor.
data GameSessionQueue = GameSessionQueue'
  { -- | Information that is added to all events that are related to this game
    -- session queue.
    customEventData :: Prelude.Maybe Prelude.Text,
    -- | A set of policies that act as a sliding cap on player latency. FleetIQ
    -- works to deliver low latency for most players in a game session. These
    -- policies ensure that no individual player can be placed into a game with
    -- unreasonably high latency. Use multiple policies to gradually relax
    -- latency requirements a step at a time. Multiple policies are applied
    -- based on their maximum allowed latency, starting with the lowest value.
    playerLatencyPolicies :: Prelude.Maybe [PlayerLatencyPolicy],
    -- | Custom settings to use when prioritizing destinations and locations for
    -- game session placements. This configuration replaces the FleetIQ default
    -- prioritization process. Priority types that are not explicitly named
    -- will be automatically applied at the end of the prioritization process.
    priorityConfiguration :: Prelude.Maybe PriorityConfiguration,
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of fleets and\/or fleet aliases that can be used to fulfill game
    -- session placement requests in the queue. Destinations are identified by
    -- either a fleet ARN or a fleet alias ARN, and are listed in order of
    -- placement preference.
    destinations :: Prelude.Maybe [GameSessionQueueDestination],
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift game session queue resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. In a
    -- GameLift game session queue ARN, the resource ID matches the /Name/
    -- value.
    gameSessionQueueArn :: Prelude.Maybe Prelude.Text,
    -- | An SNS topic ARN that is set up to receive game session placement
    -- notifications. See
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
    notificationTarget :: Prelude.Maybe Prelude.Text,
    -- | A list of locations where a queue is allowed to place new game sessions.
    -- Locations are specified in the form of AWS Region codes, such as
    -- @us-west-2@. If this parameter is not set, game sessions can be placed
    -- in any queue location.
    filterConfiguration :: Prelude.Maybe FilterConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameSessionQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customEventData', 'gameSessionQueue_customEventData' - Information that is added to all events that are related to this game
-- session queue.
--
-- 'playerLatencyPolicies', 'gameSessionQueue_playerLatencyPolicies' - A set of policies that act as a sliding cap on player latency. FleetIQ
-- works to deliver low latency for most players in a game session. These
-- policies ensure that no individual player can be placed into a game with
-- unreasonably high latency. Use multiple policies to gradually relax
-- latency requirements a step at a time. Multiple policies are applied
-- based on their maximum allowed latency, starting with the lowest value.
--
-- 'priorityConfiguration', 'gameSessionQueue_priorityConfiguration' - Custom settings to use when prioritizing destinations and locations for
-- game session placements. This configuration replaces the FleetIQ default
-- prioritization process. Priority types that are not explicitly named
-- will be automatically applied at the end of the prioritization process.
--
-- 'name', 'gameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
--
-- 'destinations', 'gameSessionQueue_destinations' - A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference.
--
-- 'timeoutInSeconds', 'gameSessionQueue_timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
--
-- 'gameSessionQueueArn', 'gameSessionQueue_gameSessionQueueArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. In a
-- GameLift game session queue ARN, the resource ID matches the /Name/
-- value.
--
-- 'notificationTarget', 'gameSessionQueue_notificationTarget' - An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
--
-- 'filterConfiguration', 'gameSessionQueue_filterConfiguration' - A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of AWS Region codes, such as
-- @us-west-2@. If this parameter is not set, game sessions can be placed
-- in any queue location.
newGameSessionQueue ::
  GameSessionQueue
newGameSessionQueue =
  GameSessionQueue'
    { customEventData =
        Prelude.Nothing,
      playerLatencyPolicies = Prelude.Nothing,
      priorityConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      destinations = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      gameSessionQueueArn = Prelude.Nothing,
      notificationTarget = Prelude.Nothing,
      filterConfiguration = Prelude.Nothing
    }

-- | Information that is added to all events that are related to this game
-- session queue.
gameSessionQueue_customEventData :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_customEventData = Lens.lens (\GameSessionQueue' {customEventData} -> customEventData) (\s@GameSessionQueue' {} a -> s {customEventData = a} :: GameSessionQueue)

-- | A set of policies that act as a sliding cap on player latency. FleetIQ
-- works to deliver low latency for most players in a game session. These
-- policies ensure that no individual player can be placed into a game with
-- unreasonably high latency. Use multiple policies to gradually relax
-- latency requirements a step at a time. Multiple policies are applied
-- based on their maximum allowed latency, starting with the lowest value.
gameSessionQueue_playerLatencyPolicies :: Lens.Lens' GameSessionQueue (Prelude.Maybe [PlayerLatencyPolicy])
gameSessionQueue_playerLatencyPolicies = Lens.lens (\GameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@GameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: GameSessionQueue) Prelude.. Lens.mapping Lens._Coerce

-- | Custom settings to use when prioritizing destinations and locations for
-- game session placements. This configuration replaces the FleetIQ default
-- prioritization process. Priority types that are not explicitly named
-- will be automatically applied at the end of the prioritization process.
gameSessionQueue_priorityConfiguration :: Lens.Lens' GameSessionQueue (Prelude.Maybe PriorityConfiguration)
gameSessionQueue_priorityConfiguration = Lens.lens (\GameSessionQueue' {priorityConfiguration} -> priorityConfiguration) (\s@GameSessionQueue' {} a -> s {priorityConfiguration = a} :: GameSessionQueue)

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
gameSessionQueue_name :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_name = Lens.lens (\GameSessionQueue' {name} -> name) (\s@GameSessionQueue' {} a -> s {name = a} :: GameSessionQueue)

-- | A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference.
gameSessionQueue_destinations :: Lens.Lens' GameSessionQueue (Prelude.Maybe [GameSessionQueueDestination])
gameSessionQueue_destinations = Lens.lens (\GameSessionQueue' {destinations} -> destinations) (\s@GameSessionQueue' {} a -> s {destinations = a} :: GameSessionQueue) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
gameSessionQueue_timeoutInSeconds :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Natural)
gameSessionQueue_timeoutInSeconds = Lens.lens (\GameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@GameSessionQueue' {} a -> s {timeoutInSeconds = a} :: GameSessionQueue)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. In a
-- GameLift game session queue ARN, the resource ID matches the /Name/
-- value.
gameSessionQueue_gameSessionQueueArn :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_gameSessionQueueArn = Lens.lens (\GameSessionQueue' {gameSessionQueueArn} -> gameSessionQueueArn) (\s@GameSessionQueue' {} a -> s {gameSessionQueueArn = a} :: GameSessionQueue)

-- | An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
gameSessionQueue_notificationTarget :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_notificationTarget = Lens.lens (\GameSessionQueue' {notificationTarget} -> notificationTarget) (\s@GameSessionQueue' {} a -> s {notificationTarget = a} :: GameSessionQueue)

-- | A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of AWS Region codes, such as
-- @us-west-2@. If this parameter is not set, game sessions can be placed
-- in any queue location.
gameSessionQueue_filterConfiguration :: Lens.Lens' GameSessionQueue (Prelude.Maybe FilterConfiguration)
gameSessionQueue_filterConfiguration = Lens.lens (\GameSessionQueue' {filterConfiguration} -> filterConfiguration) (\s@GameSessionQueue' {} a -> s {filterConfiguration = a} :: GameSessionQueue)

instance Core.FromJSON GameSessionQueue where
  parseJSON =
    Core.withObject
      "GameSessionQueue"
      ( \x ->
          GameSessionQueue'
            Prelude.<$> (x Core..:? "CustomEventData")
            Prelude.<*> ( x Core..:? "PlayerLatencyPolicies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PriorityConfiguration")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Destinations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TimeoutInSeconds")
            Prelude.<*> (x Core..:? "GameSessionQueueArn")
            Prelude.<*> (x Core..:? "NotificationTarget")
            Prelude.<*> (x Core..:? "FilterConfiguration")
      )

instance Prelude.Hashable GameSessionQueue

instance Prelude.NFData GameSessionQueue
