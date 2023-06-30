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
-- Module      : Amazonka.GameLift.Types.GameSessionQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionQueue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.FilterConfiguration
import Amazonka.GameLift.Types.GameSessionQueueDestination
import Amazonka.GameLift.Types.PlayerLatencyPolicy
import Amazonka.GameLift.Types.PriorityConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Configuration for a game session placement mechanism that processes
-- requests for new game sessions. A queue can be used on its own or as
-- part of a matchmaking solution.
--
-- /See:/ 'newGameSessionQueue' smart constructor.
data GameSessionQueue = GameSessionQueue'
  { -- | Information that is added to all events that are related to this game
    -- session queue.
    customEventData :: Prelude.Maybe Prelude.Text,
    -- | A list of fleets and\/or fleet aliases that can be used to fulfill game
    -- session placement requests in the queue. Destinations are identified by
    -- either a fleet ARN or a fleet alias ARN, and are listed in order of
    -- placement preference.
    destinations :: Prelude.Maybe [GameSessionQueueDestination],
    -- | A list of locations where a queue is allowed to place new game sessions.
    -- Locations are specified in the form of Amazon Web Services Region codes,
    -- such as @us-west-2@. If this parameter is not set, game sessions can be
    -- placed in any queue location.
    filterConfiguration :: Prelude.Maybe FilterConfiguration,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift game session queue resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. In a
    -- GameLift game session queue ARN, the resource ID matches the /Name/
    -- value.
    gameSessionQueueArn :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region.
    name :: Prelude.Maybe Prelude.Text,
    -- | An SNS topic ARN that is set up to receive game session placement
    -- notifications. See
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
    notificationTarget :: Prelude.Maybe Prelude.Text,
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
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural
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
-- 'destinations', 'gameSessionQueue_destinations' - A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference.
--
-- 'filterConfiguration', 'gameSessionQueue_filterConfiguration' - A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of Amazon Web Services Region codes,
-- such as @us-west-2@. If this parameter is not set, game sessions can be
-- placed in any queue location.
--
-- 'gameSessionQueueArn', 'gameSessionQueue_gameSessionQueueArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. In a
-- GameLift game session queue ARN, the resource ID matches the /Name/
-- value.
--
-- 'name', 'gameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
--
-- 'notificationTarget', 'gameSessionQueue_notificationTarget' - An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
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
-- 'timeoutInSeconds', 'gameSessionQueue_timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
newGameSessionQueue ::
  GameSessionQueue
newGameSessionQueue =
  GameSessionQueue'
    { customEventData =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      filterConfiguration = Prelude.Nothing,
      gameSessionQueueArn = Prelude.Nothing,
      name = Prelude.Nothing,
      notificationTarget = Prelude.Nothing,
      playerLatencyPolicies = Prelude.Nothing,
      priorityConfiguration = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing
    }

-- | Information that is added to all events that are related to this game
-- session queue.
gameSessionQueue_customEventData :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_customEventData = Lens.lens (\GameSessionQueue' {customEventData} -> customEventData) (\s@GameSessionQueue' {} a -> s {customEventData = a} :: GameSessionQueue)

-- | A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference.
gameSessionQueue_destinations :: Lens.Lens' GameSessionQueue (Prelude.Maybe [GameSessionQueueDestination])
gameSessionQueue_destinations = Lens.lens (\GameSessionQueue' {destinations} -> destinations) (\s@GameSessionQueue' {} a -> s {destinations = a} :: GameSessionQueue) Prelude.. Lens.mapping Lens.coerced

-- | A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of Amazon Web Services Region codes,
-- such as @us-west-2@. If this parameter is not set, game sessions can be
-- placed in any queue location.
gameSessionQueue_filterConfiguration :: Lens.Lens' GameSessionQueue (Prelude.Maybe FilterConfiguration)
gameSessionQueue_filterConfiguration = Lens.lens (\GameSessionQueue' {filterConfiguration} -> filterConfiguration) (\s@GameSessionQueue' {} a -> s {filterConfiguration = a} :: GameSessionQueue)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::gamesessionqueue\/\<queue name>@. In a
-- GameLift game session queue ARN, the resource ID matches the /Name/
-- value.
gameSessionQueue_gameSessionQueueArn :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_gameSessionQueueArn = Lens.lens (\GameSessionQueue' {gameSessionQueueArn} -> gameSessionQueueArn) (\s@GameSessionQueue' {} a -> s {gameSessionQueueArn = a} :: GameSessionQueue)

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
gameSessionQueue_name :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_name = Lens.lens (\GameSessionQueue' {name} -> name) (\s@GameSessionQueue' {} a -> s {name = a} :: GameSessionQueue)

-- | An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
gameSessionQueue_notificationTarget :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_notificationTarget = Lens.lens (\GameSessionQueue' {notificationTarget} -> notificationTarget) (\s@GameSessionQueue' {} a -> s {notificationTarget = a} :: GameSessionQueue)

-- | A set of policies that act as a sliding cap on player latency. FleetIQ
-- works to deliver low latency for most players in a game session. These
-- policies ensure that no individual player can be placed into a game with
-- unreasonably high latency. Use multiple policies to gradually relax
-- latency requirements a step at a time. Multiple policies are applied
-- based on their maximum allowed latency, starting with the lowest value.
gameSessionQueue_playerLatencyPolicies :: Lens.Lens' GameSessionQueue (Prelude.Maybe [PlayerLatencyPolicy])
gameSessionQueue_playerLatencyPolicies = Lens.lens (\GameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@GameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: GameSessionQueue) Prelude.. Lens.mapping Lens.coerced

-- | Custom settings to use when prioritizing destinations and locations for
-- game session placements. This configuration replaces the FleetIQ default
-- prioritization process. Priority types that are not explicitly named
-- will be automatically applied at the end of the prioritization process.
gameSessionQueue_priorityConfiguration :: Lens.Lens' GameSessionQueue (Prelude.Maybe PriorityConfiguration)
gameSessionQueue_priorityConfiguration = Lens.lens (\GameSessionQueue' {priorityConfiguration} -> priorityConfiguration) (\s@GameSessionQueue' {} a -> s {priorityConfiguration = a} :: GameSessionQueue)

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
gameSessionQueue_timeoutInSeconds :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Natural)
gameSessionQueue_timeoutInSeconds = Lens.lens (\GameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@GameSessionQueue' {} a -> s {timeoutInSeconds = a} :: GameSessionQueue)

instance Data.FromJSON GameSessionQueue where
  parseJSON =
    Data.withObject
      "GameSessionQueue"
      ( \x ->
          GameSessionQueue'
            Prelude.<$> (x Data..:? "CustomEventData")
            Prelude.<*> (x Data..:? "Destinations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FilterConfiguration")
            Prelude.<*> (x Data..:? "GameSessionQueueArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NotificationTarget")
            Prelude.<*> ( x
                            Data..:? "PlayerLatencyPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PriorityConfiguration")
            Prelude.<*> (x Data..:? "TimeoutInSeconds")
      )

instance Prelude.Hashable GameSessionQueue where
  hashWithSalt _salt GameSessionQueue' {..} =
    _salt
      `Prelude.hashWithSalt` customEventData
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` filterConfiguration
      `Prelude.hashWithSalt` gameSessionQueueArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notificationTarget
      `Prelude.hashWithSalt` playerLatencyPolicies
      `Prelude.hashWithSalt` priorityConfiguration
      `Prelude.hashWithSalt` timeoutInSeconds

instance Prelude.NFData GameSessionQueue where
  rnf GameSessionQueue' {..} =
    Prelude.rnf customEventData
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf filterConfiguration
      `Prelude.seq` Prelude.rnf gameSessionQueueArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notificationTarget
      `Prelude.seq` Prelude.rnf playerLatencyPolicies
      `Prelude.seq` Prelude.rnf priorityConfiguration
      `Prelude.seq` Prelude.rnf timeoutInSeconds
