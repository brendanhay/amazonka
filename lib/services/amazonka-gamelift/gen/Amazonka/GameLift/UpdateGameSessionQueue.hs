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
-- Module      : Amazonka.GameLift.UpdateGameSessionQueue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of a game session queue, which determines how
-- the queue processes new game session requests. To update settings,
-- specify the queue name to be updated and provide the new settings. When
-- updating destinations, provide a complete list of destinations.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Using Multi-Region Queues>
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_CreateGameSessionQueue.html CreateGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeGameSessionQueues.html DescribeGameSessionQueues>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UpdateGameSessionQueue.html UpdateGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DeleteGameSessionQueue.html DeleteGameSessionQueue>
-- |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.UpdateGameSessionQueue
  ( -- * Creating a Request
    UpdateGameSessionQueue (..),
    newUpdateGameSessionQueue,

    -- * Request Lenses
    updateGameSessionQueue_notificationTarget,
    updateGameSessionQueue_priorityConfiguration,
    updateGameSessionQueue_timeoutInSeconds,
    updateGameSessionQueue_playerLatencyPolicies,
    updateGameSessionQueue_destinations,
    updateGameSessionQueue_filterConfiguration,
    updateGameSessionQueue_customEventData,
    updateGameSessionQueue_name,

    -- * Destructuring the Response
    UpdateGameSessionQueueResponse (..),
    newUpdateGameSessionQueueResponse,

    -- * Response Lenses
    updateGameSessionQueueResponse_gameSessionQueue,
    updateGameSessionQueueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateGameSessionQueue' smart constructor.
data UpdateGameSessionQueue = UpdateGameSessionQueue'
  { -- | An SNS topic ARN that is set up to receive game session placement
    -- notifications. See
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
    notificationTarget :: Prelude.Maybe Prelude.Text,
    -- | Custom settings to use when prioritizing destinations and locations for
    -- game session placements. This configuration replaces the FleetIQ default
    -- prioritization process. Priority types that are not explicitly named
    -- will be automatically applied at the end of the prioritization process.
    -- To remove an existing priority configuration, pass in an empty set.
    priorityConfiguration :: Prelude.Maybe PriorityConfiguration,
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A set of policies that act as a sliding cap on player latency. FleetIQ
    -- works to deliver low latency for most players in a game session. These
    -- policies ensure that no individual player can be placed into a game with
    -- unreasonably high latency. Use multiple policies to gradually relax
    -- latency requirements a step at a time. Multiple policies are applied
    -- based on their maximum allowed latency, starting with the lowest value.
    -- When updating policies, provide a complete collection of policies.
    playerLatencyPolicies :: Prelude.Maybe [PlayerLatencyPolicy],
    -- | A list of fleets and\/or fleet aliases that can be used to fulfill game
    -- session placement requests in the queue. Destinations are identified by
    -- either a fleet ARN or a fleet alias ARN, and are listed in order of
    -- placement preference. When updating this list, provide a complete list
    -- of destinations.
    destinations :: Prelude.Maybe [GameSessionQueueDestination],
    -- | A list of locations where a queue is allowed to place new game sessions.
    -- Locations are specified in the form of Amazon Web Services Region codes,
    -- such as @us-west-2@. If this parameter is not set, game sessions can be
    -- placed in any queue location. To remove an existing filter
    -- configuration, pass in an empty set.
    filterConfiguration :: Prelude.Maybe FilterConfiguration,
    -- | Information to be added to all events that are related to this game
    -- session queue.
    customEventData :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region. You can use either the queue ID
    -- or ARN value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameSessionQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationTarget', 'updateGameSessionQueue_notificationTarget' - An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
--
-- 'priorityConfiguration', 'updateGameSessionQueue_priorityConfiguration' - Custom settings to use when prioritizing destinations and locations for
-- game session placements. This configuration replaces the FleetIQ default
-- prioritization process. Priority types that are not explicitly named
-- will be automatically applied at the end of the prioritization process.
-- To remove an existing priority configuration, pass in an empty set.
--
-- 'timeoutInSeconds', 'updateGameSessionQueue_timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
--
-- 'playerLatencyPolicies', 'updateGameSessionQueue_playerLatencyPolicies' - A set of policies that act as a sliding cap on player latency. FleetIQ
-- works to deliver low latency for most players in a game session. These
-- policies ensure that no individual player can be placed into a game with
-- unreasonably high latency. Use multiple policies to gradually relax
-- latency requirements a step at a time. Multiple policies are applied
-- based on their maximum allowed latency, starting with the lowest value.
-- When updating policies, provide a complete collection of policies.
--
-- 'destinations', 'updateGameSessionQueue_destinations' - A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference. When updating this list, provide a complete list
-- of destinations.
--
-- 'filterConfiguration', 'updateGameSessionQueue_filterConfiguration' - A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of Amazon Web Services Region codes,
-- such as @us-west-2@. If this parameter is not set, game sessions can be
-- placed in any queue location. To remove an existing filter
-- configuration, pass in an empty set.
--
-- 'customEventData', 'updateGameSessionQueue_customEventData' - Information to be added to all events that are related to this game
-- session queue.
--
-- 'name', 'updateGameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
newUpdateGameSessionQueue ::
  -- | 'name'
  Prelude.Text ->
  UpdateGameSessionQueue
newUpdateGameSessionQueue pName_ =
  UpdateGameSessionQueue'
    { notificationTarget =
        Prelude.Nothing,
      priorityConfiguration = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      playerLatencyPolicies = Prelude.Nothing,
      destinations = Prelude.Nothing,
      filterConfiguration = Prelude.Nothing,
      customEventData = Prelude.Nothing,
      name = pName_
    }

-- | An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
updateGameSessionQueue_notificationTarget :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe Prelude.Text)
updateGameSessionQueue_notificationTarget = Lens.lens (\UpdateGameSessionQueue' {notificationTarget} -> notificationTarget) (\s@UpdateGameSessionQueue' {} a -> s {notificationTarget = a} :: UpdateGameSessionQueue)

-- | Custom settings to use when prioritizing destinations and locations for
-- game session placements. This configuration replaces the FleetIQ default
-- prioritization process. Priority types that are not explicitly named
-- will be automatically applied at the end of the prioritization process.
-- To remove an existing priority configuration, pass in an empty set.
updateGameSessionQueue_priorityConfiguration :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe PriorityConfiguration)
updateGameSessionQueue_priorityConfiguration = Lens.lens (\UpdateGameSessionQueue' {priorityConfiguration} -> priorityConfiguration) (\s@UpdateGameSessionQueue' {} a -> s {priorityConfiguration = a} :: UpdateGameSessionQueue)

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
updateGameSessionQueue_timeoutInSeconds :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe Prelude.Natural)
updateGameSessionQueue_timeoutInSeconds = Lens.lens (\UpdateGameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@UpdateGameSessionQueue' {} a -> s {timeoutInSeconds = a} :: UpdateGameSessionQueue)

-- | A set of policies that act as a sliding cap on player latency. FleetIQ
-- works to deliver low latency for most players in a game session. These
-- policies ensure that no individual player can be placed into a game with
-- unreasonably high latency. Use multiple policies to gradually relax
-- latency requirements a step at a time. Multiple policies are applied
-- based on their maximum allowed latency, starting with the lowest value.
-- When updating policies, provide a complete collection of policies.
updateGameSessionQueue_playerLatencyPolicies :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe [PlayerLatencyPolicy])
updateGameSessionQueue_playerLatencyPolicies = Lens.lens (\UpdateGameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@UpdateGameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: UpdateGameSessionQueue) Prelude.. Lens.mapping Lens.coerced

-- | A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference. When updating this list, provide a complete list
-- of destinations.
updateGameSessionQueue_destinations :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe [GameSessionQueueDestination])
updateGameSessionQueue_destinations = Lens.lens (\UpdateGameSessionQueue' {destinations} -> destinations) (\s@UpdateGameSessionQueue' {} a -> s {destinations = a} :: UpdateGameSessionQueue) Prelude.. Lens.mapping Lens.coerced

-- | A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of Amazon Web Services Region codes,
-- such as @us-west-2@. If this parameter is not set, game sessions can be
-- placed in any queue location. To remove an existing filter
-- configuration, pass in an empty set.
updateGameSessionQueue_filterConfiguration :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe FilterConfiguration)
updateGameSessionQueue_filterConfiguration = Lens.lens (\UpdateGameSessionQueue' {filterConfiguration} -> filterConfiguration) (\s@UpdateGameSessionQueue' {} a -> s {filterConfiguration = a} :: UpdateGameSessionQueue)

-- | Information to be added to all events that are related to this game
-- session queue.
updateGameSessionQueue_customEventData :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe Prelude.Text)
updateGameSessionQueue_customEventData = Lens.lens (\UpdateGameSessionQueue' {customEventData} -> customEventData) (\s@UpdateGameSessionQueue' {} a -> s {customEventData = a} :: UpdateGameSessionQueue)

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
updateGameSessionQueue_name :: Lens.Lens' UpdateGameSessionQueue Prelude.Text
updateGameSessionQueue_name = Lens.lens (\UpdateGameSessionQueue' {name} -> name) (\s@UpdateGameSessionQueue' {} a -> s {name = a} :: UpdateGameSessionQueue)

instance Core.AWSRequest UpdateGameSessionQueue where
  type
    AWSResponse UpdateGameSessionQueue =
      UpdateGameSessionQueueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameSessionQueueResponse'
            Prelude.<$> (x Core..?> "GameSessionQueue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGameSessionQueue where
  hashWithSalt _salt UpdateGameSessionQueue' {..} =
    _salt `Prelude.hashWithSalt` notificationTarget
      `Prelude.hashWithSalt` priorityConfiguration
      `Prelude.hashWithSalt` timeoutInSeconds
      `Prelude.hashWithSalt` playerLatencyPolicies
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` filterConfiguration
      `Prelude.hashWithSalt` customEventData
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateGameSessionQueue where
  rnf UpdateGameSessionQueue' {..} =
    Prelude.rnf notificationTarget
      `Prelude.seq` Prelude.rnf priorityConfiguration
      `Prelude.seq` Prelude.rnf timeoutInSeconds
      `Prelude.seq` Prelude.rnf playerLatencyPolicies
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf filterConfiguration
      `Prelude.seq` Prelude.rnf customEventData
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateGameSessionQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.UpdateGameSessionQueue" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateGameSessionQueue where
  toJSON UpdateGameSessionQueue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NotificationTarget" Core..=)
              Prelude.<$> notificationTarget,
            ("PriorityConfiguration" Core..=)
              Prelude.<$> priorityConfiguration,
            ("TimeoutInSeconds" Core..=)
              Prelude.<$> timeoutInSeconds,
            ("PlayerLatencyPolicies" Core..=)
              Prelude.<$> playerLatencyPolicies,
            ("Destinations" Core..=) Prelude.<$> destinations,
            ("FilterConfiguration" Core..=)
              Prelude.<$> filterConfiguration,
            ("CustomEventData" Core..=)
              Prelude.<$> customEventData,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateGameSessionQueue where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateGameSessionQueue where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateGameSessionQueueResponse' smart constructor.
data UpdateGameSessionQueueResponse = UpdateGameSessionQueueResponse'
  { -- | An object that describes the newly updated game session queue.
    gameSessionQueue :: Prelude.Maybe GameSessionQueue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameSessionQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionQueue', 'updateGameSessionQueueResponse_gameSessionQueue' - An object that describes the newly updated game session queue.
--
-- 'httpStatus', 'updateGameSessionQueueResponse_httpStatus' - The response's http status code.
newUpdateGameSessionQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGameSessionQueueResponse
newUpdateGameSessionQueueResponse pHttpStatus_ =
  UpdateGameSessionQueueResponse'
    { gameSessionQueue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the newly updated game session queue.
updateGameSessionQueueResponse_gameSessionQueue :: Lens.Lens' UpdateGameSessionQueueResponse (Prelude.Maybe GameSessionQueue)
updateGameSessionQueueResponse_gameSessionQueue = Lens.lens (\UpdateGameSessionQueueResponse' {gameSessionQueue} -> gameSessionQueue) (\s@UpdateGameSessionQueueResponse' {} a -> s {gameSessionQueue = a} :: UpdateGameSessionQueueResponse)

-- | The response's http status code.
updateGameSessionQueueResponse_httpStatus :: Lens.Lens' UpdateGameSessionQueueResponse Prelude.Int
updateGameSessionQueueResponse_httpStatus = Lens.lens (\UpdateGameSessionQueueResponse' {httpStatus} -> httpStatus) (\s@UpdateGameSessionQueueResponse' {} a -> s {httpStatus = a} :: UpdateGameSessionQueueResponse)

instance
  Prelude.NFData
    UpdateGameSessionQueueResponse
  where
  rnf UpdateGameSessionQueueResponse' {..} =
    Prelude.rnf gameSessionQueue
      `Prelude.seq` Prelude.rnf httpStatus
