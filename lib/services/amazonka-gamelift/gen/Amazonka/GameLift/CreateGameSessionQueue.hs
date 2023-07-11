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
-- Module      : Amazonka.GameLift.CreateGameSessionQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a placement queue that processes requests for new game sessions.
-- A queue uses FleetIQ algorithms to determine the best placement
-- locations and find an available game server there, then prompts the game
-- server process to start a new game session.
--
-- A game session queue is configured with a set of destinations (GameLift
-- fleets or aliases), which determine the locations where the queue can
-- place new game sessions. These destinations can span multiple fleet
-- types (Spot and On-Demand), instance types, and Amazon Web Services
-- Regions. If the queue includes multi-location fleets, the queue is able
-- to place game sessions in all of a fleet\'s remote locations. You can
-- opt to filter out individual locations if needed.
--
-- The queue configuration also determines how FleetIQ selects the best
-- available placement for a new game session. Before searching for an
-- available game server, FleetIQ first prioritizes the queue\'s
-- destinations and locations, with the best placement locations on top.
-- You can set up the queue to use the FleetIQ default prioritization or
-- provide an alternate set of priorities.
--
-- To create a new queue, provide a name, timeout value, and a list of
-- destinations. Optionally, specify a sort configuration and\/or a filter,
-- and define a set of latency cap policies. You can also include the ARN
-- for an Amazon Simple Notification Service (SNS) topic to receive
-- notifications of game session placement activity. Notifications using
-- SNS or CloudWatch events is the preferred way to track placement
-- activity.
--
-- If successful, a new @GameSessionQueue@ object is returned with an
-- assigned queue ARN. New game session requests, which are submitted to
-- queue with
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StartGameSessionPlacement.html StartGameSessionPlacement>
-- or
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StartMatchmaking.html StartMatchmaking>,
-- reference a queue\'s name or ARN.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-design.html Design a game session queue>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-creating.html Create a game session queue>
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
module Amazonka.GameLift.CreateGameSessionQueue
  ( -- * Creating a Request
    CreateGameSessionQueue (..),
    newCreateGameSessionQueue,

    -- * Request Lenses
    createGameSessionQueue_customEventData,
    createGameSessionQueue_destinations,
    createGameSessionQueue_filterConfiguration,
    createGameSessionQueue_notificationTarget,
    createGameSessionQueue_playerLatencyPolicies,
    createGameSessionQueue_priorityConfiguration,
    createGameSessionQueue_tags,
    createGameSessionQueue_timeoutInSeconds,
    createGameSessionQueue_name,

    -- * Destructuring the Response
    CreateGameSessionQueueResponse (..),
    newCreateGameSessionQueueResponse,

    -- * Response Lenses
    createGameSessionQueueResponse_gameSessionQueue,
    createGameSessionQueueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGameSessionQueue' smart constructor.
data CreateGameSessionQueue = CreateGameSessionQueue'
  { -- | Information to be added to all events that are related to this game
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
    -- | A list of labels to assign to the new game session queue resource. Tags
    -- are developer-defined key-value pairs. Tagging Amazon Web Services
    -- resources are useful for resource management, access management and cost
    -- allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Reference/.
    tags :: Prelude.Maybe [Tag],
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGameSessionQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customEventData', 'createGameSessionQueue_customEventData' - Information to be added to all events that are related to this game
-- session queue.
--
-- 'destinations', 'createGameSessionQueue_destinations' - A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference.
--
-- 'filterConfiguration', 'createGameSessionQueue_filterConfiguration' - A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of Amazon Web Services Region codes,
-- such as @us-west-2@. If this parameter is not set, game sessions can be
-- placed in any queue location.
--
-- 'notificationTarget', 'createGameSessionQueue_notificationTarget' - An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
--
-- 'playerLatencyPolicies', 'createGameSessionQueue_playerLatencyPolicies' - A set of policies that act as a sliding cap on player latency. FleetIQ
-- works to deliver low latency for most players in a game session. These
-- policies ensure that no individual player can be placed into a game with
-- unreasonably high latency. Use multiple policies to gradually relax
-- latency requirements a step at a time. Multiple policies are applied
-- based on their maximum allowed latency, starting with the lowest value.
--
-- 'priorityConfiguration', 'createGameSessionQueue_priorityConfiguration' - Custom settings to use when prioritizing destinations and locations for
-- game session placements. This configuration replaces the FleetIQ default
-- prioritization process. Priority types that are not explicitly named
-- will be automatically applied at the end of the prioritization process.
--
-- 'tags', 'createGameSessionQueue_tags' - A list of labels to assign to the new game session queue resource. Tags
-- are developer-defined key-value pairs. Tagging Amazon Web Services
-- resources are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
--
-- 'timeoutInSeconds', 'createGameSessionQueue_timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
--
-- 'name', 'createGameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
newCreateGameSessionQueue ::
  -- | 'name'
  Prelude.Text ->
  CreateGameSessionQueue
newCreateGameSessionQueue pName_ =
  CreateGameSessionQueue'
    { customEventData =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      filterConfiguration = Prelude.Nothing,
      notificationTarget = Prelude.Nothing,
      playerLatencyPolicies = Prelude.Nothing,
      priorityConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      name = pName_
    }

-- | Information to be added to all events that are related to this game
-- session queue.
createGameSessionQueue_customEventData :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe Prelude.Text)
createGameSessionQueue_customEventData = Lens.lens (\CreateGameSessionQueue' {customEventData} -> customEventData) (\s@CreateGameSessionQueue' {} a -> s {customEventData = a} :: CreateGameSessionQueue)

-- | A list of fleets and\/or fleet aliases that can be used to fulfill game
-- session placement requests in the queue. Destinations are identified by
-- either a fleet ARN or a fleet alias ARN, and are listed in order of
-- placement preference.
createGameSessionQueue_destinations :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe [GameSessionQueueDestination])
createGameSessionQueue_destinations = Lens.lens (\CreateGameSessionQueue' {destinations} -> destinations) (\s@CreateGameSessionQueue' {} a -> s {destinations = a} :: CreateGameSessionQueue) Prelude.. Lens.mapping Lens.coerced

-- | A list of locations where a queue is allowed to place new game sessions.
-- Locations are specified in the form of Amazon Web Services Region codes,
-- such as @us-west-2@. If this parameter is not set, game sessions can be
-- placed in any queue location.
createGameSessionQueue_filterConfiguration :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe FilterConfiguration)
createGameSessionQueue_filterConfiguration = Lens.lens (\CreateGameSessionQueue' {filterConfiguration} -> filterConfiguration) (\s@CreateGameSessionQueue' {} a -> s {filterConfiguration = a} :: CreateGameSessionQueue)

-- | An SNS topic ARN that is set up to receive game session placement
-- notifications. See
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queue-notification.html Setting up notifications for game session placement>.
createGameSessionQueue_notificationTarget :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe Prelude.Text)
createGameSessionQueue_notificationTarget = Lens.lens (\CreateGameSessionQueue' {notificationTarget} -> notificationTarget) (\s@CreateGameSessionQueue' {} a -> s {notificationTarget = a} :: CreateGameSessionQueue)

-- | A set of policies that act as a sliding cap on player latency. FleetIQ
-- works to deliver low latency for most players in a game session. These
-- policies ensure that no individual player can be placed into a game with
-- unreasonably high latency. Use multiple policies to gradually relax
-- latency requirements a step at a time. Multiple policies are applied
-- based on their maximum allowed latency, starting with the lowest value.
createGameSessionQueue_playerLatencyPolicies :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe [PlayerLatencyPolicy])
createGameSessionQueue_playerLatencyPolicies = Lens.lens (\CreateGameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@CreateGameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: CreateGameSessionQueue) Prelude.. Lens.mapping Lens.coerced

-- | Custom settings to use when prioritizing destinations and locations for
-- game session placements. This configuration replaces the FleetIQ default
-- prioritization process. Priority types that are not explicitly named
-- will be automatically applied at the end of the prioritization process.
createGameSessionQueue_priorityConfiguration :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe PriorityConfiguration)
createGameSessionQueue_priorityConfiguration = Lens.lens (\CreateGameSessionQueue' {priorityConfiguration} -> priorityConfiguration) (\s@CreateGameSessionQueue' {} a -> s {priorityConfiguration = a} :: CreateGameSessionQueue)

-- | A list of labels to assign to the new game session queue resource. Tags
-- are developer-defined key-value pairs. Tagging Amazon Web Services
-- resources are useful for resource management, access management and cost
-- allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Reference/.
createGameSessionQueue_tags :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe [Tag])
createGameSessionQueue_tags = Lens.lens (\CreateGameSessionQueue' {tags} -> tags) (\s@CreateGameSessionQueue' {} a -> s {tags = a} :: CreateGameSessionQueue) Prelude.. Lens.mapping Lens.coerced

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
createGameSessionQueue_timeoutInSeconds :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe Prelude.Natural)
createGameSessionQueue_timeoutInSeconds = Lens.lens (\CreateGameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@CreateGameSessionQueue' {} a -> s {timeoutInSeconds = a} :: CreateGameSessionQueue)

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
createGameSessionQueue_name :: Lens.Lens' CreateGameSessionQueue Prelude.Text
createGameSessionQueue_name = Lens.lens (\CreateGameSessionQueue' {name} -> name) (\s@CreateGameSessionQueue' {} a -> s {name = a} :: CreateGameSessionQueue)

instance Core.AWSRequest CreateGameSessionQueue where
  type
    AWSResponse CreateGameSessionQueue =
      CreateGameSessionQueueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGameSessionQueueResponse'
            Prelude.<$> (x Data..?> "GameSessionQueue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGameSessionQueue where
  hashWithSalt _salt CreateGameSessionQueue' {..} =
    _salt
      `Prelude.hashWithSalt` customEventData
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` filterConfiguration
      `Prelude.hashWithSalt` notificationTarget
      `Prelude.hashWithSalt` playerLatencyPolicies
      `Prelude.hashWithSalt` priorityConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeoutInSeconds
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateGameSessionQueue where
  rnf CreateGameSessionQueue' {..} =
    Prelude.rnf customEventData
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf filterConfiguration
      `Prelude.seq` Prelude.rnf notificationTarget
      `Prelude.seq` Prelude.rnf playerLatencyPolicies
      `Prelude.seq` Prelude.rnf priorityConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeoutInSeconds
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateGameSessionQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.CreateGameSessionQueue" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGameSessionQueue where
  toJSON CreateGameSessionQueue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomEventData" Data..=)
              Prelude.<$> customEventData,
            ("Destinations" Data..=) Prelude.<$> destinations,
            ("FilterConfiguration" Data..=)
              Prelude.<$> filterConfiguration,
            ("NotificationTarget" Data..=)
              Prelude.<$> notificationTarget,
            ("PlayerLatencyPolicies" Data..=)
              Prelude.<$> playerLatencyPolicies,
            ("PriorityConfiguration" Data..=)
              Prelude.<$> priorityConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TimeoutInSeconds" Data..=)
              Prelude.<$> timeoutInSeconds,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateGameSessionQueue where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGameSessionQueue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGameSessionQueueResponse' smart constructor.
data CreateGameSessionQueueResponse = CreateGameSessionQueueResponse'
  { -- | An object that describes the newly created game session queue.
    gameSessionQueue :: Prelude.Maybe GameSessionQueue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGameSessionQueueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionQueue', 'createGameSessionQueueResponse_gameSessionQueue' - An object that describes the newly created game session queue.
--
-- 'httpStatus', 'createGameSessionQueueResponse_httpStatus' - The response's http status code.
newCreateGameSessionQueueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGameSessionQueueResponse
newCreateGameSessionQueueResponse pHttpStatus_ =
  CreateGameSessionQueueResponse'
    { gameSessionQueue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the newly created game session queue.
createGameSessionQueueResponse_gameSessionQueue :: Lens.Lens' CreateGameSessionQueueResponse (Prelude.Maybe GameSessionQueue)
createGameSessionQueueResponse_gameSessionQueue = Lens.lens (\CreateGameSessionQueueResponse' {gameSessionQueue} -> gameSessionQueue) (\s@CreateGameSessionQueueResponse' {} a -> s {gameSessionQueue = a} :: CreateGameSessionQueueResponse)

-- | The response's http status code.
createGameSessionQueueResponse_httpStatus :: Lens.Lens' CreateGameSessionQueueResponse Prelude.Int
createGameSessionQueueResponse_httpStatus = Lens.lens (\CreateGameSessionQueueResponse' {httpStatus} -> httpStatus) (\s@CreateGameSessionQueueResponse' {} a -> s {httpStatus = a} :: CreateGameSessionQueueResponse)

instance
  Prelude.NFData
    CreateGameSessionQueueResponse
  where
  rnf CreateGameSessionQueueResponse' {..} =
    Prelude.rnf gameSessionQueue
      `Prelude.seq` Prelude.rnf httpStatus
