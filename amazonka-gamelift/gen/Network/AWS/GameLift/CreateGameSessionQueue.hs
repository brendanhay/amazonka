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
-- Module      : Network.AWS.GameLift.CreateGameSessionQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a new queue for processing requests to place new game
-- sessions. A queue identifies where new game sessions can be hosted -- by
-- specifying a list of destinations (fleets or aliases) -- and how long
-- requests can wait in the queue before timing out. You can set up a queue
-- to try to place game sessions on fleets in multiple Regions. To add
-- placement requests to a queue, call StartGameSessionPlacement and
-- reference the queue name.
--
-- __Destination order.__ When processing a request for a game session,
-- Amazon GameLift tries each destination in order until it finds one with
-- available resources to host the new game session. A queue\'s default
-- order is determined by how destinations are listed. The default order is
-- overridden when a game session placement request provides player latency
-- information. Player latency information enables Amazon GameLift to
-- prioritize destinations where players report the lowest average latency,
-- as a result placing the new game session where the majority of players
-- will have the best possible gameplay experience.
--
-- __Player latency policies.__ For placement requests containing player
-- latency information, use player latency policies to protect individual
-- players from very high latencies. With a latency cap, even when a
-- destination can deliver a low latency for most players, the game is not
-- placed where any individual player is reporting latency higher than a
-- policy\'s maximum. A queue can have multiple latency policies, which are
-- enforced consecutively starting with the policy with the lowest latency
-- cap. Use multiple policies to gradually relax latency controls; for
-- example, you might set a policy with a low latency cap for the first 60
-- seconds, a second policy with a higher cap for the next 60 seconds, etc.
--
-- To create a new queue, provide a name, timeout value, a list of
-- destinations and, if desired, a set of latency policies. If successful,
-- a new queue object is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-design.html Design a Game Session Queue>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-creating.html Create a Game Session Queue>
--
-- __Related operations__
--
-- -   CreateGameSessionQueue
--
-- -   DescribeGameSessionQueues
--
-- -   UpdateGameSessionQueue
--
-- -   DeleteGameSessionQueue
module Network.AWS.GameLift.CreateGameSessionQueue
  ( -- * Creating a Request
    CreateGameSessionQueue (..),
    newCreateGameSessionQueue,

    -- * Request Lenses
    createGameSessionQueue_playerLatencyPolicies,
    createGameSessionQueue_timeoutInSeconds,
    createGameSessionQueue_destinations,
    createGameSessionQueue_tags,
    createGameSessionQueue_name,

    -- * Destructuring the Response
    CreateGameSessionQueueResponse (..),
    newCreateGameSessionQueueResponse,

    -- * Response Lenses
    createGameSessionQueueResponse_gameSessionQueue,
    createGameSessionQueueResponse_httpStatus,
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
-- /See:/ 'newCreateGameSessionQueue' smart constructor.
data CreateGameSessionQueue = CreateGameSessionQueue'
  { -- | A collection of latency policies to apply when processing game sessions
    -- placement requests with player latency information. Multiple policies
    -- are evaluated in order of the maximum latency value, starting with the
    -- lowest latency values. With just one policy, the policy is enforced at
    -- the start of the game session placement for the duration period. With
    -- multiple policies, each policy is enforced consecutively for its
    -- duration period. For example, a queue might enforce a 60-second policy
    -- followed by a 120-second policy, and then no policy for the remainder of
    -- the placement. A player latency policy must set a value for
    -- @MaximumIndividualPlayerLatencyMilliseconds@. If none is set, this API
    -- request fails.
    playerLatencyPolicies :: Prelude.Maybe [PlayerLatencyPolicy],
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A list of fleets that can be used to fulfill game session placement
    -- requests in the queue. Fleets are identified by either a fleet ARN or a
    -- fleet alias ARN. Destinations are listed in default preference order.
    destinations :: Prelude.Maybe [GameSessionQueueDestination],
    -- | A list of labels to assign to the new game session queue resource. Tags
    -- are developer-defined key-value pairs. Tagging AWS resources are useful
    -- for resource management, access management and cost allocation. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
    -- in the /AWS General Reference/. Once the resource is created, you can
    -- use TagResource, UntagResource, and ListTagsForResource to add, remove,
    -- and view tags. The maximum tag limit may be lower than stated. See the
    -- AWS General Reference for actual tagging limits.
    tags :: Prelude.Maybe [Tag],
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
-- 'playerLatencyPolicies', 'createGameSessionQueue_playerLatencyPolicies' - A collection of latency policies to apply when processing game sessions
-- placement requests with player latency information. Multiple policies
-- are evaluated in order of the maximum latency value, starting with the
-- lowest latency values. With just one policy, the policy is enforced at
-- the start of the game session placement for the duration period. With
-- multiple policies, each policy is enforced consecutively for its
-- duration period. For example, a queue might enforce a 60-second policy
-- followed by a 120-second policy, and then no policy for the remainder of
-- the placement. A player latency policy must set a value for
-- @MaximumIndividualPlayerLatencyMilliseconds@. If none is set, this API
-- request fails.
--
-- 'timeoutInSeconds', 'createGameSessionQueue_timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
--
-- 'destinations', 'createGameSessionQueue_destinations' - A list of fleets that can be used to fulfill game session placement
-- requests in the queue. Fleets are identified by either a fleet ARN or a
-- fleet alias ARN. Destinations are listed in default preference order.
--
-- 'tags', 'createGameSessionQueue_tags' - A list of labels to assign to the new game session queue resource. Tags
-- are developer-defined key-value pairs. Tagging AWS resources are useful
-- for resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
--
-- 'name', 'createGameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
newCreateGameSessionQueue ::
  -- | 'name'
  Prelude.Text ->
  CreateGameSessionQueue
newCreateGameSessionQueue pName_ =
  CreateGameSessionQueue'
    { playerLatencyPolicies =
        Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      destinations = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A collection of latency policies to apply when processing game sessions
-- placement requests with player latency information. Multiple policies
-- are evaluated in order of the maximum latency value, starting with the
-- lowest latency values. With just one policy, the policy is enforced at
-- the start of the game session placement for the duration period. With
-- multiple policies, each policy is enforced consecutively for its
-- duration period. For example, a queue might enforce a 60-second policy
-- followed by a 120-second policy, and then no policy for the remainder of
-- the placement. A player latency policy must set a value for
-- @MaximumIndividualPlayerLatencyMilliseconds@. If none is set, this API
-- request fails.
createGameSessionQueue_playerLatencyPolicies :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe [PlayerLatencyPolicy])
createGameSessionQueue_playerLatencyPolicies = Lens.lens (\CreateGameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@CreateGameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: CreateGameSessionQueue) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
createGameSessionQueue_timeoutInSeconds :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe Prelude.Natural)
createGameSessionQueue_timeoutInSeconds = Lens.lens (\CreateGameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@CreateGameSessionQueue' {} a -> s {timeoutInSeconds = a} :: CreateGameSessionQueue)

-- | A list of fleets that can be used to fulfill game session placement
-- requests in the queue. Fleets are identified by either a fleet ARN or a
-- fleet alias ARN. Destinations are listed in default preference order.
createGameSessionQueue_destinations :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe [GameSessionQueueDestination])
createGameSessionQueue_destinations = Lens.lens (\CreateGameSessionQueue' {destinations} -> destinations) (\s@CreateGameSessionQueue' {} a -> s {destinations = a} :: CreateGameSessionQueue) Prelude.. Lens.mapping Lens._Coerce

-- | A list of labels to assign to the new game session queue resource. Tags
-- are developer-defined key-value pairs. Tagging AWS resources are useful
-- for resource management, access management and cost allocation. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>
-- in the /AWS General Reference/. Once the resource is created, you can
-- use TagResource, UntagResource, and ListTagsForResource to add, remove,
-- and view tags. The maximum tag limit may be lower than stated. See the
-- AWS General Reference for actual tagging limits.
createGameSessionQueue_tags :: Lens.Lens' CreateGameSessionQueue (Prelude.Maybe [Tag])
createGameSessionQueue_tags = Lens.lens (\CreateGameSessionQueue' {tags} -> tags) (\s@CreateGameSessionQueue' {} a -> s {tags = a} :: CreateGameSessionQueue) Prelude.. Lens.mapping Lens._Coerce

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
createGameSessionQueue_name :: Lens.Lens' CreateGameSessionQueue Prelude.Text
createGameSessionQueue_name = Lens.lens (\CreateGameSessionQueue' {name} -> name) (\s@CreateGameSessionQueue' {} a -> s {name = a} :: CreateGameSessionQueue)

instance Core.AWSRequest CreateGameSessionQueue where
  type
    AWSResponse CreateGameSessionQueue =
      CreateGameSessionQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGameSessionQueueResponse'
            Prelude.<$> (x Core..?> "GameSessionQueue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGameSessionQueue

instance Prelude.NFData CreateGameSessionQueue

instance Core.ToHeaders CreateGameSessionQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.CreateGameSessionQueue" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateGameSessionQueue where
  toJSON CreateGameSessionQueue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PlayerLatencyPolicies" Core..=)
              Prelude.<$> playerLatencyPolicies,
            ("TimeoutInSeconds" Core..=)
              Prelude.<$> timeoutInSeconds,
            ("Destinations" Core..=) Prelude.<$> destinations,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateGameSessionQueue where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateGameSessionQueue where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newCreateGameSessionQueueResponse' smart constructor.
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
