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
-- Module      : Network.AWS.GameLift.UpdateGameSessionQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a game session queue, which determines how new game
-- session requests in the queue are processed. To update settings, specify
-- the queue name to be updated and provide the new settings. When updating
-- destinations, provide a complete list of destinations.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Using Multi-Region Queues>
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
module Network.AWS.GameLift.UpdateGameSessionQueue
  ( -- * Creating a Request
    UpdateGameSessionQueue (..),
    newUpdateGameSessionQueue,

    -- * Request Lenses
    updateGameSessionQueue_playerLatencyPolicies,
    updateGameSessionQueue_timeoutInSeconds,
    updateGameSessionQueue_destinations,
    updateGameSessionQueue_name,

    -- * Destructuring the Response
    UpdateGameSessionQueueResponse (..),
    newUpdateGameSessionQueueResponse,

    -- * Response Lenses
    updateGameSessionQueueResponse_gameSessionQueue,
    updateGameSessionQueueResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateGameSessionQueue' smart constructor.
data UpdateGameSessionQueue = UpdateGameSessionQueue'
  { -- | A collection of latency policies to apply when processing game sessions
    -- placement requests with player latency information. Multiple policies
    -- are evaluated in order of the maximum latency value, starting with the
    -- lowest latency values. With just one policy, the policy is enforced at
    -- the start of the game session placement for the duration period. With
    -- multiple policies, each policy is enforced consecutively for its
    -- duration period. For example, a queue might enforce a 60-second policy
    -- followed by a 120-second policy, and then no policy for the remainder of
    -- the placement. When updating policies, provide a complete collection of
    -- policies.
    playerLatencyPolicies :: Core.Maybe [PlayerLatencyPolicy],
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Core.Maybe Core.Natural,
    -- | A list of fleets that can be used to fulfill game session placement
    -- requests in the queue. Fleets are identified by either a fleet ARN or a
    -- fleet alias ARN. Destinations are listed in default preference order.
    -- When updating this list, provide a complete list of destinations.
    destinations :: Core.Maybe [GameSessionQueueDestination],
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region. You can use either the queue ID
    -- or ARN value.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGameSessionQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerLatencyPolicies', 'updateGameSessionQueue_playerLatencyPolicies' - A collection of latency policies to apply when processing game sessions
-- placement requests with player latency information. Multiple policies
-- are evaluated in order of the maximum latency value, starting with the
-- lowest latency values. With just one policy, the policy is enforced at
-- the start of the game session placement for the duration period. With
-- multiple policies, each policy is enforced consecutively for its
-- duration period. For example, a queue might enforce a 60-second policy
-- followed by a 120-second policy, and then no policy for the remainder of
-- the placement. When updating policies, provide a complete collection of
-- policies.
--
-- 'timeoutInSeconds', 'updateGameSessionQueue_timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
--
-- 'destinations', 'updateGameSessionQueue_destinations' - A list of fleets that can be used to fulfill game session placement
-- requests in the queue. Fleets are identified by either a fleet ARN or a
-- fleet alias ARN. Destinations are listed in default preference order.
-- When updating this list, provide a complete list of destinations.
--
-- 'name', 'updateGameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
newUpdateGameSessionQueue ::
  -- | 'name'
  Core.Text ->
  UpdateGameSessionQueue
newUpdateGameSessionQueue pName_ =
  UpdateGameSessionQueue'
    { playerLatencyPolicies =
        Core.Nothing,
      timeoutInSeconds = Core.Nothing,
      destinations = Core.Nothing,
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
-- the placement. When updating policies, provide a complete collection of
-- policies.
updateGameSessionQueue_playerLatencyPolicies :: Lens.Lens' UpdateGameSessionQueue (Core.Maybe [PlayerLatencyPolicy])
updateGameSessionQueue_playerLatencyPolicies = Lens.lens (\UpdateGameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@UpdateGameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: UpdateGameSessionQueue) Core.. Lens.mapping Lens._Coerce

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
updateGameSessionQueue_timeoutInSeconds :: Lens.Lens' UpdateGameSessionQueue (Core.Maybe Core.Natural)
updateGameSessionQueue_timeoutInSeconds = Lens.lens (\UpdateGameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@UpdateGameSessionQueue' {} a -> s {timeoutInSeconds = a} :: UpdateGameSessionQueue)

-- | A list of fleets that can be used to fulfill game session placement
-- requests in the queue. Fleets are identified by either a fleet ARN or a
-- fleet alias ARN. Destinations are listed in default preference order.
-- When updating this list, provide a complete list of destinations.
updateGameSessionQueue_destinations :: Lens.Lens' UpdateGameSessionQueue (Core.Maybe [GameSessionQueueDestination])
updateGameSessionQueue_destinations = Lens.lens (\UpdateGameSessionQueue' {destinations} -> destinations) (\s@UpdateGameSessionQueue' {} a -> s {destinations = a} :: UpdateGameSessionQueue) Core.. Lens.mapping Lens._Coerce

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
updateGameSessionQueue_name :: Lens.Lens' UpdateGameSessionQueue Core.Text
updateGameSessionQueue_name = Lens.lens (\UpdateGameSessionQueue' {name} -> name) (\s@UpdateGameSessionQueue' {} a -> s {name = a} :: UpdateGameSessionQueue)

instance Core.AWSRequest UpdateGameSessionQueue where
  type
    AWSResponse UpdateGameSessionQueue =
      UpdateGameSessionQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameSessionQueueResponse'
            Core.<$> (x Core..?> "GameSessionQueue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGameSessionQueue

instance Core.NFData UpdateGameSessionQueue

instance Core.ToHeaders UpdateGameSessionQueue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.UpdateGameSessionQueue" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGameSessionQueue where
  toJSON UpdateGameSessionQueue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PlayerLatencyPolicies" Core..=)
              Core.<$> playerLatencyPolicies,
            ("TimeoutInSeconds" Core..=)
              Core.<$> timeoutInSeconds,
            ("Destinations" Core..=) Core.<$> destinations,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateGameSessionQueue where
  toPath = Core.const "/"

instance Core.ToQuery UpdateGameSessionQueue where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateGameSessionQueueResponse' smart constructor.
data UpdateGameSessionQueueResponse = UpdateGameSessionQueueResponse'
  { -- | An object that describes the newly updated game session queue.
    gameSessionQueue :: Core.Maybe GameSessionQueue,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateGameSessionQueueResponse
newUpdateGameSessionQueueResponse pHttpStatus_ =
  UpdateGameSessionQueueResponse'
    { gameSessionQueue =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the newly updated game session queue.
updateGameSessionQueueResponse_gameSessionQueue :: Lens.Lens' UpdateGameSessionQueueResponse (Core.Maybe GameSessionQueue)
updateGameSessionQueueResponse_gameSessionQueue = Lens.lens (\UpdateGameSessionQueueResponse' {gameSessionQueue} -> gameSessionQueue) (\s@UpdateGameSessionQueueResponse' {} a -> s {gameSessionQueue = a} :: UpdateGameSessionQueueResponse)

-- | The response's http status code.
updateGameSessionQueueResponse_httpStatus :: Lens.Lens' UpdateGameSessionQueueResponse Core.Int
updateGameSessionQueueResponse_httpStatus = Lens.lens (\UpdateGameSessionQueueResponse' {httpStatus} -> httpStatus) (\s@UpdateGameSessionQueueResponse' {} a -> s {httpStatus = a} :: UpdateGameSessionQueueResponse)

instance Core.NFData UpdateGameSessionQueueResponse
