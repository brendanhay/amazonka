{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    playerLatencyPolicies :: Prelude.Maybe [PlayerLatencyPolicy],
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A list of fleets that can be used to fulfill game session placement
    -- requests in the queue. Fleets are identified by either a fleet ARN or a
    -- fleet alias ARN. Destinations are listed in default preference order.
    -- When updating this list, provide a complete list of destinations.
    destinations :: Prelude.Maybe [GameSessionQueueDestination],
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region. You can use either the queue ID
    -- or ARN value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateGameSessionQueue
newUpdateGameSessionQueue pName_ =
  UpdateGameSessionQueue'
    { playerLatencyPolicies =
        Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      destinations = Prelude.Nothing,
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
updateGameSessionQueue_playerLatencyPolicies :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe [PlayerLatencyPolicy])
updateGameSessionQueue_playerLatencyPolicies = Lens.lens (\UpdateGameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@UpdateGameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: UpdateGameSessionQueue) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
updateGameSessionQueue_timeoutInSeconds :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe Prelude.Natural)
updateGameSessionQueue_timeoutInSeconds = Lens.lens (\UpdateGameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@UpdateGameSessionQueue' {} a -> s {timeoutInSeconds = a} :: UpdateGameSessionQueue)

-- | A list of fleets that can be used to fulfill game session placement
-- requests in the queue. Fleets are identified by either a fleet ARN or a
-- fleet alias ARN. Destinations are listed in default preference order.
-- When updating this list, provide a complete list of destinations.
updateGameSessionQueue_destinations :: Lens.Lens' UpdateGameSessionQueue (Prelude.Maybe [GameSessionQueueDestination])
updateGameSessionQueue_destinations = Lens.lens (\UpdateGameSessionQueue' {destinations} -> destinations) (\s@UpdateGameSessionQueue' {} a -> s {destinations = a} :: UpdateGameSessionQueue) Prelude.. Lens.mapping Prelude._Coerce

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region. You can use either the queue ID
-- or ARN value.
updateGameSessionQueue_name :: Lens.Lens' UpdateGameSessionQueue Prelude.Text
updateGameSessionQueue_name = Lens.lens (\UpdateGameSessionQueue' {name} -> name) (\s@UpdateGameSessionQueue' {} a -> s {name = a} :: UpdateGameSessionQueue)

instance Prelude.AWSRequest UpdateGameSessionQueue where
  type
    Rs UpdateGameSessionQueue =
      UpdateGameSessionQueueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameSessionQueueResponse'
            Prelude.<$> (x Prelude..?> "GameSessionQueue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGameSessionQueue

instance Prelude.NFData UpdateGameSessionQueue

instance Prelude.ToHeaders UpdateGameSessionQueue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "GameLift.UpdateGameSessionQueue" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateGameSessionQueue where
  toJSON UpdateGameSessionQueue' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PlayerLatencyPolicies" Prelude..=)
              Prelude.<$> playerLatencyPolicies,
            ("TimeoutInSeconds" Prelude..=)
              Prelude.<$> timeoutInSeconds,
            ("Destinations" Prelude..=) Prelude.<$> destinations,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath UpdateGameSessionQueue where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateGameSessionQueue where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
