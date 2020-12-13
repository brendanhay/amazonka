{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.UpdateGameSessionQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a game session queue, which determines how new game session requests in the queue are processed. To update settings, specify the queue name to be updated and provide the new settings. When updating destinations, provide a complete list of destinations.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Using Multi-Region Queues>
-- __Related operations__
--
--     * 'CreateGameSessionQueue'
--
--
--     * 'DescribeGameSessionQueues'
--
--
--     * 'UpdateGameSessionQueue'
--
--
--     * 'DeleteGameSessionQueue'
module Network.AWS.GameLift.UpdateGameSessionQueue
  ( -- * Creating a request
    UpdateGameSessionQueue (..),
    mkUpdateGameSessionQueue,

    -- ** Request lenses
    ugsqPlayerLatencyPolicies,
    ugsqTimeoutInSeconds,
    ugsqDestinations,
    ugsqName,

    -- * Destructuring the response
    UpdateGameSessionQueueResponse (..),
    mkUpdateGameSessionQueueResponse,

    -- ** Response lenses
    ugsqrsGameSessionQueue,
    ugsqrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkUpdateGameSessionQueue' smart constructor.
data UpdateGameSessionQueue = UpdateGameSessionQueue'
  { -- | A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. When updating policies, provide a complete collection of policies.
    playerLatencyPolicies :: Lude.Maybe [PlayerLatencyPolicy],
    -- | The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Lude.Maybe Lude.Natural,
    -- | A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order. When updating this list, provide a complete list of destinations.
    destinations :: Lude.Maybe [GameSessionQueueDestination],
    -- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region. You can use either the queue ID or ARN value.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGameSessionQueue' with the minimum fields required to make a request.
--
-- * 'playerLatencyPolicies' - A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. When updating policies, provide a complete collection of policies.
-- * 'timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
-- * 'destinations' - A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order. When updating this list, provide a complete list of destinations.
-- * 'name' - A descriptive label that is associated with game session queue. Queue names must be unique within each Region. You can use either the queue ID or ARN value.
mkUpdateGameSessionQueue ::
  -- | 'name'
  Lude.Text ->
  UpdateGameSessionQueue
mkUpdateGameSessionQueue pName_ =
  UpdateGameSessionQueue'
    { playerLatencyPolicies = Lude.Nothing,
      timeoutInSeconds = Lude.Nothing,
      destinations = Lude.Nothing,
      name = pName_
    }

-- | A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. When updating policies, provide a complete collection of policies.
--
-- /Note:/ Consider using 'playerLatencyPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsqPlayerLatencyPolicies :: Lens.Lens' UpdateGameSessionQueue (Lude.Maybe [PlayerLatencyPolicy])
ugsqPlayerLatencyPolicies = Lens.lens (playerLatencyPolicies :: UpdateGameSessionQueue -> Lude.Maybe [PlayerLatencyPolicy]) (\s a -> s {playerLatencyPolicies = a} :: UpdateGameSessionQueue)
{-# DEPRECATED ugsqPlayerLatencyPolicies "Use generic-lens or generic-optics with 'playerLatencyPolicies' instead." #-}

-- | The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsqTimeoutInSeconds :: Lens.Lens' UpdateGameSessionQueue (Lude.Maybe Lude.Natural)
ugsqTimeoutInSeconds = Lens.lens (timeoutInSeconds :: UpdateGameSessionQueue -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInSeconds = a} :: UpdateGameSessionQueue)
{-# DEPRECATED ugsqTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order. When updating this list, provide a complete list of destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsqDestinations :: Lens.Lens' UpdateGameSessionQueue (Lude.Maybe [GameSessionQueueDestination])
ugsqDestinations = Lens.lens (destinations :: UpdateGameSessionQueue -> Lude.Maybe [GameSessionQueueDestination]) (\s a -> s {destinations = a} :: UpdateGameSessionQueue)
{-# DEPRECATED ugsqDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region. You can use either the queue ID or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsqName :: Lens.Lens' UpdateGameSessionQueue Lude.Text
ugsqName = Lens.lens (name :: UpdateGameSessionQueue -> Lude.Text) (\s a -> s {name = a} :: UpdateGameSessionQueue)
{-# DEPRECATED ugsqName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateGameSessionQueue where
  type Rs UpdateGameSessionQueue = UpdateGameSessionQueueResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGameSessionQueueResponse'
            Lude.<$> (x Lude..?> "GameSessionQueue")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGameSessionQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.UpdateGameSessionQueue" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGameSessionQueue where
  toJSON UpdateGameSessionQueue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PlayerLatencyPolicies" Lude..=) Lude.<$> playerLatencyPolicies,
            ("TimeoutInSeconds" Lude..=) Lude.<$> timeoutInSeconds,
            ("Destinations" Lude..=) Lude.<$> destinations,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateGameSessionQueue where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGameSessionQueue where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkUpdateGameSessionQueueResponse' smart constructor.
data UpdateGameSessionQueueResponse = UpdateGameSessionQueueResponse'
  { -- | An object that describes the newly updated game session queue.
    gameSessionQueue :: Lude.Maybe GameSessionQueue,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGameSessionQueueResponse' with the minimum fields required to make a request.
--
-- * 'gameSessionQueue' - An object that describes the newly updated game session queue.
-- * 'responseStatus' - The response status code.
mkUpdateGameSessionQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGameSessionQueueResponse
mkUpdateGameSessionQueueResponse pResponseStatus_ =
  UpdateGameSessionQueueResponse'
    { gameSessionQueue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the newly updated game session queue.
--
-- /Note:/ Consider using 'gameSessionQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsqrsGameSessionQueue :: Lens.Lens' UpdateGameSessionQueueResponse (Lude.Maybe GameSessionQueue)
ugsqrsGameSessionQueue = Lens.lens (gameSessionQueue :: UpdateGameSessionQueueResponse -> Lude.Maybe GameSessionQueue) (\s a -> s {gameSessionQueue = a} :: UpdateGameSessionQueueResponse)
{-# DEPRECATED ugsqrsGameSessionQueue "Use generic-lens or generic-optics with 'gameSessionQueue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsqrsResponseStatus :: Lens.Lens' UpdateGameSessionQueueResponse Lude.Int
ugsqrsResponseStatus = Lens.lens (responseStatus :: UpdateGameSessionQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGameSessionQueueResponse)
{-# DEPRECATED ugsqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
