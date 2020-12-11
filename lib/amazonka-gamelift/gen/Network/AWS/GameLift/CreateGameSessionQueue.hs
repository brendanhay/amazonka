{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateGameSessionQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a new queue for processing requests to place new game sessions. A queue identifies where new game sessions can be hosted -- by specifying a list of destinations (fleets or aliases) -- and how long requests can wait in the queue before timing out. You can set up a queue to try to place game sessions on fleets in multiple Regions. To add placement requests to a queue, call 'StartGameSessionPlacement' and reference the queue name.
--
-- __Destination order.__ When processing a request for a game session, Amazon GameLift tries each destination in order until it finds one with available resources to host the new game session. A queue's default order is determined by how destinations are listed. The default order is overridden when a game session placement request provides player latency information. Player latency information enables Amazon GameLift to prioritize destinations where players report the lowest average latency, as a result placing the new game session where the majority of players will have the best possible gameplay experience.
-- __Player latency policies.__ For placement requests containing player latency information, use player latency policies to protect individual players from very high latencies. With a latency cap, even when a destination can deliver a low latency for most players, the game is not placed where any individual player is reporting latency higher than a policy's maximum. A queue can have multiple latency policies, which are enforced consecutively starting with the policy with the lowest latency cap. Use multiple policies to gradually relax latency controls; for example, you might set a policy with a low latency cap for the first 60 seconds, a second policy with a higher cap for the next 60 seconds, etc.
-- To create a new queue, provide a name, timeout value, a list of destinations and, if desired, a set of latency policies. If successful, a new queue object is returned.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-design.html Design a Game Session Queue>
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-creating.html Create a Game Session Queue>
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
module Network.AWS.GameLift.CreateGameSessionQueue
  ( -- * Creating a request
    CreateGameSessionQueue (..),
    mkCreateGameSessionQueue,

    -- ** Request lenses
    cgsqPlayerLatencyPolicies,
    cgsqTimeoutInSeconds,
    cgsqDestinations,
    cgsqTags,
    cgsqName,

    -- * Destructuring the response
    CreateGameSessionQueueResponse (..),
    mkCreateGameSessionQueueResponse,

    -- ** Response lenses
    cgsqrsGameSessionQueue,
    cgsqrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateGameSessionQueue' smart constructor.
data CreateGameSessionQueue = CreateGameSessionQueue'
  { playerLatencyPolicies ::
      Lude.Maybe [PlayerLatencyPolicy],
    timeoutInSeconds :: Lude.Maybe Lude.Natural,
    destinations ::
      Lude.Maybe [GameSessionQueueDestination],
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGameSessionQueue' with the minimum fields required to make a request.
--
-- * 'destinations' - A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
-- * 'name' - A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
-- * 'playerLatencyPolicies' - A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. A player latency policy must set a value for @MaximumIndividualPlayerLatencyMilliseconds@ . If none is set, this API request fails.
-- * 'tags' - A list of labels to assign to the new game session queue resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
-- * 'timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
mkCreateGameSessionQueue ::
  -- | 'name'
  Lude.Text ->
  CreateGameSessionQueue
mkCreateGameSessionQueue pName_ =
  CreateGameSessionQueue'
    { playerLatencyPolicies = Lude.Nothing,
      timeoutInSeconds = Lude.Nothing,
      destinations = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

-- | A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. A player latency policy must set a value for @MaximumIndividualPlayerLatencyMilliseconds@ . If none is set, this API request fails.
--
-- /Note:/ Consider using 'playerLatencyPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqPlayerLatencyPolicies :: Lens.Lens' CreateGameSessionQueue (Lude.Maybe [PlayerLatencyPolicy])
cgsqPlayerLatencyPolicies = Lens.lens (playerLatencyPolicies :: CreateGameSessionQueue -> Lude.Maybe [PlayerLatencyPolicy]) (\s a -> s {playerLatencyPolicies = a} :: CreateGameSessionQueue)
{-# DEPRECATED cgsqPlayerLatencyPolicies "Use generic-lens or generic-optics with 'playerLatencyPolicies' instead." #-}

-- | The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqTimeoutInSeconds :: Lens.Lens' CreateGameSessionQueue (Lude.Maybe Lude.Natural)
cgsqTimeoutInSeconds = Lens.lens (timeoutInSeconds :: CreateGameSessionQueue -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInSeconds = a} :: CreateGameSessionQueue)
{-# DEPRECATED cgsqTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqDestinations :: Lens.Lens' CreateGameSessionQueue (Lude.Maybe [GameSessionQueueDestination])
cgsqDestinations = Lens.lens (destinations :: CreateGameSessionQueue -> Lude.Maybe [GameSessionQueueDestination]) (\s a -> s {destinations = a} :: CreateGameSessionQueue)
{-# DEPRECATED cgsqDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | A list of labels to assign to the new game session queue resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqTags :: Lens.Lens' CreateGameSessionQueue (Lude.Maybe [Tag])
cgsqTags = Lens.lens (tags :: CreateGameSessionQueue -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateGameSessionQueue)
{-# DEPRECATED cgsqTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqName :: Lens.Lens' CreateGameSessionQueue Lude.Text
cgsqName = Lens.lens (name :: CreateGameSessionQueue -> Lude.Text) (\s a -> s {name = a} :: CreateGameSessionQueue)
{-# DEPRECATED cgsqName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateGameSessionQueue where
  type Rs CreateGameSessionQueue = CreateGameSessionQueueResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGameSessionQueueResponse'
            Lude.<$> (x Lude..?> "GameSessionQueue")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGameSessionQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.CreateGameSessionQueue" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGameSessionQueue where
  toJSON CreateGameSessionQueue' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PlayerLatencyPolicies" Lude..=) Lude.<$> playerLatencyPolicies,
            ("TimeoutInSeconds" Lude..=) Lude.<$> timeoutInSeconds,
            ("Destinations" Lude..=) Lude.<$> destinations,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateGameSessionQueue where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGameSessionQueue where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateGameSessionQueueResponse' smart constructor.
data CreateGameSessionQueueResponse = CreateGameSessionQueueResponse'
  { gameSessionQueue ::
      Lude.Maybe GameSessionQueue,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGameSessionQueueResponse' with the minimum fields required to make a request.
--
-- * 'gameSessionQueue' - An object that describes the newly created game session queue.
-- * 'responseStatus' - The response status code.
mkCreateGameSessionQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGameSessionQueueResponse
mkCreateGameSessionQueueResponse pResponseStatus_ =
  CreateGameSessionQueueResponse'
    { gameSessionQueue = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the newly created game session queue.
--
-- /Note:/ Consider using 'gameSessionQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqrsGameSessionQueue :: Lens.Lens' CreateGameSessionQueueResponse (Lude.Maybe GameSessionQueue)
cgsqrsGameSessionQueue = Lens.lens (gameSessionQueue :: CreateGameSessionQueueResponse -> Lude.Maybe GameSessionQueue) (\s a -> s {gameSessionQueue = a} :: CreateGameSessionQueueResponse)
{-# DEPRECATED cgsqrsGameSessionQueue "Use generic-lens or generic-optics with 'gameSessionQueue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqrsResponseStatus :: Lens.Lens' CreateGameSessionQueueResponse Lude.Int
cgsqrsResponseStatus = Lens.lens (responseStatus :: CreateGameSessionQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGameSessionQueueResponse)
{-# DEPRECATED cgsqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
