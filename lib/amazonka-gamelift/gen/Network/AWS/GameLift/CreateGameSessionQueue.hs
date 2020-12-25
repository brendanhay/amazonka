{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cgsqName,
    cgsqDestinations,
    cgsqPlayerLatencyPolicies,
    cgsqTags,
    cgsqTimeoutInSeconds,

    -- * Destructuring the response
    CreateGameSessionQueueResponse (..),
    mkCreateGameSessionQueueResponse,

    -- ** Response lenses
    cgsqrrsGameSessionQueue,
    cgsqrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateGameSessionQueue' smart constructor.
data CreateGameSessionQueue = CreateGameSessionQueue'
  { -- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
    name :: Types.Name,
    -- | A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
    destinations :: Core.Maybe [Types.GameSessionQueueDestination],
    -- | A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. A player latency policy must set a value for @MaximumIndividualPlayerLatencyMilliseconds@ . If none is set, this API request fails.
    playerLatencyPolicies :: Core.Maybe [Types.PlayerLatencyPolicy],
    -- | A list of labels to assign to the new game session queue resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
    tags :: Core.Maybe [Types.Tag],
    -- | The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGameSessionQueue' value with any optional fields omitted.
mkCreateGameSessionQueue ::
  -- | 'name'
  Types.Name ->
  CreateGameSessionQueue
mkCreateGameSessionQueue name =
  CreateGameSessionQueue'
    { name,
      destinations = Core.Nothing,
      playerLatencyPolicies = Core.Nothing,
      tags = Core.Nothing,
      timeoutInSeconds = Core.Nothing
    }

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqName :: Lens.Lens' CreateGameSessionQueue Types.Name
cgsqName = Lens.field @"name"
{-# DEPRECATED cgsqName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqDestinations :: Lens.Lens' CreateGameSessionQueue (Core.Maybe [Types.GameSessionQueueDestination])
cgsqDestinations = Lens.field @"destinations"
{-# DEPRECATED cgsqDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. A player latency policy must set a value for @MaximumIndividualPlayerLatencyMilliseconds@ . If none is set, this API request fails.
--
-- /Note:/ Consider using 'playerLatencyPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqPlayerLatencyPolicies :: Lens.Lens' CreateGameSessionQueue (Core.Maybe [Types.PlayerLatencyPolicy])
cgsqPlayerLatencyPolicies = Lens.field @"playerLatencyPolicies"
{-# DEPRECATED cgsqPlayerLatencyPolicies "Use generic-lens or generic-optics with 'playerLatencyPolicies' instead." #-}

-- | A list of labels to assign to the new game session queue resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqTags :: Lens.Lens' CreateGameSessionQueue (Core.Maybe [Types.Tag])
cgsqTags = Lens.field @"tags"
{-# DEPRECATED cgsqTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqTimeoutInSeconds :: Lens.Lens' CreateGameSessionQueue (Core.Maybe Core.Natural)
cgsqTimeoutInSeconds = Lens.field @"timeoutInSeconds"
{-# DEPRECATED cgsqTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

instance Core.FromJSON CreateGameSessionQueue where
  toJSON CreateGameSessionQueue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Destinations" Core..=) Core.<$> destinations,
            ("PlayerLatencyPolicies" Core..=) Core.<$> playerLatencyPolicies,
            ("Tags" Core..=) Core.<$> tags,
            ("TimeoutInSeconds" Core..=) Core.<$> timeoutInSeconds
          ]
      )

instance Core.AWSRequest CreateGameSessionQueue where
  type Rs CreateGameSessionQueue = CreateGameSessionQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.CreateGameSessionQueue")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGameSessionQueueResponse'
            Core.<$> (x Core..:? "GameSessionQueue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateGameSessionQueueResponse' smart constructor.
data CreateGameSessionQueueResponse = CreateGameSessionQueueResponse'
  { -- | An object that describes the newly created game session queue.
    gameSessionQueue :: Core.Maybe Types.GameSessionQueue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGameSessionQueueResponse' value with any optional fields omitted.
mkCreateGameSessionQueueResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGameSessionQueueResponse
mkCreateGameSessionQueueResponse responseStatus =
  CreateGameSessionQueueResponse'
    { gameSessionQueue = Core.Nothing,
      responseStatus
    }

-- | An object that describes the newly created game session queue.
--
-- /Note:/ Consider using 'gameSessionQueue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqrrsGameSessionQueue :: Lens.Lens' CreateGameSessionQueueResponse (Core.Maybe Types.GameSessionQueue)
cgsqrrsGameSessionQueue = Lens.field @"gameSessionQueue"
{-# DEPRECATED cgsqrrsGameSessionQueue "Use generic-lens or generic-optics with 'gameSessionQueue' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsqrrsResponseStatus :: Lens.Lens' CreateGameSessionQueueResponse Core.Int
cgsqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgsqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
