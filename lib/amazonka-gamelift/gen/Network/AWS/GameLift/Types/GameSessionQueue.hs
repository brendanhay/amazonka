{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameSessionQueue
  ( GameSessionQueue (..)
  -- * Smart constructor
  , mkGameSessionQueue
  -- * Lenses
  , gsqDestinations
  , gsqGameSessionQueueArn
  , gsqName
  , gsqPlayerLatencyPolicies
  , gsqTimeoutInSeconds
  ) where

import qualified Network.AWS.GameLift.Types.GameSessionQueueArn as Types
import qualified Network.AWS.GameLift.Types.GameSessionQueueDestination as Types
import qualified Network.AWS.GameLift.Types.GameSessionQueueName as Types
import qualified Network.AWS.GameLift.Types.PlayerLatencyPolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration of a queue that is used to process game session placement requests. The queue configuration identifies several game features:
--
--
--     * The destinations where a new game session can potentially be hosted. Amazon GameLift tries these destinations in an order based on either the queue's default order or player latency information, if provided in a placement request. With latency information, Amazon GameLift can place game sessions where the majority of players are reporting the lowest possible latency. 
--
--
--     * The length of time that placement requests can wait in the queue before timing out. 
--
--
--     * A set of optional latency policies that protect individual players from high latencies, preventing game sessions from being placed where any individual player is reporting latency higher than a policy's maximum.
--
--
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
--
--
--
-- /See:/ 'mkGameSessionQueue' smart constructor.
data GameSessionQueue = GameSessionQueue'
  { destinations :: Core.Maybe [Types.GameSessionQueueDestination]
    -- ^ A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
  , gameSessionQueueArn :: Core.Maybe Types.GameSessionQueueArn
    -- ^ Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift game session queue ARN, the resource ID matches the /Name/ value.
  , name :: Core.Maybe Types.GameSessionQueueName
    -- ^ A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
  , playerLatencyPolicies :: Core.Maybe [Types.PlayerLatencyPolicy]
    -- ^ A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. 
  , timeoutInSeconds :: Core.Maybe Core.Natural
    -- ^ The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GameSessionQueue' value with any optional fields omitted.
mkGameSessionQueue
    :: GameSessionQueue
mkGameSessionQueue
  = GameSessionQueue'{destinations = Core.Nothing,
                      gameSessionQueueArn = Core.Nothing, name = Core.Nothing,
                      playerLatencyPolicies = Core.Nothing,
                      timeoutInSeconds = Core.Nothing}

-- | A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqDestinations :: Lens.Lens' GameSessionQueue (Core.Maybe [Types.GameSessionQueueDestination])
gsqDestinations = Lens.field @"destinations"
{-# INLINEABLE gsqDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift game session queue ARN, the resource ID matches the /Name/ value.
--
-- /Note:/ Consider using 'gameSessionQueueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqGameSessionQueueArn :: Lens.Lens' GameSessionQueue (Core.Maybe Types.GameSessionQueueArn)
gsqGameSessionQueueArn = Lens.field @"gameSessionQueueArn"
{-# INLINEABLE gsqGameSessionQueueArn #-}
{-# DEPRECATED gameSessionQueueArn "Use generic-lens or generic-optics with 'gameSessionQueueArn' instead"  #-}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqName :: Lens.Lens' GameSessionQueue (Core.Maybe Types.GameSessionQueueName)
gsqName = Lens.field @"name"
{-# INLINEABLE gsqName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement. 
--
-- /Note:/ Consider using 'playerLatencyPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqPlayerLatencyPolicies :: Lens.Lens' GameSessionQueue (Core.Maybe [Types.PlayerLatencyPolicy])
gsqPlayerLatencyPolicies = Lens.field @"playerLatencyPolicies"
{-# INLINEABLE gsqPlayerLatencyPolicies #-}
{-# DEPRECATED playerLatencyPolicies "Use generic-lens or generic-optics with 'playerLatencyPolicies' instead"  #-}

-- | The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqTimeoutInSeconds :: Lens.Lens' GameSessionQueue (Core.Maybe Core.Natural)
gsqTimeoutInSeconds = Lens.field @"timeoutInSeconds"
{-# INLINEABLE gsqTimeoutInSeconds #-}
{-# DEPRECATED timeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead"  #-}

instance Core.FromJSON GameSessionQueue where
        parseJSON
          = Core.withObject "GameSessionQueue" Core.$
              \ x ->
                GameSessionQueue' Core.<$>
                  (x Core..:? "Destinations") Core.<*>
                    x Core..:? "GameSessionQueueArn"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "PlayerLatencyPolicies"
                    Core.<*> x Core..:? "TimeoutInSeconds"
