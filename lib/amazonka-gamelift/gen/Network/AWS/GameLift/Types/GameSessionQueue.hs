{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionQueue
  ( GameSessionQueue (..),

    -- * Smart constructor
    mkGameSessionQueue,

    -- * Lenses
    gsqGameSessionQueueARN,
    gsqPlayerLatencyPolicies,
    gsqTimeoutInSeconds,
    gsqDestinations,
    gsqName,
  )
where

import Network.AWS.GameLift.Types.GameSessionQueueDestination
import Network.AWS.GameLift.Types.PlayerLatencyPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { gameSessionQueueARN ::
      Lude.Maybe Lude.Text,
    playerLatencyPolicies :: Lude.Maybe [PlayerLatencyPolicy],
    timeoutInSeconds :: Lude.Maybe Lude.Natural,
    destinations :: Lude.Maybe [GameSessionQueueDestination],
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GameSessionQueue' with the minimum fields required to make a request.
--
-- * 'destinations' - A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
-- * 'gameSessionQueueARN' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift game session queue ARN, the resource ID matches the /Name/ value.
-- * 'name' - A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
-- * 'playerLatencyPolicies' - A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement.
-- * 'timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
mkGameSessionQueue ::
  GameSessionQueue
mkGameSessionQueue =
  GameSessionQueue'
    { gameSessionQueueARN = Lude.Nothing,
      playerLatencyPolicies = Lude.Nothing,
      timeoutInSeconds = Lude.Nothing,
      destinations = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. In a GameLift game session queue ARN, the resource ID matches the /Name/ value.
--
-- /Note:/ Consider using 'gameSessionQueueARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqGameSessionQueueARN :: Lens.Lens' GameSessionQueue (Lude.Maybe Lude.Text)
gsqGameSessionQueueARN = Lens.lens (gameSessionQueueARN :: GameSessionQueue -> Lude.Maybe Lude.Text) (\s a -> s {gameSessionQueueARN = a} :: GameSessionQueue)
{-# DEPRECATED gsqGameSessionQueueARN "Use generic-lens or generic-optics with 'gameSessionQueueARN' instead." #-}

-- | A collection of latency policies to apply when processing game sessions placement requests with player latency information. Multiple policies are evaluated in order of the maximum latency value, starting with the lowest latency values. With just one policy, the policy is enforced at the start of the game session placement for the duration period. With multiple policies, each policy is enforced consecutively for its duration period. For example, a queue might enforce a 60-second policy followed by a 120-second policy, and then no policy for the remainder of the placement.
--
-- /Note:/ Consider using 'playerLatencyPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqPlayerLatencyPolicies :: Lens.Lens' GameSessionQueue (Lude.Maybe [PlayerLatencyPolicy])
gsqPlayerLatencyPolicies = Lens.lens (playerLatencyPolicies :: GameSessionQueue -> Lude.Maybe [PlayerLatencyPolicy]) (\s a -> s {playerLatencyPolicies = a} :: GameSessionQueue)
{-# DEPRECATED gsqPlayerLatencyPolicies "Use generic-lens or generic-optics with 'playerLatencyPolicies' instead." #-}

-- | The maximum time, in seconds, that a new game session placement request remains in the queue. When a request exceeds this time, the game session placement changes to a @TIMED_OUT@ status.
--
-- /Note:/ Consider using 'timeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqTimeoutInSeconds :: Lens.Lens' GameSessionQueue (Lude.Maybe Lude.Natural)
gsqTimeoutInSeconds = Lens.lens (timeoutInSeconds :: GameSessionQueue -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInSeconds = a} :: GameSessionQueue)
{-# DEPRECATED gsqTimeoutInSeconds "Use generic-lens or generic-optics with 'timeoutInSeconds' instead." #-}

-- | A list of fleets that can be used to fulfill game session placement requests in the queue. Fleets are identified by either a fleet ARN or a fleet alias ARN. Destinations are listed in default preference order.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqDestinations :: Lens.Lens' GameSessionQueue (Lude.Maybe [GameSessionQueueDestination])
gsqDestinations = Lens.lens (destinations :: GameSessionQueue -> Lude.Maybe [GameSessionQueueDestination]) (\s a -> s {destinations = a} :: GameSessionQueue)
{-# DEPRECATED gsqDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsqName :: Lens.Lens' GameSessionQueue (Lude.Maybe Lude.Text)
gsqName = Lens.lens (name :: GameSessionQueue -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GameSessionQueue)
{-# DEPRECATED gsqName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON GameSessionQueue where
  parseJSON =
    Lude.withObject
      "GameSessionQueue"
      ( \x ->
          GameSessionQueue'
            Lude.<$> (x Lude..:? "GameSessionQueueArn")
            Lude.<*> (x Lude..:? "PlayerLatencyPolicies" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TimeoutInSeconds")
            Lude.<*> (x Lude..:? "Destinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
      )
