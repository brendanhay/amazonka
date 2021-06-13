{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionQueue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionQueue where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.GameSessionQueueDestination
import Network.AWS.GameLift.Types.PlayerLatencyPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration of a queue that is used to process game session placement
-- requests. The queue configuration identifies several game features:
--
-- -   The destinations where a new game session can potentially be hosted.
--     Amazon GameLift tries these destinations in an order based on either
--     the queue\'s default order or player latency information, if
--     provided in a placement request. With latency information, Amazon
--     GameLift can place game sessions where the majority of players are
--     reporting the lowest possible latency.
--
-- -   The length of time that placement requests can wait in the queue
--     before timing out.
--
-- -   A set of optional latency policies that protect individual players
--     from high latencies, preventing game sessions from being placed
--     where any individual player is reporting latency higher than a
--     policy\'s maximum.
--
-- -   CreateGameSessionQueue
--
-- -   DescribeGameSessionQueues
--
-- -   UpdateGameSessionQueue
--
-- -   DeleteGameSessionQueue
--
-- /See:/ 'newGameSessionQueue' smart constructor.
data GameSessionQueue = GameSessionQueue'
  { -- | A collection of latency policies to apply when processing game sessions
    -- placement requests with player latency information. Multiple policies
    -- are evaluated in order of the maximum latency value, starting with the
    -- lowest latency values. With just one policy, the policy is enforced at
    -- the start of the game session placement for the duration period. With
    -- multiple policies, each policy is enforced consecutively for its
    -- duration period. For example, a queue might enforce a 60-second policy
    -- followed by a 120-second policy, and then no policy for the remainder of
    -- the placement.
    playerLatencyPolicies :: Prelude.Maybe [PlayerLatencyPolicy],
    -- | The maximum time, in seconds, that a new game session placement request
    -- remains in the queue. When a request exceeds this time, the game session
    -- placement changes to a @TIMED_OUT@ status.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A list of fleets that can be used to fulfill game session placement
    -- requests in the queue. Fleets are identified by either a fleet ARN or a
    -- fleet alias ARN. Destinations are listed in default preference order.
    destinations :: Prelude.Maybe [GameSessionQueueDestination],
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region.
    name :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift game session queue resource and uniquely
    -- identifies it. ARNs are unique across all Regions. In a GameLift game
    -- session queue ARN, the resource ID matches the /Name/ value.
    gameSessionQueueArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameSessionQueue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'playerLatencyPolicies', 'gameSessionQueue_playerLatencyPolicies' - A collection of latency policies to apply when processing game sessions
-- placement requests with player latency information. Multiple policies
-- are evaluated in order of the maximum latency value, starting with the
-- lowest latency values. With just one policy, the policy is enforced at
-- the start of the game session placement for the duration period. With
-- multiple policies, each policy is enforced consecutively for its
-- duration period. For example, a queue might enforce a 60-second policy
-- followed by a 120-second policy, and then no policy for the remainder of
-- the placement.
--
-- 'timeoutInSeconds', 'gameSessionQueue_timeoutInSeconds' - The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
--
-- 'destinations', 'gameSessionQueue_destinations' - A list of fleets that can be used to fulfill game session placement
-- requests in the queue. Fleets are identified by either a fleet ARN or a
-- fleet alias ARN. Destinations are listed in default preference order.
--
-- 'name', 'gameSessionQueue_name' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
--
-- 'gameSessionQueueArn', 'gameSessionQueue_gameSessionQueueArn' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. In a GameLift game
-- session queue ARN, the resource ID matches the /Name/ value.
newGameSessionQueue ::
  GameSessionQueue
newGameSessionQueue =
  GameSessionQueue'
    { playerLatencyPolicies =
        Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      gameSessionQueueArn = Prelude.Nothing
    }

-- | A collection of latency policies to apply when processing game sessions
-- placement requests with player latency information. Multiple policies
-- are evaluated in order of the maximum latency value, starting with the
-- lowest latency values. With just one policy, the policy is enforced at
-- the start of the game session placement for the duration period. With
-- multiple policies, each policy is enforced consecutively for its
-- duration period. For example, a queue might enforce a 60-second policy
-- followed by a 120-second policy, and then no policy for the remainder of
-- the placement.
gameSessionQueue_playerLatencyPolicies :: Lens.Lens' GameSessionQueue (Prelude.Maybe [PlayerLatencyPolicy])
gameSessionQueue_playerLatencyPolicies = Lens.lens (\GameSessionQueue' {playerLatencyPolicies} -> playerLatencyPolicies) (\s@GameSessionQueue' {} a -> s {playerLatencyPolicies = a} :: GameSessionQueue) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum time, in seconds, that a new game session placement request
-- remains in the queue. When a request exceeds this time, the game session
-- placement changes to a @TIMED_OUT@ status.
gameSessionQueue_timeoutInSeconds :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Natural)
gameSessionQueue_timeoutInSeconds = Lens.lens (\GameSessionQueue' {timeoutInSeconds} -> timeoutInSeconds) (\s@GameSessionQueue' {} a -> s {timeoutInSeconds = a} :: GameSessionQueue)

-- | A list of fleets that can be used to fulfill game session placement
-- requests in the queue. Fleets are identified by either a fleet ARN or a
-- fleet alias ARN. Destinations are listed in default preference order.
gameSessionQueue_destinations :: Lens.Lens' GameSessionQueue (Prelude.Maybe [GameSessionQueueDestination])
gameSessionQueue_destinations = Lens.lens (\GameSessionQueue' {destinations} -> destinations) (\s@GameSessionQueue' {} a -> s {destinations = a} :: GameSessionQueue) Prelude.. Lens.mapping Lens._Coerce

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
gameSessionQueue_name :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_name = Lens.lens (\GameSessionQueue' {name} -> name) (\s@GameSessionQueue' {} a -> s {name = a} :: GameSessionQueue)

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift game session queue resource and uniquely
-- identifies it. ARNs are unique across all Regions. In a GameLift game
-- session queue ARN, the resource ID matches the /Name/ value.
gameSessionQueue_gameSessionQueueArn :: Lens.Lens' GameSessionQueue (Prelude.Maybe Prelude.Text)
gameSessionQueue_gameSessionQueueArn = Lens.lens (\GameSessionQueue' {gameSessionQueueArn} -> gameSessionQueueArn) (\s@GameSessionQueue' {} a -> s {gameSessionQueueArn = a} :: GameSessionQueue)

instance Core.FromJSON GameSessionQueue where
  parseJSON =
    Core.withObject
      "GameSessionQueue"
      ( \x ->
          GameSessionQueue'
            Prelude.<$> ( x Core..:? "PlayerLatencyPolicies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TimeoutInSeconds")
            Prelude.<*> (x Core..:? "Destinations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "GameSessionQueueArn")
      )

instance Prelude.Hashable GameSessionQueue

instance Prelude.NFData GameSessionQueue
