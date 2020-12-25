{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerLatencyPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerLatencyPolicy
  ( PlayerLatencyPolicy (..),

    -- * Smart constructor
    mkPlayerLatencyPolicy,

    -- * Lenses
    plpMaximumIndividualPlayerLatencyMilliseconds,
    plpPolicyDurationSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Queue setting that determines the highest latency allowed for individual players when placing a game session. When a latency policy is in force, a game session cannot be placed with any fleet in a Region where a player reports latency higher than the cap. Latency policies are only enforced when the placement request contains player latency information.
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
-- /See:/ 'mkPlayerLatencyPolicy' smart constructor.
data PlayerLatencyPolicy = PlayerLatencyPolicy'
  { -- | The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
    maximumIndividualPlayerLatencyMilliseconds :: Core.Maybe Core.Natural,
    -- | The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
    policyDurationSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlayerLatencyPolicy' value with any optional fields omitted.
mkPlayerLatencyPolicy ::
  PlayerLatencyPolicy
mkPlayerLatencyPolicy =
  PlayerLatencyPolicy'
    { maximumIndividualPlayerLatencyMilliseconds =
        Core.Nothing,
      policyDurationSeconds = Core.Nothing
    }

-- | The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
--
-- /Note:/ Consider using 'maximumIndividualPlayerLatencyMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpMaximumIndividualPlayerLatencyMilliseconds :: Lens.Lens' PlayerLatencyPolicy (Core.Maybe Core.Natural)
plpMaximumIndividualPlayerLatencyMilliseconds = Lens.field @"maximumIndividualPlayerLatencyMilliseconds"
{-# DEPRECATED plpMaximumIndividualPlayerLatencyMilliseconds "Use generic-lens or generic-optics with 'maximumIndividualPlayerLatencyMilliseconds' instead." #-}

-- | The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
--
-- /Note:/ Consider using 'policyDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpPolicyDurationSeconds :: Lens.Lens' PlayerLatencyPolicy (Core.Maybe Core.Natural)
plpPolicyDurationSeconds = Lens.field @"policyDurationSeconds"
{-# DEPRECATED plpPolicyDurationSeconds "Use generic-lens or generic-optics with 'policyDurationSeconds' instead." #-}

instance Core.FromJSON PlayerLatencyPolicy where
  toJSON PlayerLatencyPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaximumIndividualPlayerLatencyMilliseconds" Core..=)
              Core.<$> maximumIndividualPlayerLatencyMilliseconds,
            ("PolicyDurationSeconds" Core..=) Core.<$> policyDurationSeconds
          ]
      )

instance Core.FromJSON PlayerLatencyPolicy where
  parseJSON =
    Core.withObject "PlayerLatencyPolicy" Core.$
      \x ->
        PlayerLatencyPolicy'
          Core.<$> (x Core..:? "MaximumIndividualPlayerLatencyMilliseconds")
          Core.<*> (x Core..:? "PolicyDurationSeconds")
