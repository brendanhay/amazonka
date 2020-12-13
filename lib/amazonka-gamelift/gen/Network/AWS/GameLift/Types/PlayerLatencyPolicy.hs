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
    plpPolicyDurationSeconds,
    plpMaximumIndividualPlayerLatencyMilliseconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
    policyDurationSeconds :: Lude.Maybe Lude.Natural,
    -- | The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
    maximumIndividualPlayerLatencyMilliseconds :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlayerLatencyPolicy' with the minimum fields required to make a request.
--
-- * 'policyDurationSeconds' - The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
-- * 'maximumIndividualPlayerLatencyMilliseconds' - The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
mkPlayerLatencyPolicy ::
  PlayerLatencyPolicy
mkPlayerLatencyPolicy =
  PlayerLatencyPolicy'
    { policyDurationSeconds = Lude.Nothing,
      maximumIndividualPlayerLatencyMilliseconds = Lude.Nothing
    }

-- | The length of time, in seconds, that the policy is enforced while placing a new game session. A null value for this property means that the policy is enforced until the queue times out.
--
-- /Note:/ Consider using 'policyDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpPolicyDurationSeconds :: Lens.Lens' PlayerLatencyPolicy (Lude.Maybe Lude.Natural)
plpPolicyDurationSeconds = Lens.lens (policyDurationSeconds :: PlayerLatencyPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {policyDurationSeconds = a} :: PlayerLatencyPolicy)
{-# DEPRECATED plpPolicyDurationSeconds "Use generic-lens or generic-optics with 'policyDurationSeconds' instead." #-}

-- | The maximum latency value that is allowed for any player, in milliseconds. All policies must have a value set for this property.
--
-- /Note:/ Consider using 'maximumIndividualPlayerLatencyMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plpMaximumIndividualPlayerLatencyMilliseconds :: Lens.Lens' PlayerLatencyPolicy (Lude.Maybe Lude.Natural)
plpMaximumIndividualPlayerLatencyMilliseconds = Lens.lens (maximumIndividualPlayerLatencyMilliseconds :: PlayerLatencyPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maximumIndividualPlayerLatencyMilliseconds = a} :: PlayerLatencyPolicy)
{-# DEPRECATED plpMaximumIndividualPlayerLatencyMilliseconds "Use generic-lens or generic-optics with 'maximumIndividualPlayerLatencyMilliseconds' instead." #-}

instance Lude.FromJSON PlayerLatencyPolicy where
  parseJSON =
    Lude.withObject
      "PlayerLatencyPolicy"
      ( \x ->
          PlayerLatencyPolicy'
            Lude.<$> (x Lude..:? "PolicyDurationSeconds")
            Lude.<*> (x Lude..:? "MaximumIndividualPlayerLatencyMilliseconds")
      )

instance Lude.ToJSON PlayerLatencyPolicy where
  toJSON PlayerLatencyPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PolicyDurationSeconds" Lude..=) Lude.<$> policyDurationSeconds,
            ("MaximumIndividualPlayerLatencyMilliseconds" Lude..=)
              Lude.<$> maximumIndividualPlayerLatencyMilliseconds
          ]
      )
