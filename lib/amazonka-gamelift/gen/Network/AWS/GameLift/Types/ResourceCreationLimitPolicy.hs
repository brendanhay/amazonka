{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
  ( ResourceCreationLimitPolicy (..),

    -- * Smart constructor
    mkResourceCreationLimitPolicy,

    -- * Lenses
    rclpNewGameSessionsPerCreator,
    rclpPolicyPeriodInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A policy that limits the number of game sessions a player can create on the same fleet. This optional policy gives game owners control over how players can consume available game server resources. A resource creation policy makes the following statement: "An individual player can create a maximum number of new game sessions within a specified time period".
--
-- The policy is evaluated when a player tries to create a new game session. For example: Assume you have a policy of 10 new game sessions and a time period of 60 minutes. On receiving a @CreateGameSession@ request, Amazon GameLift checks that the player (identified by @CreatorId@ ) has created fewer than 10 game sessions in the past 60 minutes.
--
-- /See:/ 'mkResourceCreationLimitPolicy' smart constructor.
data ResourceCreationLimitPolicy = ResourceCreationLimitPolicy'
  { newGameSessionsPerCreator ::
      Lude.Maybe Lude.Natural,
    policyPeriodInMinutes ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceCreationLimitPolicy' with the minimum fields required to make a request.
--
-- * 'newGameSessionsPerCreator' - The maximum number of game sessions that an individual can create during the policy period.
-- * 'policyPeriodInMinutes' - The time span used in evaluating the resource creation limit policy.
mkResourceCreationLimitPolicy ::
  ResourceCreationLimitPolicy
mkResourceCreationLimitPolicy =
  ResourceCreationLimitPolicy'
    { newGameSessionsPerCreator =
        Lude.Nothing,
      policyPeriodInMinutes = Lude.Nothing
    }

-- | The maximum number of game sessions that an individual can create during the policy period.
--
-- /Note:/ Consider using 'newGameSessionsPerCreator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rclpNewGameSessionsPerCreator :: Lens.Lens' ResourceCreationLimitPolicy (Lude.Maybe Lude.Natural)
rclpNewGameSessionsPerCreator = Lens.lens (newGameSessionsPerCreator :: ResourceCreationLimitPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {newGameSessionsPerCreator = a} :: ResourceCreationLimitPolicy)
{-# DEPRECATED rclpNewGameSessionsPerCreator "Use generic-lens or generic-optics with 'newGameSessionsPerCreator' instead." #-}

-- | The time span used in evaluating the resource creation limit policy.
--
-- /Note:/ Consider using 'policyPeriodInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rclpPolicyPeriodInMinutes :: Lens.Lens' ResourceCreationLimitPolicy (Lude.Maybe Lude.Natural)
rclpPolicyPeriodInMinutes = Lens.lens (policyPeriodInMinutes :: ResourceCreationLimitPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {policyPeriodInMinutes = a} :: ResourceCreationLimitPolicy)
{-# DEPRECATED rclpPolicyPeriodInMinutes "Use generic-lens or generic-optics with 'policyPeriodInMinutes' instead." #-}

instance Lude.FromJSON ResourceCreationLimitPolicy where
  parseJSON =
    Lude.withObject
      "ResourceCreationLimitPolicy"
      ( \x ->
          ResourceCreationLimitPolicy'
            Lude.<$> (x Lude..:? "NewGameSessionsPerCreator")
            Lude.<*> (x Lude..:? "PolicyPeriodInMinutes")
      )

instance Lude.ToJSON ResourceCreationLimitPolicy where
  toJSON ResourceCreationLimitPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NewGameSessionsPerCreator" Lude..=)
              Lude.<$> newGameSessionsPerCreator,
            ("PolicyPeriodInMinutes" Lude..=) Lude.<$> policyPeriodInMinutes
          ]
      )
