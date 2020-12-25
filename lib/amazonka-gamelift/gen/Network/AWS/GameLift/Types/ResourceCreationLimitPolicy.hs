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
import qualified Network.AWS.Prelude as Core

-- | A policy that limits the number of game sessions a player can create on the same fleet. This optional policy gives game owners control over how players can consume available game server resources. A resource creation policy makes the following statement: "An individual player can create a maximum number of new game sessions within a specified time period".
--
-- The policy is evaluated when a player tries to create a new game session. For example: Assume you have a policy of 10 new game sessions and a time period of 60 minutes. On receiving a @CreateGameSession@ request, Amazon GameLift checks that the player (identified by @CreatorId@ ) has created fewer than 10 game sessions in the past 60 minutes.
--
-- /See:/ 'mkResourceCreationLimitPolicy' smart constructor.
data ResourceCreationLimitPolicy = ResourceCreationLimitPolicy'
  { -- | The maximum number of game sessions that an individual can create during the policy period.
    newGameSessionsPerCreator :: Core.Maybe Core.Natural,
    -- | The time span used in evaluating the resource creation limit policy.
    policyPeriodInMinutes :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceCreationLimitPolicy' value with any optional fields omitted.
mkResourceCreationLimitPolicy ::
  ResourceCreationLimitPolicy
mkResourceCreationLimitPolicy =
  ResourceCreationLimitPolicy'
    { newGameSessionsPerCreator =
        Core.Nothing,
      policyPeriodInMinutes = Core.Nothing
    }

-- | The maximum number of game sessions that an individual can create during the policy period.
--
-- /Note:/ Consider using 'newGameSessionsPerCreator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rclpNewGameSessionsPerCreator :: Lens.Lens' ResourceCreationLimitPolicy (Core.Maybe Core.Natural)
rclpNewGameSessionsPerCreator = Lens.field @"newGameSessionsPerCreator"
{-# DEPRECATED rclpNewGameSessionsPerCreator "Use generic-lens or generic-optics with 'newGameSessionsPerCreator' instead." #-}

-- | The time span used in evaluating the resource creation limit policy.
--
-- /Note:/ Consider using 'policyPeriodInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rclpPolicyPeriodInMinutes :: Lens.Lens' ResourceCreationLimitPolicy (Core.Maybe Core.Natural)
rclpPolicyPeriodInMinutes = Lens.field @"policyPeriodInMinutes"
{-# DEPRECATED rclpPolicyPeriodInMinutes "Use generic-lens or generic-optics with 'policyPeriodInMinutes' instead." #-}

instance Core.FromJSON ResourceCreationLimitPolicy where
  toJSON ResourceCreationLimitPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ ("NewGameSessionsPerCreator" Core..=)
              Core.<$> newGameSessionsPerCreator,
            ("PolicyPeriodInMinutes" Core..=) Core.<$> policyPeriodInMinutes
          ]
      )

instance Core.FromJSON ResourceCreationLimitPolicy where
  parseJSON =
    Core.withObject "ResourceCreationLimitPolicy" Core.$
      \x ->
        ResourceCreationLimitPolicy'
          Core.<$> (x Core..:? "NewGameSessionsPerCreator")
          Core.<*> (x Core..:? "PolicyPeriodInMinutes")
