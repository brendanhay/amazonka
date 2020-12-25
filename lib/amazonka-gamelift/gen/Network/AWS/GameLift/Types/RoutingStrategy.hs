{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.RoutingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.RoutingStrategy
  ( RoutingStrategy (..),

    -- * Smart constructor
    mkRoutingStrategy,

    -- * Lenses
    rsFleetId,
    rsMessage,
    rsType,
  )
where

import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.FreeText as Types
import qualified Network.AWS.GameLift.Types.RoutingStrategyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The routing configuration for a fleet alias.
--
--
--     * 'CreateAlias'
--
--
--     * 'ListAliases'
--
--
--     * 'DescribeAlias'
--
--
--     * 'UpdateAlias'
--
--
--     * 'DeleteAlias'
--
--
--     * 'ResolveAlias'
--
--
--
-- /See:/ 'mkRoutingStrategy' smart constructor.
data RoutingStrategy = RoutingStrategy'
  { -- | The unique identifier for a fleet that the alias points to. This value is the fleet ID, not the fleet ARN.
    fleetId :: Core.Maybe Types.FleetId,
    -- | The message text to be used with a terminal routing strategy.
    message :: Core.Maybe Types.FreeText,
    -- | The type of routing strategy for the alias.
    --
    -- Possible routing types include the following:
    --
    --     * __SIMPLE__ - The alias resolves to one specific fleet. Use this type when routing to active fleets.
    --
    --
    --     * __TERMINAL__ - The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
    type' :: Core.Maybe Types.RoutingStrategyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoutingStrategy' value with any optional fields omitted.
mkRoutingStrategy ::
  RoutingStrategy
mkRoutingStrategy =
  RoutingStrategy'
    { fleetId = Core.Nothing,
      message = Core.Nothing,
      type' = Core.Nothing
    }

-- | The unique identifier for a fleet that the alias points to. This value is the fleet ID, not the fleet ARN.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsFleetId :: Lens.Lens' RoutingStrategy (Core.Maybe Types.FleetId)
rsFleetId = Lens.field @"fleetId"
{-# DEPRECATED rsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The message text to be used with a terminal routing strategy.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsMessage :: Lens.Lens' RoutingStrategy (Core.Maybe Types.FreeText)
rsMessage = Lens.field @"message"
{-# DEPRECATED rsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The type of routing strategy for the alias.
--
-- Possible routing types include the following:
--
--     * __SIMPLE__ - The alias resolves to one specific fleet. Use this type when routing to active fleets.
--
--
--     * __TERMINAL__ - The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsType :: Lens.Lens' RoutingStrategy (Core.Maybe Types.RoutingStrategyType)
rsType = Lens.field @"type'"
{-# DEPRECATED rsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON RoutingStrategy where
  toJSON RoutingStrategy {..} =
    Core.object
      ( Core.catMaybes
          [ ("FleetId" Core..=) Core.<$> fleetId,
            ("Message" Core..=) Core.<$> message,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON RoutingStrategy where
  parseJSON =
    Core.withObject "RoutingStrategy" Core.$
      \x ->
        RoutingStrategy'
          Core.<$> (x Core..:? "FleetId")
          Core.<*> (x Core..:? "Message")
          Core.<*> (x Core..:? "Type")
