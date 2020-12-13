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
    rsType,
    rsMessage,
    rsFleetId,
  )
where

import Network.AWS.GameLift.Types.RoutingStrategyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | The type of routing strategy for the alias.
    --
    -- Possible routing types include the following:
    --
    --     * __SIMPLE__ - The alias resolves to one specific fleet. Use this type when routing to active fleets.
    --
    --
    --     * __TERMINAL__ - The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
    type' :: Lude.Maybe RoutingStrategyType,
    -- | The message text to be used with a terminal routing strategy.
    message :: Lude.Maybe Lude.Text,
    -- | The unique identifier for a fleet that the alias points to. This value is the fleet ID, not the fleet ARN.
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoutingStrategy' with the minimum fields required to make a request.
--
-- * 'type'' - The type of routing strategy for the alias.
--
-- Possible routing types include the following:
--
--     * __SIMPLE__ - The alias resolves to one specific fleet. Use this type when routing to active fleets.
--
--
--     * __TERMINAL__ - The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
--
-- * 'message' - The message text to be used with a terminal routing strategy.
-- * 'fleetId' - The unique identifier for a fleet that the alias points to. This value is the fleet ID, not the fleet ARN.
mkRoutingStrategy ::
  RoutingStrategy
mkRoutingStrategy =
  RoutingStrategy'
    { type' = Lude.Nothing,
      message = Lude.Nothing,
      fleetId = Lude.Nothing
    }

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
rsType :: Lens.Lens' RoutingStrategy (Lude.Maybe RoutingStrategyType)
rsType = Lens.lens (type' :: RoutingStrategy -> Lude.Maybe RoutingStrategyType) (\s a -> s {type' = a} :: RoutingStrategy)
{-# DEPRECATED rsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The message text to be used with a terminal routing strategy.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsMessage :: Lens.Lens' RoutingStrategy (Lude.Maybe Lude.Text)
rsMessage = Lens.lens (message :: RoutingStrategy -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: RoutingStrategy)
{-# DEPRECATED rsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The unique identifier for a fleet that the alias points to. This value is the fleet ID, not the fleet ARN.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsFleetId :: Lens.Lens' RoutingStrategy (Lude.Maybe Lude.Text)
rsFleetId = Lens.lens (fleetId :: RoutingStrategy -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: RoutingStrategy)
{-# DEPRECATED rsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.FromJSON RoutingStrategy where
  parseJSON =
    Lude.withObject
      "RoutingStrategy"
      ( \x ->
          RoutingStrategy'
            Lude.<$> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Message")
            Lude.<*> (x Lude..:? "FleetId")
      )

instance Lude.ToJSON RoutingStrategy where
  toJSON RoutingStrategy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Type" Lude..=) Lude.<$> type',
            ("Message" Lude..=) Lude.<$> message,
            ("FleetId" Lude..=) Lude.<$> fleetId
          ]
      )
