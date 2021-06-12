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
-- Module      : Network.AWS.GameLift.Types.RoutingStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.RoutingStrategy where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.RoutingStrategyType
import qualified Network.AWS.Lens as Lens

-- | The routing configuration for a fleet alias.
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
--
-- /See:/ 'newRoutingStrategy' smart constructor.
data RoutingStrategy = RoutingStrategy'
  { -- | The unique identifier for a fleet that the alias points to. This value
    -- is the fleet ID, not the fleet ARN.
    fleetId :: Core.Maybe Core.Text,
    -- | The message text to be used with a terminal routing strategy.
    message :: Core.Maybe Core.Text,
    -- | The type of routing strategy for the alias.
    --
    -- Possible routing types include the following:
    --
    -- -   __SIMPLE__ - The alias resolves to one specific fleet. Use this type
    --     when routing to active fleets.
    --
    -- -   __TERMINAL__ - The alias does not resolve to a fleet but instead can
    --     be used to display a message to the user. A terminal alias throws a
    --     TerminalRoutingStrategyException with the RoutingStrategy message
    --     embedded.
    type' :: Core.Maybe RoutingStrategyType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RoutingStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'routingStrategy_fleetId' - The unique identifier for a fleet that the alias points to. This value
-- is the fleet ID, not the fleet ARN.
--
-- 'message', 'routingStrategy_message' - The message text to be used with a terminal routing strategy.
--
-- 'type'', 'routingStrategy_type' - The type of routing strategy for the alias.
--
-- Possible routing types include the following:
--
-- -   __SIMPLE__ - The alias resolves to one specific fleet. Use this type
--     when routing to active fleets.
--
-- -   __TERMINAL__ - The alias does not resolve to a fleet but instead can
--     be used to display a message to the user. A terminal alias throws a
--     TerminalRoutingStrategyException with the RoutingStrategy message
--     embedded.
newRoutingStrategy ::
  RoutingStrategy
newRoutingStrategy =
  RoutingStrategy'
    { fleetId = Core.Nothing,
      message = Core.Nothing,
      type' = Core.Nothing
    }

-- | The unique identifier for a fleet that the alias points to. This value
-- is the fleet ID, not the fleet ARN.
routingStrategy_fleetId :: Lens.Lens' RoutingStrategy (Core.Maybe Core.Text)
routingStrategy_fleetId = Lens.lens (\RoutingStrategy' {fleetId} -> fleetId) (\s@RoutingStrategy' {} a -> s {fleetId = a} :: RoutingStrategy)

-- | The message text to be used with a terminal routing strategy.
routingStrategy_message :: Lens.Lens' RoutingStrategy (Core.Maybe Core.Text)
routingStrategy_message = Lens.lens (\RoutingStrategy' {message} -> message) (\s@RoutingStrategy' {} a -> s {message = a} :: RoutingStrategy)

-- | The type of routing strategy for the alias.
--
-- Possible routing types include the following:
--
-- -   __SIMPLE__ - The alias resolves to one specific fleet. Use this type
--     when routing to active fleets.
--
-- -   __TERMINAL__ - The alias does not resolve to a fleet but instead can
--     be used to display a message to the user. A terminal alias throws a
--     TerminalRoutingStrategyException with the RoutingStrategy message
--     embedded.
routingStrategy_type :: Lens.Lens' RoutingStrategy (Core.Maybe RoutingStrategyType)
routingStrategy_type = Lens.lens (\RoutingStrategy' {type'} -> type') (\s@RoutingStrategy' {} a -> s {type' = a} :: RoutingStrategy)

instance Core.FromJSON RoutingStrategy where
  parseJSON =
    Core.withObject
      "RoutingStrategy"
      ( \x ->
          RoutingStrategy'
            Core.<$> (x Core..:? "FleetId")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable RoutingStrategy

instance Core.NFData RoutingStrategy

instance Core.ToJSON RoutingStrategy where
  toJSON RoutingStrategy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FleetId" Core..=) Core.<$> fleetId,
            ("Message" Core..=) Core.<$> message,
            ("Type" Core..=) Core.<$> type'
          ]
      )
