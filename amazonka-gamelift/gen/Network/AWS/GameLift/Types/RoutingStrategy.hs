{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GameLift.Types.RoutingStrategyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The message text to be used with a terminal routing strategy.
    message :: Prelude.Maybe Prelude.Text,
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
    type' :: Prelude.Maybe RoutingStrategyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { fleetId = Prelude.Nothing,
      message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The unique identifier for a fleet that the alias points to. This value
-- is the fleet ID, not the fleet ARN.
routingStrategy_fleetId :: Lens.Lens' RoutingStrategy (Prelude.Maybe Prelude.Text)
routingStrategy_fleetId = Lens.lens (\RoutingStrategy' {fleetId} -> fleetId) (\s@RoutingStrategy' {} a -> s {fleetId = a} :: RoutingStrategy)

-- | The message text to be used with a terminal routing strategy.
routingStrategy_message :: Lens.Lens' RoutingStrategy (Prelude.Maybe Prelude.Text)
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
routingStrategy_type :: Lens.Lens' RoutingStrategy (Prelude.Maybe RoutingStrategyType)
routingStrategy_type = Lens.lens (\RoutingStrategy' {type'} -> type') (\s@RoutingStrategy' {} a -> s {type' = a} :: RoutingStrategy)

instance Prelude.FromJSON RoutingStrategy where
  parseJSON =
    Prelude.withObject
      "RoutingStrategy"
      ( \x ->
          RoutingStrategy'
            Prelude.<$> (x Prelude..:? "FleetId")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable RoutingStrategy

instance Prelude.NFData RoutingStrategy

instance Prelude.ToJSON RoutingStrategy where
  toJSON RoutingStrategy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FleetId" Prelude..=) Prelude.<$> fleetId,
            ("Message" Prelude..=) Prelude.<$> message,
            ("Type" Prelude..=) Prelude.<$> type'
          ]
      )
