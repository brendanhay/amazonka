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
-- Module      : Amazonka.GameLift.Types.RoutingStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.RoutingStrategy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.RoutingStrategyType
import qualified Amazonka.Prelude as Prelude

-- | The routing configuration for a fleet alias.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newRoutingStrategy' smart constructor.
data RoutingStrategy = RoutingStrategy'
  { -- | A unique identifier for the fleet that the alias points to. This value
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
    --     TerminalRoutingStrategyException with the message embedded.
    type' :: Prelude.Maybe RoutingStrategyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'routingStrategy_fleetId' - A unique identifier for the fleet that the alias points to. This value
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
--     TerminalRoutingStrategyException with the message embedded.
newRoutingStrategy ::
  RoutingStrategy
newRoutingStrategy =
  RoutingStrategy'
    { fleetId = Prelude.Nothing,
      message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A unique identifier for the fleet that the alias points to. This value
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
--     TerminalRoutingStrategyException with the message embedded.
routingStrategy_type :: Lens.Lens' RoutingStrategy (Prelude.Maybe RoutingStrategyType)
routingStrategy_type = Lens.lens (\RoutingStrategy' {type'} -> type') (\s@RoutingStrategy' {} a -> s {type' = a} :: RoutingStrategy)

instance Data.FromJSON RoutingStrategy where
  parseJSON =
    Data.withObject
      "RoutingStrategy"
      ( \x ->
          RoutingStrategy'
            Prelude.<$> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable RoutingStrategy where
  hashWithSalt _salt RoutingStrategy' {..} =
    _salt `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RoutingStrategy where
  rnf RoutingStrategy' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON RoutingStrategy where
  toJSON RoutingStrategy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FleetId" Data..=) Prelude.<$> fleetId,
            ("Message" Data..=) Prelude.<$> message,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
