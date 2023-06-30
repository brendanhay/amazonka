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
-- Module      : Amazonka.NetworkManager.Types.NetworkRoute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.NetworkRoute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.NetworkRouteDestination
import Amazonka.NetworkManager.Types.RouteState
import Amazonka.NetworkManager.Types.RouteType
import qualified Amazonka.Prelude as Prelude

-- | Describes a network route.
--
-- /See:/ 'newNetworkRoute' smart constructor.
data NetworkRoute = NetworkRoute'
  { -- | A unique identifier for the route, such as a CIDR block.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The destinations.
    destinations :: Prelude.Maybe [NetworkRouteDestination],
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Maybe Prelude.Text,
    -- | The route state. The possible values are @active@ and @blackhole@.
    state :: Prelude.Maybe RouteState,
    -- | The route type. The possible values are @propagated@ and @static@.
    type' :: Prelude.Maybe RouteType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCidrBlock', 'networkRoute_destinationCidrBlock' - A unique identifier for the route, such as a CIDR block.
--
-- 'destinations', 'networkRoute_destinations' - The destinations.
--
-- 'prefixListId', 'networkRoute_prefixListId' - The ID of the prefix list.
--
-- 'state', 'networkRoute_state' - The route state. The possible values are @active@ and @blackhole@.
--
-- 'type'', 'networkRoute_type' - The route type. The possible values are @propagated@ and @static@.
newNetworkRoute ::
  NetworkRoute
newNetworkRoute =
  NetworkRoute'
    { destinationCidrBlock =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      prefixListId = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A unique identifier for the route, such as a CIDR block.
networkRoute_destinationCidrBlock :: Lens.Lens' NetworkRoute (Prelude.Maybe Prelude.Text)
networkRoute_destinationCidrBlock = Lens.lens (\NetworkRoute' {destinationCidrBlock} -> destinationCidrBlock) (\s@NetworkRoute' {} a -> s {destinationCidrBlock = a} :: NetworkRoute)

-- | The destinations.
networkRoute_destinations :: Lens.Lens' NetworkRoute (Prelude.Maybe [NetworkRouteDestination])
networkRoute_destinations = Lens.lens (\NetworkRoute' {destinations} -> destinations) (\s@NetworkRoute' {} a -> s {destinations = a} :: NetworkRoute) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the prefix list.
networkRoute_prefixListId :: Lens.Lens' NetworkRoute (Prelude.Maybe Prelude.Text)
networkRoute_prefixListId = Lens.lens (\NetworkRoute' {prefixListId} -> prefixListId) (\s@NetworkRoute' {} a -> s {prefixListId = a} :: NetworkRoute)

-- | The route state. The possible values are @active@ and @blackhole@.
networkRoute_state :: Lens.Lens' NetworkRoute (Prelude.Maybe RouteState)
networkRoute_state = Lens.lens (\NetworkRoute' {state} -> state) (\s@NetworkRoute' {} a -> s {state = a} :: NetworkRoute)

-- | The route type. The possible values are @propagated@ and @static@.
networkRoute_type :: Lens.Lens' NetworkRoute (Prelude.Maybe RouteType)
networkRoute_type = Lens.lens (\NetworkRoute' {type'} -> type') (\s@NetworkRoute' {} a -> s {type' = a} :: NetworkRoute)

instance Data.FromJSON NetworkRoute where
  parseJSON =
    Data.withObject
      "NetworkRoute"
      ( \x ->
          NetworkRoute'
            Prelude.<$> (x Data..:? "DestinationCidrBlock")
            Prelude.<*> (x Data..:? "Destinations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PrefixListId")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable NetworkRoute where
  hashWithSalt _salt NetworkRoute' {..} =
    _salt
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` prefixListId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` type'

instance Prelude.NFData NetworkRoute where
  rnf NetworkRoute' {..} =
    Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf type'
