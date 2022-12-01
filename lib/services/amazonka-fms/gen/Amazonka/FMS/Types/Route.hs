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
-- Module      : Amazonka.FMS.Types.Route
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.Route where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.DestinationType
import Amazonka.FMS.Types.TargetType
import qualified Amazonka.Prelude as Prelude

-- | Describes a route in a route table.
--
-- /See:/ 'newRoute' smart constructor.
data Route = Route'
  { -- | The destination of the route.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The type of destination for the route.
    destinationType :: Prelude.Maybe DestinationType,
    -- | The route\'s target.
    target :: Prelude.Maybe Prelude.Text,
    -- | The type of target for the route.
    targetType :: Prelude.Maybe TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Route' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'route_destination' - The destination of the route.
--
-- 'destinationType', 'route_destinationType' - The type of destination for the route.
--
-- 'target', 'route_target' - The route\'s target.
--
-- 'targetType', 'route_targetType' - The type of target for the route.
newRoute ::
  Route
newRoute =
  Route'
    { destination = Prelude.Nothing,
      destinationType = Prelude.Nothing,
      target = Prelude.Nothing,
      targetType = Prelude.Nothing
    }

-- | The destination of the route.
route_destination :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_destination = Lens.lens (\Route' {destination} -> destination) (\s@Route' {} a -> s {destination = a} :: Route)

-- | The type of destination for the route.
route_destinationType :: Lens.Lens' Route (Prelude.Maybe DestinationType)
route_destinationType = Lens.lens (\Route' {destinationType} -> destinationType) (\s@Route' {} a -> s {destinationType = a} :: Route)

-- | The route\'s target.
route_target :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_target = Lens.lens (\Route' {target} -> target) (\s@Route' {} a -> s {target = a} :: Route)

-- | The type of target for the route.
route_targetType :: Lens.Lens' Route (Prelude.Maybe TargetType)
route_targetType = Lens.lens (\Route' {targetType} -> targetType) (\s@Route' {} a -> s {targetType = a} :: Route)

instance Core.FromJSON Route where
  parseJSON =
    Core.withObject
      "Route"
      ( \x ->
          Route'
            Prelude.<$> (x Core..:? "Destination")
            Prelude.<*> (x Core..:? "DestinationType")
            Prelude.<*> (x Core..:? "Target")
            Prelude.<*> (x Core..:? "TargetType")
      )

instance Prelude.Hashable Route where
  hashWithSalt _salt Route' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationType
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData Route where
  rnf Route' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationType
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf targetType
