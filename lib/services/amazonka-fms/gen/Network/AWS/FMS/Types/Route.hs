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
-- Module      : Network.AWS.FMS.Types.Route
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.Route where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.DestinationType
import Network.AWS.FMS.Types.TargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a route in a route table.
--
-- /See:/ 'newRoute' smart constructor.
data Route = Route'
  { -- | The destination of the route.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The type of target for the route.
    targetType :: Prelude.Maybe TargetType,
    -- | The type of destination for the route.
    destinationType :: Prelude.Maybe DestinationType,
    -- | The route\'s target.
    target :: Prelude.Maybe Prelude.Text
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
-- 'targetType', 'route_targetType' - The type of target for the route.
--
-- 'destinationType', 'route_destinationType' - The type of destination for the route.
--
-- 'target', 'route_target' - The route\'s target.
newRoute ::
  Route
newRoute =
  Route'
    { destination = Prelude.Nothing,
      targetType = Prelude.Nothing,
      destinationType = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The destination of the route.
route_destination :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_destination = Lens.lens (\Route' {destination} -> destination) (\s@Route' {} a -> s {destination = a} :: Route)

-- | The type of target for the route.
route_targetType :: Lens.Lens' Route (Prelude.Maybe TargetType)
route_targetType = Lens.lens (\Route' {targetType} -> targetType) (\s@Route' {} a -> s {targetType = a} :: Route)

-- | The type of destination for the route.
route_destinationType :: Lens.Lens' Route (Prelude.Maybe DestinationType)
route_destinationType = Lens.lens (\Route' {destinationType} -> destinationType) (\s@Route' {} a -> s {destinationType = a} :: Route)

-- | The route\'s target.
route_target :: Lens.Lens' Route (Prelude.Maybe Prelude.Text)
route_target = Lens.lens (\Route' {target} -> target) (\s@Route' {} a -> s {target = a} :: Route)

instance Core.FromJSON Route where
  parseJSON =
    Core.withObject
      "Route"
      ( \x ->
          Route'
            Prelude.<$> (x Core..:? "Destination")
            Prelude.<*> (x Core..:? "TargetType")
            Prelude.<*> (x Core..:? "DestinationType")
            Prelude.<*> (x Core..:? "Target")
      )

instance Prelude.Hashable Route

instance Prelude.NFData Route
