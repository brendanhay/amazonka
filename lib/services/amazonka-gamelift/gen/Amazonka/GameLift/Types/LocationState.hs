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
-- Module      : Amazonka.GameLift.Types.LocationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LocationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types.FleetStatus
import qualified Amazonka.Prelude as Prelude

-- | A fleet location and its life-cycle state. A location state object might
-- be used to describe a fleet\'s remote location or home Region.
-- Life-cycle state tracks the progress of launching the first instance in
-- a new location and preparing it for game hosting, and then removing all
-- instances and deleting the location from the fleet.
--
-- __Related actions__
--
-- CreateFleet | CreateFleetLocations | DeleteFleetLocations
--
-- /See:/ 'newLocationState' smart constructor.
data LocationState = LocationState'
  { -- | The life-cycle status of a fleet location.
    status :: Prelude.Maybe FleetStatus,
    -- | The fleet location, expressed as an Amazon Web Services Region code such
    -- as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'locationState_status' - The life-cycle status of a fleet location.
--
-- 'location', 'locationState_location' - The fleet location, expressed as an Amazon Web Services Region code such
-- as @us-west-2@.
newLocationState ::
  LocationState
newLocationState =
  LocationState'
    { status = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The life-cycle status of a fleet location.
locationState_status :: Lens.Lens' LocationState (Prelude.Maybe FleetStatus)
locationState_status = Lens.lens (\LocationState' {status} -> status) (\s@LocationState' {} a -> s {status = a} :: LocationState)

-- | The fleet location, expressed as an Amazon Web Services Region code such
-- as @us-west-2@.
locationState_location :: Lens.Lens' LocationState (Prelude.Maybe Prelude.Text)
locationState_location = Lens.lens (\LocationState' {location} -> location) (\s@LocationState' {} a -> s {location = a} :: LocationState)

instance Core.FromJSON LocationState where
  parseJSON =
    Core.withObject
      "LocationState"
      ( \x ->
          LocationState'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Location")
      )

instance Prelude.Hashable LocationState where
  hashWithSalt _salt LocationState' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` location

instance Prelude.NFData LocationState where
  rnf LocationState' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf location
