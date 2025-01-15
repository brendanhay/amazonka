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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LocationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.FleetStatus
import qualified Amazonka.Prelude as Prelude

-- | A fleet location and its life-cycle state. A location state object might
-- be used to describe a fleet\'s remote location or home Region.
-- Life-cycle state tracks the progress of launching the first instance in
-- a new location and preparing it for game hosting, and then removing all
-- instances and deleting the location from the fleet.
--
-- -   __NEW__ -- A new fleet location has been defined and desired
--     instances is set to 1.
--
-- -   __DOWNLOADING\/VALIDATING\/BUILDING\/ACTIVATING__ -- GameLift is
--     setting up the new fleet location, creating new instances with the
--     game build or Realtime script and starting server processes.
--
-- -   __ACTIVE__ -- Hosts can now accept game sessions.
--
-- -   __ERROR__ -- An error occurred when downloading, validating,
--     building, or activating the fleet location.
--
-- -   __DELETING__ -- Hosts are responding to a delete fleet location
--     request.
--
-- -   __TERMINATED__ -- The fleet location no longer exists.
--
-- -   __NOT_FOUND__ -- The fleet location was not found. This could be
--     because the custom location was removed or not created.
--
-- /See:/ 'newLocationState' smart constructor.
data LocationState = LocationState'
  { -- | The fleet location, expressed as an Amazon Web Services Region code such
    -- as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The life-cycle status of a fleet location.
    status :: Prelude.Maybe FleetStatus
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
-- 'location', 'locationState_location' - The fleet location, expressed as an Amazon Web Services Region code such
-- as @us-west-2@.
--
-- 'status', 'locationState_status' - The life-cycle status of a fleet location.
newLocationState ::
  LocationState
newLocationState =
  LocationState'
    { location = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The fleet location, expressed as an Amazon Web Services Region code such
-- as @us-west-2@.
locationState_location :: Lens.Lens' LocationState (Prelude.Maybe Prelude.Text)
locationState_location = Lens.lens (\LocationState' {location} -> location) (\s@LocationState' {} a -> s {location = a} :: LocationState)

-- | The life-cycle status of a fleet location.
locationState_status :: Lens.Lens' LocationState (Prelude.Maybe FleetStatus)
locationState_status = Lens.lens (\LocationState' {status} -> status) (\s@LocationState' {} a -> s {status = a} :: LocationState)

instance Data.FromJSON LocationState where
  parseJSON =
    Data.withObject
      "LocationState"
      ( \x ->
          LocationState'
            Prelude.<$> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable LocationState where
  hashWithSalt _salt LocationState' {..} =
    _salt
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` status

instance Prelude.NFData LocationState where
  rnf LocationState' {..} =
    Prelude.rnf location `Prelude.seq`
      Prelude.rnf status
