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
-- Module      : Amazonka.GameLift.Types.LocationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LocationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.FleetAction
import Amazonka.GameLift.Types.LocationState
import Amazonka.GameLift.Types.LocationUpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about a location in a multi-location fleet.
--
-- /See:/ 'newLocationAttributes' smart constructor.
data LocationAttributes = LocationAttributes'
  { -- | A fleet location and its current life-cycle state.
    locationState :: Prelude.Maybe LocationState,
    -- | A list of fleet actions that have been suspended in the fleet location.
    stoppedActions :: Prelude.Maybe (Prelude.NonEmpty FleetAction),
    -- | The status of fleet activity updates to the location. The status
    -- @PENDING_UPDATE@ indicates that @StopFleetActions@ or
    -- @StartFleetActions@ has been requested but the update has not yet been
    -- completed for the location.
    updateStatus :: Prelude.Maybe LocationUpdateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationState', 'locationAttributes_locationState' - A fleet location and its current life-cycle state.
--
-- 'stoppedActions', 'locationAttributes_stoppedActions' - A list of fleet actions that have been suspended in the fleet location.
--
-- 'updateStatus', 'locationAttributes_updateStatus' - The status of fleet activity updates to the location. The status
-- @PENDING_UPDATE@ indicates that @StopFleetActions@ or
-- @StartFleetActions@ has been requested but the update has not yet been
-- completed for the location.
newLocationAttributes ::
  LocationAttributes
newLocationAttributes =
  LocationAttributes'
    { locationState =
        Prelude.Nothing,
      stoppedActions = Prelude.Nothing,
      updateStatus = Prelude.Nothing
    }

-- | A fleet location and its current life-cycle state.
locationAttributes_locationState :: Lens.Lens' LocationAttributes (Prelude.Maybe LocationState)
locationAttributes_locationState = Lens.lens (\LocationAttributes' {locationState} -> locationState) (\s@LocationAttributes' {} a -> s {locationState = a} :: LocationAttributes)

-- | A list of fleet actions that have been suspended in the fleet location.
locationAttributes_stoppedActions :: Lens.Lens' LocationAttributes (Prelude.Maybe (Prelude.NonEmpty FleetAction))
locationAttributes_stoppedActions = Lens.lens (\LocationAttributes' {stoppedActions} -> stoppedActions) (\s@LocationAttributes' {} a -> s {stoppedActions = a} :: LocationAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The status of fleet activity updates to the location. The status
-- @PENDING_UPDATE@ indicates that @StopFleetActions@ or
-- @StartFleetActions@ has been requested but the update has not yet been
-- completed for the location.
locationAttributes_updateStatus :: Lens.Lens' LocationAttributes (Prelude.Maybe LocationUpdateStatus)
locationAttributes_updateStatus = Lens.lens (\LocationAttributes' {updateStatus} -> updateStatus) (\s@LocationAttributes' {} a -> s {updateStatus = a} :: LocationAttributes)

instance Data.FromJSON LocationAttributes where
  parseJSON =
    Data.withObject
      "LocationAttributes"
      ( \x ->
          LocationAttributes'
            Prelude.<$> (x Data..:? "LocationState")
            Prelude.<*> (x Data..:? "StoppedActions")
            Prelude.<*> (x Data..:? "UpdateStatus")
      )

instance Prelude.Hashable LocationAttributes where
  hashWithSalt _salt LocationAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` locationState
      `Prelude.hashWithSalt` stoppedActions
      `Prelude.hashWithSalt` updateStatus

instance Prelude.NFData LocationAttributes where
  rnf LocationAttributes' {..} =
    Prelude.rnf locationState
      `Prelude.seq` Prelude.rnf stoppedActions
      `Prelude.seq` Prelude.rnf updateStatus
