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
-- Module      : Network.AWS.GameLift.Types.LocationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.LocationAttributes where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.FleetAction
import Network.AWS.GameLift.Types.LocationState
import Network.AWS.GameLift.Types.LocationUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a location in a multi-location fleet.
--
-- __Related actions__
--
-- DescribeFleetLocationAttributes
--
-- /See:/ 'newLocationAttributes' smart constructor.
data LocationAttributes = LocationAttributes'
  { -- | The status of fleet activity updates to the location. The status
    -- @PENDING_UPDATE@ indicates that StopFleetActions or StartFleetActions
    -- has been requested but the update has not yet been completed for the
    -- location.
    updateStatus :: Prelude.Maybe LocationUpdateStatus,
    -- | A fleet location and its current life-cycle state.
    locationState :: Prelude.Maybe LocationState,
    -- | A list of fleet actions that have been suspended in the fleet location.
    stoppedActions :: Prelude.Maybe (Prelude.NonEmpty FleetAction)
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
-- 'updateStatus', 'locationAttributes_updateStatus' - The status of fleet activity updates to the location. The status
-- @PENDING_UPDATE@ indicates that StopFleetActions or StartFleetActions
-- has been requested but the update has not yet been completed for the
-- location.
--
-- 'locationState', 'locationAttributes_locationState' - A fleet location and its current life-cycle state.
--
-- 'stoppedActions', 'locationAttributes_stoppedActions' - A list of fleet actions that have been suspended in the fleet location.
newLocationAttributes ::
  LocationAttributes
newLocationAttributes =
  LocationAttributes'
    { updateStatus = Prelude.Nothing,
      locationState = Prelude.Nothing,
      stoppedActions = Prelude.Nothing
    }

-- | The status of fleet activity updates to the location. The status
-- @PENDING_UPDATE@ indicates that StopFleetActions or StartFleetActions
-- has been requested but the update has not yet been completed for the
-- location.
locationAttributes_updateStatus :: Lens.Lens' LocationAttributes (Prelude.Maybe LocationUpdateStatus)
locationAttributes_updateStatus = Lens.lens (\LocationAttributes' {updateStatus} -> updateStatus) (\s@LocationAttributes' {} a -> s {updateStatus = a} :: LocationAttributes)

-- | A fleet location and its current life-cycle state.
locationAttributes_locationState :: Lens.Lens' LocationAttributes (Prelude.Maybe LocationState)
locationAttributes_locationState = Lens.lens (\LocationAttributes' {locationState} -> locationState) (\s@LocationAttributes' {} a -> s {locationState = a} :: LocationAttributes)

-- | A list of fleet actions that have been suspended in the fleet location.
locationAttributes_stoppedActions :: Lens.Lens' LocationAttributes (Prelude.Maybe (Prelude.NonEmpty FleetAction))
locationAttributes_stoppedActions = Lens.lens (\LocationAttributes' {stoppedActions} -> stoppedActions) (\s@LocationAttributes' {} a -> s {stoppedActions = a} :: LocationAttributes) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON LocationAttributes where
  parseJSON =
    Core.withObject
      "LocationAttributes"
      ( \x ->
          LocationAttributes'
            Prelude.<$> (x Core..:? "UpdateStatus")
            Prelude.<*> (x Core..:? "LocationState")
            Prelude.<*> (x Core..:? "StoppedActions")
      )

instance Prelude.Hashable LocationAttributes

instance Prelude.NFData LocationAttributes
