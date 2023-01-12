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
-- Module      : Amazonka.IoTFleetWise.Types.VehicleStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.VehicleStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.VehicleState
import qualified Amazonka.Prelude as Prelude

-- | Information about the state of a vehicle and how it relates to the
-- status of a campaign.
--
-- /See:/ 'newVehicleStatus' smart constructor.
data VehicleStatus = VehicleStatus'
  { -- | The name of a campaign.
    campaignName :: Prelude.Maybe Prelude.Text,
    -- | The state of a vehicle, which can be one of the following:
    --
    -- -   @CREATED@ - Amazon Web Services IoT FleetWise sucessfully created
    --     the vehicle.
    --
    -- -   @READY@ - The vehicle is ready to receive a campaign deployment.
    --
    -- -   @HEALTHY@ - A campaign deployment was delivered to the vehicle.
    --
    -- -   @SUSPENDED@ - A campaign associated with the vehicle was suspended
    --     and data collection was paused.
    --
    -- -   @DELETING@ - Amazon Web Services IoT FleetWise is removing a
    --     campaign from the vehicle.
    status :: Prelude.Maybe VehicleState,
    -- | The unique ID of the vehicle.
    vehicleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VehicleStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'campaignName', 'vehicleStatus_campaignName' - The name of a campaign.
--
-- 'status', 'vehicleStatus_status' - The state of a vehicle, which can be one of the following:
--
-- -   @CREATED@ - Amazon Web Services IoT FleetWise sucessfully created
--     the vehicle.
--
-- -   @READY@ - The vehicle is ready to receive a campaign deployment.
--
-- -   @HEALTHY@ - A campaign deployment was delivered to the vehicle.
--
-- -   @SUSPENDED@ - A campaign associated with the vehicle was suspended
--     and data collection was paused.
--
-- -   @DELETING@ - Amazon Web Services IoT FleetWise is removing a
--     campaign from the vehicle.
--
-- 'vehicleName', 'vehicleStatus_vehicleName' - The unique ID of the vehicle.
newVehicleStatus ::
  VehicleStatus
newVehicleStatus =
  VehicleStatus'
    { campaignName = Prelude.Nothing,
      status = Prelude.Nothing,
      vehicleName = Prelude.Nothing
    }

-- | The name of a campaign.
vehicleStatus_campaignName :: Lens.Lens' VehicleStatus (Prelude.Maybe Prelude.Text)
vehicleStatus_campaignName = Lens.lens (\VehicleStatus' {campaignName} -> campaignName) (\s@VehicleStatus' {} a -> s {campaignName = a} :: VehicleStatus)

-- | The state of a vehicle, which can be one of the following:
--
-- -   @CREATED@ - Amazon Web Services IoT FleetWise sucessfully created
--     the vehicle.
--
-- -   @READY@ - The vehicle is ready to receive a campaign deployment.
--
-- -   @HEALTHY@ - A campaign deployment was delivered to the vehicle.
--
-- -   @SUSPENDED@ - A campaign associated with the vehicle was suspended
--     and data collection was paused.
--
-- -   @DELETING@ - Amazon Web Services IoT FleetWise is removing a
--     campaign from the vehicle.
vehicleStatus_status :: Lens.Lens' VehicleStatus (Prelude.Maybe VehicleState)
vehicleStatus_status = Lens.lens (\VehicleStatus' {status} -> status) (\s@VehicleStatus' {} a -> s {status = a} :: VehicleStatus)

-- | The unique ID of the vehicle.
vehicleStatus_vehicleName :: Lens.Lens' VehicleStatus (Prelude.Maybe Prelude.Text)
vehicleStatus_vehicleName = Lens.lens (\VehicleStatus' {vehicleName} -> vehicleName) (\s@VehicleStatus' {} a -> s {vehicleName = a} :: VehicleStatus)

instance Data.FromJSON VehicleStatus where
  parseJSON =
    Data.withObject
      "VehicleStatus"
      ( \x ->
          VehicleStatus'
            Prelude.<$> (x Data..:? "campaignName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "vehicleName")
      )

instance Prelude.Hashable VehicleStatus where
  hashWithSalt _salt VehicleStatus' {..} =
    _salt `Prelude.hashWithSalt` campaignName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` vehicleName

instance Prelude.NFData VehicleStatus where
  rnf VehicleStatus' {..} =
    Prelude.rnf campaignName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vehicleName
