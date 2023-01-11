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
-- Module      : Amazonka.IoT.Types.LocationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.LocationAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.LocationTimestamp
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Location rule action sends device location updates from an
-- MQTT message to an Amazon Location tracker resource.
--
-- /See:/ 'newLocationAction' smart constructor.
data LocationAction = LocationAction'
  { -- | The time that the location data was sampled. The default value is the
    -- time the MQTT message was processed.
    timestamp :: Prelude.Maybe LocationTimestamp,
    -- | The IAM role that grants permission to write to the Amazon Location
    -- resource.
    roleArn :: Prelude.Text,
    -- | The name of the tracker resource in Amazon Location in which the
    -- location is updated.
    trackerName :: Prelude.Text,
    -- | The unique ID of the device providing the location data.
    deviceId :: Prelude.Text,
    -- | A string that evaluates to a double value that represents the latitude
    -- of the device\'s location.
    latitude :: Prelude.Text,
    -- | A string that evaluates to a double value that represents the longitude
    -- of the device\'s location.
    longitude :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'locationAction_timestamp' - The time that the location data was sampled. The default value is the
-- time the MQTT message was processed.
--
-- 'roleArn', 'locationAction_roleArn' - The IAM role that grants permission to write to the Amazon Location
-- resource.
--
-- 'trackerName', 'locationAction_trackerName' - The name of the tracker resource in Amazon Location in which the
-- location is updated.
--
-- 'deviceId', 'locationAction_deviceId' - The unique ID of the device providing the location data.
--
-- 'latitude', 'locationAction_latitude' - A string that evaluates to a double value that represents the latitude
-- of the device\'s location.
--
-- 'longitude', 'locationAction_longitude' - A string that evaluates to a double value that represents the longitude
-- of the device\'s location.
newLocationAction ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'latitude'
  Prelude.Text ->
  -- | 'longitude'
  Prelude.Text ->
  LocationAction
newLocationAction
  pRoleArn_
  pTrackerName_
  pDeviceId_
  pLatitude_
  pLongitude_ =
    LocationAction'
      { timestamp = Prelude.Nothing,
        roleArn = pRoleArn_,
        trackerName = pTrackerName_,
        deviceId = pDeviceId_,
        latitude = pLatitude_,
        longitude = pLongitude_
      }

-- | The time that the location data was sampled. The default value is the
-- time the MQTT message was processed.
locationAction_timestamp :: Lens.Lens' LocationAction (Prelude.Maybe LocationTimestamp)
locationAction_timestamp = Lens.lens (\LocationAction' {timestamp} -> timestamp) (\s@LocationAction' {} a -> s {timestamp = a} :: LocationAction)

-- | The IAM role that grants permission to write to the Amazon Location
-- resource.
locationAction_roleArn :: Lens.Lens' LocationAction Prelude.Text
locationAction_roleArn = Lens.lens (\LocationAction' {roleArn} -> roleArn) (\s@LocationAction' {} a -> s {roleArn = a} :: LocationAction)

-- | The name of the tracker resource in Amazon Location in which the
-- location is updated.
locationAction_trackerName :: Lens.Lens' LocationAction Prelude.Text
locationAction_trackerName = Lens.lens (\LocationAction' {trackerName} -> trackerName) (\s@LocationAction' {} a -> s {trackerName = a} :: LocationAction)

-- | The unique ID of the device providing the location data.
locationAction_deviceId :: Lens.Lens' LocationAction Prelude.Text
locationAction_deviceId = Lens.lens (\LocationAction' {deviceId} -> deviceId) (\s@LocationAction' {} a -> s {deviceId = a} :: LocationAction)

-- | A string that evaluates to a double value that represents the latitude
-- of the device\'s location.
locationAction_latitude :: Lens.Lens' LocationAction Prelude.Text
locationAction_latitude = Lens.lens (\LocationAction' {latitude} -> latitude) (\s@LocationAction' {} a -> s {latitude = a} :: LocationAction)

-- | A string that evaluates to a double value that represents the longitude
-- of the device\'s location.
locationAction_longitude :: Lens.Lens' LocationAction Prelude.Text
locationAction_longitude = Lens.lens (\LocationAction' {longitude} -> longitude) (\s@LocationAction' {} a -> s {longitude = a} :: LocationAction)

instance Data.FromJSON LocationAction where
  parseJSON =
    Data.withObject
      "LocationAction"
      ( \x ->
          LocationAction'
            Prelude.<$> (x Data..:? "timestamp")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "trackerName")
            Prelude.<*> (x Data..: "deviceId")
            Prelude.<*> (x Data..: "latitude")
            Prelude.<*> (x Data..: "longitude")
      )

instance Prelude.Hashable LocationAction where
  hashWithSalt _salt LocationAction' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` trackerName
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` latitude
      `Prelude.hashWithSalt` longitude

instance Prelude.NFData LocationAction where
  rnf LocationAction' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf trackerName
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf latitude
      `Prelude.seq` Prelude.rnf longitude

instance Data.ToJSON LocationAction where
  toJSON LocationAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timestamp" Data..=) Prelude.<$> timestamp,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("trackerName" Data..= trackerName),
            Prelude.Just ("deviceId" Data..= deviceId),
            Prelude.Just ("latitude" Data..= latitude),
            Prelude.Just ("longitude" Data..= longitude)
          ]
      )
