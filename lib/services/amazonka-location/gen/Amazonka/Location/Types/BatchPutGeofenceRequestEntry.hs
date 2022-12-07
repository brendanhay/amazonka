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
-- Module      : Amazonka.Location.Types.BatchPutGeofenceRequestEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchPutGeofenceRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.GeofenceGeometry
import qualified Amazonka.Prelude as Prelude

-- | Contains geofence geometry details.
--
-- /See:/ 'newBatchPutGeofenceRequestEntry' smart constructor.
data BatchPutGeofenceRequestEntry = BatchPutGeofenceRequestEntry'
  { -- | The identifier for the geofence to be stored in a given geofence
    -- collection.
    geofenceId :: Prelude.Text,
    -- | Contains the details of the position of the geofence. Can be either a
    -- polygon or a circle. Including both will return a validation error.
    --
    -- Each
    -- <https://docs.aws.amazon.com/location-geofences/latest/APIReference/API_GeofenceGeometry.html geofence polygon>
    -- can have a maximum of 1,000 vertices.
    geometry :: GeofenceGeometry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutGeofenceRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geofenceId', 'batchPutGeofenceRequestEntry_geofenceId' - The identifier for the geofence to be stored in a given geofence
-- collection.
--
-- 'geometry', 'batchPutGeofenceRequestEntry_geometry' - Contains the details of the position of the geofence. Can be either a
-- polygon or a circle. Including both will return a validation error.
--
-- Each
-- <https://docs.aws.amazon.com/location-geofences/latest/APIReference/API_GeofenceGeometry.html geofence polygon>
-- can have a maximum of 1,000 vertices.
newBatchPutGeofenceRequestEntry ::
  -- | 'geofenceId'
  Prelude.Text ->
  -- | 'geometry'
  GeofenceGeometry ->
  BatchPutGeofenceRequestEntry
newBatchPutGeofenceRequestEntry
  pGeofenceId_
  pGeometry_ =
    BatchPutGeofenceRequestEntry'
      { geofenceId =
          pGeofenceId_,
        geometry = pGeometry_
      }

-- | The identifier for the geofence to be stored in a given geofence
-- collection.
batchPutGeofenceRequestEntry_geofenceId :: Lens.Lens' BatchPutGeofenceRequestEntry Prelude.Text
batchPutGeofenceRequestEntry_geofenceId = Lens.lens (\BatchPutGeofenceRequestEntry' {geofenceId} -> geofenceId) (\s@BatchPutGeofenceRequestEntry' {} a -> s {geofenceId = a} :: BatchPutGeofenceRequestEntry)

-- | Contains the details of the position of the geofence. Can be either a
-- polygon or a circle. Including both will return a validation error.
--
-- Each
-- <https://docs.aws.amazon.com/location-geofences/latest/APIReference/API_GeofenceGeometry.html geofence polygon>
-- can have a maximum of 1,000 vertices.
batchPutGeofenceRequestEntry_geometry :: Lens.Lens' BatchPutGeofenceRequestEntry GeofenceGeometry
batchPutGeofenceRequestEntry_geometry = Lens.lens (\BatchPutGeofenceRequestEntry' {geometry} -> geometry) (\s@BatchPutGeofenceRequestEntry' {} a -> s {geometry = a} :: BatchPutGeofenceRequestEntry)

instance
  Prelude.Hashable
    BatchPutGeofenceRequestEntry
  where
  hashWithSalt _salt BatchPutGeofenceRequestEntry' {..} =
    _salt `Prelude.hashWithSalt` geofenceId
      `Prelude.hashWithSalt` geometry

instance Prelude.NFData BatchPutGeofenceRequestEntry where
  rnf BatchPutGeofenceRequestEntry' {..} =
    Prelude.rnf geofenceId
      `Prelude.seq` Prelude.rnf geometry

instance Data.ToJSON BatchPutGeofenceRequestEntry where
  toJSON BatchPutGeofenceRequestEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GeofenceId" Data..= geofenceId),
            Prelude.Just ("Geometry" Data..= geometry)
          ]
      )
