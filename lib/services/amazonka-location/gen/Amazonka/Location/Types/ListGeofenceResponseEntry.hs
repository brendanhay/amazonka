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
-- Module      : Amazonka.Location.Types.ListGeofenceResponseEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListGeofenceResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.GeofenceGeometry
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of geofences stored in a given geofence collection.
--
-- /See:/ 'newListGeofenceResponseEntry' smart constructor.
data ListGeofenceResponseEntry = ListGeofenceResponseEntry'
  { -- | Contains additional user-defined properties stored with the geofence. An
    -- array of key-value pairs.
    geofenceProperties :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The timestamp for when the geofence was stored in a geofence collection
    -- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    createTime :: Data.ISO8601,
    -- | The geofence identifier.
    geofenceId :: Prelude.Text,
    -- | Contains the geofence geometry details describing a polygon or a circle.
    geometry :: GeofenceGeometry,
    -- | Identifies the state of the geofence. A geofence will hold one of the
    -- following states:
    --
    -- -   @ACTIVE@ — The geofence has been indexed by the system.
    --
    -- -   @PENDING@ — The geofence is being processed by the system.
    --
    -- -   @FAILED@ — The geofence failed to be indexed by the system.
    --
    -- -   @DELETED@ — The geofence has been deleted from the system index.
    --
    -- -   @DELETING@ — The geofence is being deleted from the system index.
    status :: Prelude.Text,
    -- | The timestamp for when the geofence was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeofenceResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geofenceProperties', 'listGeofenceResponseEntry_geofenceProperties' - Contains additional user-defined properties stored with the geofence. An
-- array of key-value pairs.
--
-- 'createTime', 'listGeofenceResponseEntry_createTime' - The timestamp for when the geofence was stored in a geofence collection
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
--
-- 'geofenceId', 'listGeofenceResponseEntry_geofenceId' - The geofence identifier.
--
-- 'geometry', 'listGeofenceResponseEntry_geometry' - Contains the geofence geometry details describing a polygon or a circle.
--
-- 'status', 'listGeofenceResponseEntry_status' - Identifies the state of the geofence. A geofence will hold one of the
-- following states:
--
-- -   @ACTIVE@ — The geofence has been indexed by the system.
--
-- -   @PENDING@ — The geofence is being processed by the system.
--
-- -   @FAILED@ — The geofence failed to be indexed by the system.
--
-- -   @DELETED@ — The geofence has been deleted from the system index.
--
-- -   @DELETING@ — The geofence is being deleted from the system index.
--
-- 'updateTime', 'listGeofenceResponseEntry_updateTime' - The timestamp for when the geofence was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newListGeofenceResponseEntry ::
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'geofenceId'
  Prelude.Text ->
  -- | 'geometry'
  GeofenceGeometry ->
  -- | 'status'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListGeofenceResponseEntry
newListGeofenceResponseEntry
  pCreateTime_
  pGeofenceId_
  pGeometry_
  pStatus_
  pUpdateTime_ =
    ListGeofenceResponseEntry'
      { geofenceProperties =
          Prelude.Nothing,
        createTime = Data._Time Lens.# pCreateTime_,
        geofenceId = pGeofenceId_,
        geometry = pGeometry_,
        status = pStatus_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | Contains additional user-defined properties stored with the geofence. An
-- array of key-value pairs.
listGeofenceResponseEntry_geofenceProperties :: Lens.Lens' ListGeofenceResponseEntry (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listGeofenceResponseEntry_geofenceProperties = Lens.lens (\ListGeofenceResponseEntry' {geofenceProperties} -> geofenceProperties) (\s@ListGeofenceResponseEntry' {} a -> s {geofenceProperties = a} :: ListGeofenceResponseEntry) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The timestamp for when the geofence was stored in a geofence collection
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
listGeofenceResponseEntry_createTime :: Lens.Lens' ListGeofenceResponseEntry Prelude.UTCTime
listGeofenceResponseEntry_createTime = Lens.lens (\ListGeofenceResponseEntry' {createTime} -> createTime) (\s@ListGeofenceResponseEntry' {} a -> s {createTime = a} :: ListGeofenceResponseEntry) Prelude.. Data._Time

-- | The geofence identifier.
listGeofenceResponseEntry_geofenceId :: Lens.Lens' ListGeofenceResponseEntry Prelude.Text
listGeofenceResponseEntry_geofenceId = Lens.lens (\ListGeofenceResponseEntry' {geofenceId} -> geofenceId) (\s@ListGeofenceResponseEntry' {} a -> s {geofenceId = a} :: ListGeofenceResponseEntry)

-- | Contains the geofence geometry details describing a polygon or a circle.
listGeofenceResponseEntry_geometry :: Lens.Lens' ListGeofenceResponseEntry GeofenceGeometry
listGeofenceResponseEntry_geometry = Lens.lens (\ListGeofenceResponseEntry' {geometry} -> geometry) (\s@ListGeofenceResponseEntry' {} a -> s {geometry = a} :: ListGeofenceResponseEntry)

-- | Identifies the state of the geofence. A geofence will hold one of the
-- following states:
--
-- -   @ACTIVE@ — The geofence has been indexed by the system.
--
-- -   @PENDING@ — The geofence is being processed by the system.
--
-- -   @FAILED@ — The geofence failed to be indexed by the system.
--
-- -   @DELETED@ — The geofence has been deleted from the system index.
--
-- -   @DELETING@ — The geofence is being deleted from the system index.
listGeofenceResponseEntry_status :: Lens.Lens' ListGeofenceResponseEntry Prelude.Text
listGeofenceResponseEntry_status = Lens.lens (\ListGeofenceResponseEntry' {status} -> status) (\s@ListGeofenceResponseEntry' {} a -> s {status = a} :: ListGeofenceResponseEntry)

-- | The timestamp for when the geofence was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
listGeofenceResponseEntry_updateTime :: Lens.Lens' ListGeofenceResponseEntry Prelude.UTCTime
listGeofenceResponseEntry_updateTime = Lens.lens (\ListGeofenceResponseEntry' {updateTime} -> updateTime) (\s@ListGeofenceResponseEntry' {} a -> s {updateTime = a} :: ListGeofenceResponseEntry) Prelude.. Data._Time

instance Data.FromJSON ListGeofenceResponseEntry where
  parseJSON =
    Data.withObject
      "ListGeofenceResponseEntry"
      ( \x ->
          ListGeofenceResponseEntry'
            Prelude.<$> ( x
                            Data..:? "GeofenceProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "CreateTime")
            Prelude.<*> (x Data..: "GeofenceId")
            Prelude.<*> (x Data..: "Geometry")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "UpdateTime")
      )

instance Prelude.Hashable ListGeofenceResponseEntry where
  hashWithSalt _salt ListGeofenceResponseEntry' {..} =
    _salt
      `Prelude.hashWithSalt` geofenceProperties
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` geofenceId
      `Prelude.hashWithSalt` geometry
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ListGeofenceResponseEntry where
  rnf ListGeofenceResponseEntry' {..} =
    Prelude.rnf geofenceProperties
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf geofenceId
      `Prelude.seq` Prelude.rnf geometry
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
