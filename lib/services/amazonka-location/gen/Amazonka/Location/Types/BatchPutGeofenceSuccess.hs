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
-- Module      : Amazonka.Location.Types.BatchPutGeofenceSuccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchPutGeofenceSuccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of each geofence that was successfully stored in a
-- given geofence collection.
--
-- /See:/ 'newBatchPutGeofenceSuccess' smart constructor.
data BatchPutGeofenceSuccess = BatchPutGeofenceSuccess'
  { -- | The timestamp for when the geofence was stored in a geofence collection
    -- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    createTime :: Data.ISO8601,
    -- | The geofence successfully stored in a geofence collection.
    geofenceId :: Prelude.Text,
    -- | The timestamp for when the geofence was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutGeofenceSuccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'batchPutGeofenceSuccess_createTime' - The timestamp for when the geofence was stored in a geofence collection
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
--
-- 'geofenceId', 'batchPutGeofenceSuccess_geofenceId' - The geofence successfully stored in a geofence collection.
--
-- 'updateTime', 'batchPutGeofenceSuccess_updateTime' - The timestamp for when the geofence was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newBatchPutGeofenceSuccess ::
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'geofenceId'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  BatchPutGeofenceSuccess
newBatchPutGeofenceSuccess
  pCreateTime_
  pGeofenceId_
  pUpdateTime_ =
    BatchPutGeofenceSuccess'
      { createTime =
          Data._Time Lens.# pCreateTime_,
        geofenceId = pGeofenceId_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The timestamp for when the geofence was stored in a geofence collection
-- in <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
batchPutGeofenceSuccess_createTime :: Lens.Lens' BatchPutGeofenceSuccess Prelude.UTCTime
batchPutGeofenceSuccess_createTime = Lens.lens (\BatchPutGeofenceSuccess' {createTime} -> createTime) (\s@BatchPutGeofenceSuccess' {} a -> s {createTime = a} :: BatchPutGeofenceSuccess) Prelude.. Data._Time

-- | The geofence successfully stored in a geofence collection.
batchPutGeofenceSuccess_geofenceId :: Lens.Lens' BatchPutGeofenceSuccess Prelude.Text
batchPutGeofenceSuccess_geofenceId = Lens.lens (\BatchPutGeofenceSuccess' {geofenceId} -> geofenceId) (\s@BatchPutGeofenceSuccess' {} a -> s {geofenceId = a} :: BatchPutGeofenceSuccess)

-- | The timestamp for when the geofence was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
batchPutGeofenceSuccess_updateTime :: Lens.Lens' BatchPutGeofenceSuccess Prelude.UTCTime
batchPutGeofenceSuccess_updateTime = Lens.lens (\BatchPutGeofenceSuccess' {updateTime} -> updateTime) (\s@BatchPutGeofenceSuccess' {} a -> s {updateTime = a} :: BatchPutGeofenceSuccess) Prelude.. Data._Time

instance Data.FromJSON BatchPutGeofenceSuccess where
  parseJSON =
    Data.withObject
      "BatchPutGeofenceSuccess"
      ( \x ->
          BatchPutGeofenceSuccess'
            Prelude.<$> (x Data..: "CreateTime")
            Prelude.<*> (x Data..: "GeofenceId")
            Prelude.<*> (x Data..: "UpdateTime")
      )

instance Prelude.Hashable BatchPutGeofenceSuccess where
  hashWithSalt _salt BatchPutGeofenceSuccess' {..} =
    _salt `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` geofenceId
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData BatchPutGeofenceSuccess where
  rnf BatchPutGeofenceSuccess' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf geofenceId
      `Prelude.seq` Prelude.rnf updateTime
