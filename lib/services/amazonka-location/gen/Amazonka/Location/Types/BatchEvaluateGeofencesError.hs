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
-- Module      : Amazonka.Location.Types.BatchEvaluateGeofencesError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchEvaluateGeofencesError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.BatchItemError
import qualified Amazonka.Prelude as Prelude

-- | Contains error details for each device that failed to evaluate its
-- position against the geofences in a given geofence collection.
--
-- /See:/ 'newBatchEvaluateGeofencesError' smart constructor.
data BatchEvaluateGeofencesError = BatchEvaluateGeofencesError'
  { -- | The device associated with the position evaluation error.
    deviceId :: Prelude.Text,
    -- | Contains details associated to the batch error.
    error :: BatchItemError,
    -- | Specifies a timestamp for when the error occurred in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    sampleTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEvaluateGeofencesError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'batchEvaluateGeofencesError_deviceId' - The device associated with the position evaluation error.
--
-- 'error', 'batchEvaluateGeofencesError_error' - Contains details associated to the batch error.
--
-- 'sampleTime', 'batchEvaluateGeofencesError_sampleTime' - Specifies a timestamp for when the error occurred in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newBatchEvaluateGeofencesError ::
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'error'
  BatchItemError ->
  -- | 'sampleTime'
  Prelude.UTCTime ->
  BatchEvaluateGeofencesError
newBatchEvaluateGeofencesError
  pDeviceId_
  pError_
  pSampleTime_ =
    BatchEvaluateGeofencesError'
      { deviceId = pDeviceId_,
        error = pError_,
        sampleTime = Data._Time Lens.# pSampleTime_
      }

-- | The device associated with the position evaluation error.
batchEvaluateGeofencesError_deviceId :: Lens.Lens' BatchEvaluateGeofencesError Prelude.Text
batchEvaluateGeofencesError_deviceId = Lens.lens (\BatchEvaluateGeofencesError' {deviceId} -> deviceId) (\s@BatchEvaluateGeofencesError' {} a -> s {deviceId = a} :: BatchEvaluateGeofencesError)

-- | Contains details associated to the batch error.
batchEvaluateGeofencesError_error :: Lens.Lens' BatchEvaluateGeofencesError BatchItemError
batchEvaluateGeofencesError_error = Lens.lens (\BatchEvaluateGeofencesError' {error} -> error) (\s@BatchEvaluateGeofencesError' {} a -> s {error = a} :: BatchEvaluateGeofencesError)

-- | Specifies a timestamp for when the error occurred in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
batchEvaluateGeofencesError_sampleTime :: Lens.Lens' BatchEvaluateGeofencesError Prelude.UTCTime
batchEvaluateGeofencesError_sampleTime = Lens.lens (\BatchEvaluateGeofencesError' {sampleTime} -> sampleTime) (\s@BatchEvaluateGeofencesError' {} a -> s {sampleTime = a} :: BatchEvaluateGeofencesError) Prelude.. Data._Time

instance Data.FromJSON BatchEvaluateGeofencesError where
  parseJSON =
    Data.withObject
      "BatchEvaluateGeofencesError"
      ( \x ->
          BatchEvaluateGeofencesError'
            Prelude.<$> (x Data..: "DeviceId")
            Prelude.<*> (x Data..: "Error")
            Prelude.<*> (x Data..: "SampleTime")
      )

instance Prelude.Hashable BatchEvaluateGeofencesError where
  hashWithSalt _salt BatchEvaluateGeofencesError' {..} =
    _salt `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` sampleTime

instance Prelude.NFData BatchEvaluateGeofencesError where
  rnf BatchEvaluateGeofencesError' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf sampleTime
