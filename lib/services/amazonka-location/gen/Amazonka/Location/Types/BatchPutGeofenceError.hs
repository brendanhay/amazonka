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
-- Module      : Amazonka.Location.Types.BatchPutGeofenceError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchPutGeofenceError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.BatchItemError
import qualified Amazonka.Prelude as Prelude

-- | Contains error details for each geofence that failed to be stored in a
-- given geofence collection.
--
-- /See:/ 'newBatchPutGeofenceError' smart constructor.
data BatchPutGeofenceError = BatchPutGeofenceError'
  { -- | Contains details associated to the batch error.
    error :: BatchItemError,
    -- | The geofence associated with the error message.
    geofenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutGeofenceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'batchPutGeofenceError_error' - Contains details associated to the batch error.
--
-- 'geofenceId', 'batchPutGeofenceError_geofenceId' - The geofence associated with the error message.
newBatchPutGeofenceError ::
  -- | 'error'
  BatchItemError ->
  -- | 'geofenceId'
  Prelude.Text ->
  BatchPutGeofenceError
newBatchPutGeofenceError pError_ pGeofenceId_ =
  BatchPutGeofenceError'
    { error = pError_,
      geofenceId = pGeofenceId_
    }

-- | Contains details associated to the batch error.
batchPutGeofenceError_error :: Lens.Lens' BatchPutGeofenceError BatchItemError
batchPutGeofenceError_error = Lens.lens (\BatchPutGeofenceError' {error} -> error) (\s@BatchPutGeofenceError' {} a -> s {error = a} :: BatchPutGeofenceError)

-- | The geofence associated with the error message.
batchPutGeofenceError_geofenceId :: Lens.Lens' BatchPutGeofenceError Prelude.Text
batchPutGeofenceError_geofenceId = Lens.lens (\BatchPutGeofenceError' {geofenceId} -> geofenceId) (\s@BatchPutGeofenceError' {} a -> s {geofenceId = a} :: BatchPutGeofenceError)

instance Data.FromJSON BatchPutGeofenceError where
  parseJSON =
    Data.withObject
      "BatchPutGeofenceError"
      ( \x ->
          BatchPutGeofenceError'
            Prelude.<$> (x Data..: "Error")
            Prelude.<*> (x Data..: "GeofenceId")
      )

instance Prelude.Hashable BatchPutGeofenceError where
  hashWithSalt _salt BatchPutGeofenceError' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` geofenceId

instance Prelude.NFData BatchPutGeofenceError where
  rnf BatchPutGeofenceError' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf geofenceId
