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
-- Module      : Amazonka.Location.Types.BatchDeleteGeofenceError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchDeleteGeofenceError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.BatchItemError
import qualified Amazonka.Prelude as Prelude

-- | Contains error details for each geofence that failed to delete from the
-- geofence collection.
--
-- /See:/ 'newBatchDeleteGeofenceError' smart constructor.
data BatchDeleteGeofenceError = BatchDeleteGeofenceError'
  { -- | Contains details associated to the batch error.
    error :: BatchItemError,
    -- | The geofence associated with the error message.
    geofenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteGeofenceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'batchDeleteGeofenceError_error' - Contains details associated to the batch error.
--
-- 'geofenceId', 'batchDeleteGeofenceError_geofenceId' - The geofence associated with the error message.
newBatchDeleteGeofenceError ::
  -- | 'error'
  BatchItemError ->
  -- | 'geofenceId'
  Prelude.Text ->
  BatchDeleteGeofenceError
newBatchDeleteGeofenceError pError_ pGeofenceId_ =
  BatchDeleteGeofenceError'
    { error = pError_,
      geofenceId = pGeofenceId_
    }

-- | Contains details associated to the batch error.
batchDeleteGeofenceError_error :: Lens.Lens' BatchDeleteGeofenceError BatchItemError
batchDeleteGeofenceError_error = Lens.lens (\BatchDeleteGeofenceError' {error} -> error) (\s@BatchDeleteGeofenceError' {} a -> s {error = a} :: BatchDeleteGeofenceError)

-- | The geofence associated with the error message.
batchDeleteGeofenceError_geofenceId :: Lens.Lens' BatchDeleteGeofenceError Prelude.Text
batchDeleteGeofenceError_geofenceId = Lens.lens (\BatchDeleteGeofenceError' {geofenceId} -> geofenceId) (\s@BatchDeleteGeofenceError' {} a -> s {geofenceId = a} :: BatchDeleteGeofenceError)

instance Data.FromJSON BatchDeleteGeofenceError where
  parseJSON =
    Data.withObject
      "BatchDeleteGeofenceError"
      ( \x ->
          BatchDeleteGeofenceError'
            Prelude.<$> (x Data..: "Error")
            Prelude.<*> (x Data..: "GeofenceId")
      )

instance Prelude.Hashable BatchDeleteGeofenceError where
  hashWithSalt _salt BatchDeleteGeofenceError' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` geofenceId

instance Prelude.NFData BatchDeleteGeofenceError where
  rnf BatchDeleteGeofenceError' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf geofenceId
