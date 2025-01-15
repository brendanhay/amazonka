{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.BatchPutGeofence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A batch request for storing geofence geometries into a given geofence
-- collection, or updates the geometry of an existing geofence if a
-- geofence ID is included in the request.
module Amazonka.Location.BatchPutGeofence
  ( -- * Creating a Request
    BatchPutGeofence (..),
    newBatchPutGeofence,

    -- * Request Lenses
    batchPutGeofence_collectionName,
    batchPutGeofence_entries,

    -- * Destructuring the Response
    BatchPutGeofenceResponse (..),
    newBatchPutGeofenceResponse,

    -- * Response Lenses
    batchPutGeofenceResponse_httpStatus,
    batchPutGeofenceResponse_errors,
    batchPutGeofenceResponse_successes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutGeofence' smart constructor.
data BatchPutGeofence = BatchPutGeofence'
  { -- | The geofence collection storing the geofences.
    collectionName :: Prelude.Text,
    -- | The batch of geofences to be stored in a geofence collection.
    entries :: Prelude.NonEmpty BatchPutGeofenceRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutGeofence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'batchPutGeofence_collectionName' - The geofence collection storing the geofences.
--
-- 'entries', 'batchPutGeofence_entries' - The batch of geofences to be stored in a geofence collection.
newBatchPutGeofence ::
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'entries'
  Prelude.NonEmpty BatchPutGeofenceRequestEntry ->
  BatchPutGeofence
newBatchPutGeofence pCollectionName_ pEntries_ =
  BatchPutGeofence'
    { collectionName =
        pCollectionName_,
      entries = Lens.coerced Lens.# pEntries_
    }

-- | The geofence collection storing the geofences.
batchPutGeofence_collectionName :: Lens.Lens' BatchPutGeofence Prelude.Text
batchPutGeofence_collectionName = Lens.lens (\BatchPutGeofence' {collectionName} -> collectionName) (\s@BatchPutGeofence' {} a -> s {collectionName = a} :: BatchPutGeofence)

-- | The batch of geofences to be stored in a geofence collection.
batchPutGeofence_entries :: Lens.Lens' BatchPutGeofence (Prelude.NonEmpty BatchPutGeofenceRequestEntry)
batchPutGeofence_entries = Lens.lens (\BatchPutGeofence' {entries} -> entries) (\s@BatchPutGeofence' {} a -> s {entries = a} :: BatchPutGeofence) Prelude.. Lens.coerced

instance Core.AWSRequest BatchPutGeofence where
  type
    AWSResponse BatchPutGeofence =
      BatchPutGeofenceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutGeofenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Successes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchPutGeofence where
  hashWithSalt _salt BatchPutGeofence' {..} =
    _salt
      `Prelude.hashWithSalt` collectionName
      `Prelude.hashWithSalt` entries

instance Prelude.NFData BatchPutGeofence where
  rnf BatchPutGeofence' {..} =
    Prelude.rnf collectionName `Prelude.seq`
      Prelude.rnf entries

instance Data.ToHeaders BatchPutGeofence where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchPutGeofence where
  toJSON BatchPutGeofence' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Entries" Data..= entries)]
      )

instance Data.ToPath BatchPutGeofence where
  toPath BatchPutGeofence' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Data.toBS collectionName,
        "/put-geofences"
      ]

instance Data.ToQuery BatchPutGeofence where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutGeofenceResponse' smart constructor.
data BatchPutGeofenceResponse = BatchPutGeofenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains additional error details for each geofence that failed to be
    -- stored in a geofence collection.
    errors :: [BatchPutGeofenceError],
    -- | Contains each geofence that was successfully stored in a geofence
    -- collection.
    successes :: [BatchPutGeofenceSuccess]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutGeofenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchPutGeofenceResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchPutGeofenceResponse_errors' - Contains additional error details for each geofence that failed to be
-- stored in a geofence collection.
--
-- 'successes', 'batchPutGeofenceResponse_successes' - Contains each geofence that was successfully stored in a geofence
-- collection.
newBatchPutGeofenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchPutGeofenceResponse
newBatchPutGeofenceResponse pHttpStatus_ =
  BatchPutGeofenceResponse'
    { httpStatus =
        pHttpStatus_,
      errors = Prelude.mempty,
      successes = Prelude.mempty
    }

-- | The response's http status code.
batchPutGeofenceResponse_httpStatus :: Lens.Lens' BatchPutGeofenceResponse Prelude.Int
batchPutGeofenceResponse_httpStatus = Lens.lens (\BatchPutGeofenceResponse' {httpStatus} -> httpStatus) (\s@BatchPutGeofenceResponse' {} a -> s {httpStatus = a} :: BatchPutGeofenceResponse)

-- | Contains additional error details for each geofence that failed to be
-- stored in a geofence collection.
batchPutGeofenceResponse_errors :: Lens.Lens' BatchPutGeofenceResponse [BatchPutGeofenceError]
batchPutGeofenceResponse_errors = Lens.lens (\BatchPutGeofenceResponse' {errors} -> errors) (\s@BatchPutGeofenceResponse' {} a -> s {errors = a} :: BatchPutGeofenceResponse) Prelude.. Lens.coerced

-- | Contains each geofence that was successfully stored in a geofence
-- collection.
batchPutGeofenceResponse_successes :: Lens.Lens' BatchPutGeofenceResponse [BatchPutGeofenceSuccess]
batchPutGeofenceResponse_successes = Lens.lens (\BatchPutGeofenceResponse' {successes} -> successes) (\s@BatchPutGeofenceResponse' {} a -> s {successes = a} :: BatchPutGeofenceResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchPutGeofenceResponse where
  rnf BatchPutGeofenceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf errors `Prelude.seq`
        Prelude.rnf successes
