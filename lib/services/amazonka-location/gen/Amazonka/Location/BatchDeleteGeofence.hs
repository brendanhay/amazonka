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
-- Module      : Amazonka.Location.BatchDeleteGeofence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a batch of geofences from a geofence collection.
--
-- This operation deletes the resource permanently.
module Amazonka.Location.BatchDeleteGeofence
  ( -- * Creating a Request
    BatchDeleteGeofence (..),
    newBatchDeleteGeofence,

    -- * Request Lenses
    batchDeleteGeofence_collectionName,
    batchDeleteGeofence_geofenceIds,

    -- * Destructuring the Response
    BatchDeleteGeofenceResponse (..),
    newBatchDeleteGeofenceResponse,

    -- * Response Lenses
    batchDeleteGeofenceResponse_httpStatus,
    batchDeleteGeofenceResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteGeofence' smart constructor.
data BatchDeleteGeofence = BatchDeleteGeofence'
  { -- | The geofence collection storing the geofences to be deleted.
    collectionName :: Prelude.Text,
    -- | The batch of geofences to be deleted.
    geofenceIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteGeofence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'batchDeleteGeofence_collectionName' - The geofence collection storing the geofences to be deleted.
--
-- 'geofenceIds', 'batchDeleteGeofence_geofenceIds' - The batch of geofences to be deleted.
newBatchDeleteGeofence ::
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'geofenceIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeleteGeofence
newBatchDeleteGeofence pCollectionName_ pGeofenceIds_ =
  BatchDeleteGeofence'
    { collectionName =
        pCollectionName_,
      geofenceIds = Lens.coerced Lens.# pGeofenceIds_
    }

-- | The geofence collection storing the geofences to be deleted.
batchDeleteGeofence_collectionName :: Lens.Lens' BatchDeleteGeofence Prelude.Text
batchDeleteGeofence_collectionName = Lens.lens (\BatchDeleteGeofence' {collectionName} -> collectionName) (\s@BatchDeleteGeofence' {} a -> s {collectionName = a} :: BatchDeleteGeofence)

-- | The batch of geofences to be deleted.
batchDeleteGeofence_geofenceIds :: Lens.Lens' BatchDeleteGeofence (Prelude.NonEmpty Prelude.Text)
batchDeleteGeofence_geofenceIds = Lens.lens (\BatchDeleteGeofence' {geofenceIds} -> geofenceIds) (\s@BatchDeleteGeofence' {} a -> s {geofenceIds = a} :: BatchDeleteGeofence) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteGeofence where
  type
    AWSResponse BatchDeleteGeofence =
      BatchDeleteGeofenceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteGeofenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchDeleteGeofence where
  hashWithSalt _salt BatchDeleteGeofence' {..} =
    _salt
      `Prelude.hashWithSalt` collectionName
      `Prelude.hashWithSalt` geofenceIds

instance Prelude.NFData BatchDeleteGeofence where
  rnf BatchDeleteGeofence' {..} =
    Prelude.rnf collectionName
      `Prelude.seq` Prelude.rnf geofenceIds

instance Data.ToHeaders BatchDeleteGeofence where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteGeofence where
  toJSON BatchDeleteGeofence' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GeofenceIds" Data..= geofenceIds)]
      )

instance Data.ToPath BatchDeleteGeofence where
  toPath BatchDeleteGeofence' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Data.toBS collectionName,
        "/delete-geofences"
      ]

instance Data.ToQuery BatchDeleteGeofence where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteGeofenceResponse' smart constructor.
data BatchDeleteGeofenceResponse = BatchDeleteGeofenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains error details for each geofence that failed to delete.
    errors :: [BatchDeleteGeofenceError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteGeofenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDeleteGeofenceResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchDeleteGeofenceResponse_errors' - Contains error details for each geofence that failed to delete.
newBatchDeleteGeofenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteGeofenceResponse
newBatchDeleteGeofenceResponse pHttpStatus_ =
  BatchDeleteGeofenceResponse'
    { httpStatus =
        pHttpStatus_,
      errors = Prelude.mempty
    }

-- | The response's http status code.
batchDeleteGeofenceResponse_httpStatus :: Lens.Lens' BatchDeleteGeofenceResponse Prelude.Int
batchDeleteGeofenceResponse_httpStatus = Lens.lens (\BatchDeleteGeofenceResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteGeofenceResponse' {} a -> s {httpStatus = a} :: BatchDeleteGeofenceResponse)

-- | Contains error details for each geofence that failed to delete.
batchDeleteGeofenceResponse_errors :: Lens.Lens' BatchDeleteGeofenceResponse [BatchDeleteGeofenceError]
batchDeleteGeofenceResponse_errors = Lens.lens (\BatchDeleteGeofenceResponse' {errors} -> errors) (\s@BatchDeleteGeofenceResponse' {} a -> s {errors = a} :: BatchDeleteGeofenceResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchDeleteGeofenceResponse where
  rnf BatchDeleteGeofenceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
