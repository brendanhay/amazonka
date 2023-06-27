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
-- Module      : Amazonka.Location.BatchEvaluateGeofences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Evaluates device positions against the geofence geometries from a given
-- geofence collection.
--
-- This operation always returns an empty response because geofences are
-- asynchronously evaluated. The evaluation determines if the device has
-- entered or exited a geofenced area, and then publishes one of the
-- following events to Amazon EventBridge:
--
-- -   @ENTER@ if Amazon Location determines that the tracked device has
--     entered a geofenced area.
--
-- -   @EXIT@ if Amazon Location determines that the tracked device has
--     exited a geofenced area.
--
-- The last geofence that a device was observed within is tracked for 30
-- days after the most recent device position update.
--
-- Geofence evaluation uses the given device position. It does not account
-- for the optional @Accuracy@ of a @DevicePositionUpdate@.
--
-- The @DeviceID@ is used as a string to represent the device. You do not
-- need to have a @Tracker@ associated with the @DeviceID@.
module Amazonka.Location.BatchEvaluateGeofences
  ( -- * Creating a Request
    BatchEvaluateGeofences (..),
    newBatchEvaluateGeofences,

    -- * Request Lenses
    batchEvaluateGeofences_collectionName,
    batchEvaluateGeofences_devicePositionUpdates,

    -- * Destructuring the Response
    BatchEvaluateGeofencesResponse (..),
    newBatchEvaluateGeofencesResponse,

    -- * Response Lenses
    batchEvaluateGeofencesResponse_httpStatus,
    batchEvaluateGeofencesResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchEvaluateGeofences' smart constructor.
data BatchEvaluateGeofences = BatchEvaluateGeofences'
  { -- | The geofence collection used in evaluating the position of devices
    -- against its geofences.
    collectionName :: Prelude.Text,
    -- | Contains device details for each device to be evaluated against the
    -- given geofence collection.
    devicePositionUpdates :: Prelude.NonEmpty DevicePositionUpdate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEvaluateGeofences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'batchEvaluateGeofences_collectionName' - The geofence collection used in evaluating the position of devices
-- against its geofences.
--
-- 'devicePositionUpdates', 'batchEvaluateGeofences_devicePositionUpdates' - Contains device details for each device to be evaluated against the
-- given geofence collection.
newBatchEvaluateGeofences ::
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'devicePositionUpdates'
  Prelude.NonEmpty DevicePositionUpdate ->
  BatchEvaluateGeofences
newBatchEvaluateGeofences
  pCollectionName_
  pDevicePositionUpdates_ =
    BatchEvaluateGeofences'
      { collectionName =
          pCollectionName_,
        devicePositionUpdates =
          Lens.coerced Lens.# pDevicePositionUpdates_
      }

-- | The geofence collection used in evaluating the position of devices
-- against its geofences.
batchEvaluateGeofences_collectionName :: Lens.Lens' BatchEvaluateGeofences Prelude.Text
batchEvaluateGeofences_collectionName = Lens.lens (\BatchEvaluateGeofences' {collectionName} -> collectionName) (\s@BatchEvaluateGeofences' {} a -> s {collectionName = a} :: BatchEvaluateGeofences)

-- | Contains device details for each device to be evaluated against the
-- given geofence collection.
batchEvaluateGeofences_devicePositionUpdates :: Lens.Lens' BatchEvaluateGeofences (Prelude.NonEmpty DevicePositionUpdate)
batchEvaluateGeofences_devicePositionUpdates = Lens.lens (\BatchEvaluateGeofences' {devicePositionUpdates} -> devicePositionUpdates) (\s@BatchEvaluateGeofences' {} a -> s {devicePositionUpdates = a} :: BatchEvaluateGeofences) Prelude.. Lens.coerced

instance Core.AWSRequest BatchEvaluateGeofences where
  type
    AWSResponse BatchEvaluateGeofences =
      BatchEvaluateGeofencesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchEvaluateGeofencesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchEvaluateGeofences where
  hashWithSalt _salt BatchEvaluateGeofences' {..} =
    _salt
      `Prelude.hashWithSalt` collectionName
      `Prelude.hashWithSalt` devicePositionUpdates

instance Prelude.NFData BatchEvaluateGeofences where
  rnf BatchEvaluateGeofences' {..} =
    Prelude.rnf collectionName
      `Prelude.seq` Prelude.rnf devicePositionUpdates

instance Data.ToHeaders BatchEvaluateGeofences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchEvaluateGeofences where
  toJSON BatchEvaluateGeofences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "DevicePositionUpdates"
                  Data..= devicePositionUpdates
              )
          ]
      )

instance Data.ToPath BatchEvaluateGeofences where
  toPath BatchEvaluateGeofences' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Data.toBS collectionName,
        "/positions"
      ]

instance Data.ToQuery BatchEvaluateGeofences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchEvaluateGeofencesResponse' smart constructor.
data BatchEvaluateGeofencesResponse = BatchEvaluateGeofencesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains error details for each device that failed to evaluate its
    -- position against the given geofence collection.
    errors :: [BatchEvaluateGeofencesError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchEvaluateGeofencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchEvaluateGeofencesResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchEvaluateGeofencesResponse_errors' - Contains error details for each device that failed to evaluate its
-- position against the given geofence collection.
newBatchEvaluateGeofencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchEvaluateGeofencesResponse
newBatchEvaluateGeofencesResponse pHttpStatus_ =
  BatchEvaluateGeofencesResponse'
    { httpStatus =
        pHttpStatus_,
      errors = Prelude.mempty
    }

-- | The response's http status code.
batchEvaluateGeofencesResponse_httpStatus :: Lens.Lens' BatchEvaluateGeofencesResponse Prelude.Int
batchEvaluateGeofencesResponse_httpStatus = Lens.lens (\BatchEvaluateGeofencesResponse' {httpStatus} -> httpStatus) (\s@BatchEvaluateGeofencesResponse' {} a -> s {httpStatus = a} :: BatchEvaluateGeofencesResponse)

-- | Contains error details for each device that failed to evaluate its
-- position against the given geofence collection.
batchEvaluateGeofencesResponse_errors :: Lens.Lens' BatchEvaluateGeofencesResponse [BatchEvaluateGeofencesError]
batchEvaluateGeofencesResponse_errors = Lens.lens (\BatchEvaluateGeofencesResponse' {errors} -> errors) (\s@BatchEvaluateGeofencesResponse' {} a -> s {errors = a} :: BatchEvaluateGeofencesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchEvaluateGeofencesResponse
  where
  rnf BatchEvaluateGeofencesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
