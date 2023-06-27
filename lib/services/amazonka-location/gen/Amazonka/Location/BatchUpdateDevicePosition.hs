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
-- Module      : Amazonka.Location.BatchUpdateDevicePosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads position update data for one or more devices to a tracker
-- resource (up to 10 devices per batch). Amazon Location uses the data
-- when it reports the last known device position and position history.
-- Amazon Location retains location data for 30 days.
--
-- Position updates are handled based on the @PositionFiltering@ property
-- of the tracker. When @PositionFiltering@ is set to @TimeBased@, updates
-- are evaluated against linked geofence collections, and location data is
-- stored at a maximum of one position per 30 second interval. If your
-- update frequency is more often than every 30 seconds, only one update
-- per 30 seconds is stored for each unique device ID.
--
-- When @PositionFiltering@ is set to @DistanceBased@ filtering, location
-- data is stored and evaluated against linked geofence collections only if
-- the device has moved more than 30 m (98.4 ft).
--
-- When @PositionFiltering@ is set to @AccuracyBased@ filtering, location
-- data is stored and evaluated against linked geofence collections only if
-- the device has moved more than the measured accuracy. For example, if
-- two consecutive updates from a device have a horizontal accuracy of 5 m
-- and 10 m, the second update is neither stored or evaluated if the device
-- has moved less than 15 m. If @PositionFiltering@ is set to
-- @AccuracyBased@ filtering, Amazon Location uses the default value
-- @{ \"Horizontal\": 0}@ when accuracy is not provided on a
-- @DevicePositionUpdate@.
module Amazonka.Location.BatchUpdateDevicePosition
  ( -- * Creating a Request
    BatchUpdateDevicePosition (..),
    newBatchUpdateDevicePosition,

    -- * Request Lenses
    batchUpdateDevicePosition_trackerName,
    batchUpdateDevicePosition_updates,

    -- * Destructuring the Response
    BatchUpdateDevicePositionResponse (..),
    newBatchUpdateDevicePositionResponse,

    -- * Response Lenses
    batchUpdateDevicePositionResponse_httpStatus,
    batchUpdateDevicePositionResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateDevicePosition' smart constructor.
data BatchUpdateDevicePosition = BatchUpdateDevicePosition'
  { -- | The name of the tracker resource to update.
    trackerName :: Prelude.Text,
    -- | Contains the position update details for each device, up to 10 devices.
    updates :: Prelude.NonEmpty DevicePositionUpdate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateDevicePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackerName', 'batchUpdateDevicePosition_trackerName' - The name of the tracker resource to update.
--
-- 'updates', 'batchUpdateDevicePosition_updates' - Contains the position update details for each device, up to 10 devices.
newBatchUpdateDevicePosition ::
  -- | 'trackerName'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty DevicePositionUpdate ->
  BatchUpdateDevicePosition
newBatchUpdateDevicePosition pTrackerName_ pUpdates_ =
  BatchUpdateDevicePosition'
    { trackerName =
        pTrackerName_,
      updates = Lens.coerced Lens.# pUpdates_
    }

-- | The name of the tracker resource to update.
batchUpdateDevicePosition_trackerName :: Lens.Lens' BatchUpdateDevicePosition Prelude.Text
batchUpdateDevicePosition_trackerName = Lens.lens (\BatchUpdateDevicePosition' {trackerName} -> trackerName) (\s@BatchUpdateDevicePosition' {} a -> s {trackerName = a} :: BatchUpdateDevicePosition)

-- | Contains the position update details for each device, up to 10 devices.
batchUpdateDevicePosition_updates :: Lens.Lens' BatchUpdateDevicePosition (Prelude.NonEmpty DevicePositionUpdate)
batchUpdateDevicePosition_updates = Lens.lens (\BatchUpdateDevicePosition' {updates} -> updates) (\s@BatchUpdateDevicePosition' {} a -> s {updates = a} :: BatchUpdateDevicePosition) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpdateDevicePosition where
  type
    AWSResponse BatchUpdateDevicePosition =
      BatchUpdateDevicePositionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateDevicePositionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchUpdateDevicePosition where
  hashWithSalt _salt BatchUpdateDevicePosition' {..} =
    _salt
      `Prelude.hashWithSalt` trackerName
      `Prelude.hashWithSalt` updates

instance Prelude.NFData BatchUpdateDevicePosition where
  rnf BatchUpdateDevicePosition' {..} =
    Prelude.rnf trackerName
      `Prelude.seq` Prelude.rnf updates

instance Data.ToHeaders BatchUpdateDevicePosition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpdateDevicePosition where
  toJSON BatchUpdateDevicePosition' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Updates" Data..= updates)]
      )

instance Data.ToPath BatchUpdateDevicePosition where
  toPath BatchUpdateDevicePosition' {..} =
    Prelude.mconcat
      [ "/tracking/v0/trackers/",
        Data.toBS trackerName,
        "/positions"
      ]

instance Data.ToQuery BatchUpdateDevicePosition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateDevicePositionResponse' smart constructor.
data BatchUpdateDevicePositionResponse = BatchUpdateDevicePositionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains error details for each device that failed to update its
    -- position.
    errors :: [BatchUpdateDevicePositionError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateDevicePositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchUpdateDevicePositionResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchUpdateDevicePositionResponse_errors' - Contains error details for each device that failed to update its
-- position.
newBatchUpdateDevicePositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateDevicePositionResponse
newBatchUpdateDevicePositionResponse pHttpStatus_ =
  BatchUpdateDevicePositionResponse'
    { httpStatus =
        pHttpStatus_,
      errors = Prelude.mempty
    }

-- | The response's http status code.
batchUpdateDevicePositionResponse_httpStatus :: Lens.Lens' BatchUpdateDevicePositionResponse Prelude.Int
batchUpdateDevicePositionResponse_httpStatus = Lens.lens (\BatchUpdateDevicePositionResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateDevicePositionResponse' {} a -> s {httpStatus = a} :: BatchUpdateDevicePositionResponse)

-- | Contains error details for each device that failed to update its
-- position.
batchUpdateDevicePositionResponse_errors :: Lens.Lens' BatchUpdateDevicePositionResponse [BatchUpdateDevicePositionError]
batchUpdateDevicePositionResponse_errors = Lens.lens (\BatchUpdateDevicePositionResponse' {errors} -> errors) (\s@BatchUpdateDevicePositionResponse' {} a -> s {errors = a} :: BatchUpdateDevicePositionResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchUpdateDevicePositionResponse
  where
  rnf BatchUpdateDevicePositionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
