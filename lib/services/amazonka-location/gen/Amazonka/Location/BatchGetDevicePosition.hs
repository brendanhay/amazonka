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
-- Module      : Amazonka.Location.BatchGetDevicePosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the latest device positions for requested devices.
module Amazonka.Location.BatchGetDevicePosition
  ( -- * Creating a Request
    BatchGetDevicePosition (..),
    newBatchGetDevicePosition,

    -- * Request Lenses
    batchGetDevicePosition_deviceIds,
    batchGetDevicePosition_trackerName,

    -- * Destructuring the Response
    BatchGetDevicePositionResponse (..),
    newBatchGetDevicePositionResponse,

    -- * Response Lenses
    batchGetDevicePositionResponse_httpStatus,
    batchGetDevicePositionResponse_devicePositions,
    batchGetDevicePositionResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetDevicePosition' smart constructor.
data BatchGetDevicePosition = BatchGetDevicePosition'
  { -- | Devices whose position you want to retrieve.
    --
    -- -   For example, for two devices:
    --     @device-ids=DeviceId1&device-ids=DeviceId2@
    deviceIds :: Prelude.NonEmpty Prelude.Text,
    -- | The tracker resource retrieving the device position.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDevicePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceIds', 'batchGetDevicePosition_deviceIds' - Devices whose position you want to retrieve.
--
-- -   For example, for two devices:
--     @device-ids=DeviceId1&device-ids=DeviceId2@
--
-- 'trackerName', 'batchGetDevicePosition_trackerName' - The tracker resource retrieving the device position.
newBatchGetDevicePosition ::
  -- | 'deviceIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  BatchGetDevicePosition
newBatchGetDevicePosition pDeviceIds_ pTrackerName_ =
  BatchGetDevicePosition'
    { deviceIds =
        Lens.coerced Lens.# pDeviceIds_,
      trackerName = pTrackerName_
    }

-- | Devices whose position you want to retrieve.
--
-- -   For example, for two devices:
--     @device-ids=DeviceId1&device-ids=DeviceId2@
batchGetDevicePosition_deviceIds :: Lens.Lens' BatchGetDevicePosition (Prelude.NonEmpty Prelude.Text)
batchGetDevicePosition_deviceIds = Lens.lens (\BatchGetDevicePosition' {deviceIds} -> deviceIds) (\s@BatchGetDevicePosition' {} a -> s {deviceIds = a} :: BatchGetDevicePosition) Prelude.. Lens.coerced

-- | The tracker resource retrieving the device position.
batchGetDevicePosition_trackerName :: Lens.Lens' BatchGetDevicePosition Prelude.Text
batchGetDevicePosition_trackerName = Lens.lens (\BatchGetDevicePosition' {trackerName} -> trackerName) (\s@BatchGetDevicePosition' {} a -> s {trackerName = a} :: BatchGetDevicePosition)

instance Core.AWSRequest BatchGetDevicePosition where
  type
    AWSResponse BatchGetDevicePosition =
      BatchGetDevicePositionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDevicePositionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "DevicePositions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchGetDevicePosition where
  hashWithSalt _salt BatchGetDevicePosition' {..} =
    _salt `Prelude.hashWithSalt` deviceIds
      `Prelude.hashWithSalt` trackerName

instance Prelude.NFData BatchGetDevicePosition where
  rnf BatchGetDevicePosition' {..} =
    Prelude.rnf deviceIds
      `Prelude.seq` Prelude.rnf trackerName

instance Core.ToHeaders BatchGetDevicePosition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetDevicePosition where
  toJSON BatchGetDevicePosition' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DeviceIds" Core..= deviceIds)]
      )

instance Core.ToPath BatchGetDevicePosition where
  toPath BatchGetDevicePosition' {..} =
    Prelude.mconcat
      [ "/tracking/v0/trackers/",
        Core.toBS trackerName,
        "/get-positions"
      ]

instance Core.ToQuery BatchGetDevicePosition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetDevicePositionResponse' smart constructor.
data BatchGetDevicePositionResponse = BatchGetDevicePositionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains device position details such as the device ID, position, and
    -- timestamps for when the position was received and sampled.
    devicePositions :: [DevicePosition],
    -- | Contains error details for each device that failed to send its position
    -- to the tracker resource.
    errors :: [BatchGetDevicePositionError]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDevicePositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchGetDevicePositionResponse_httpStatus' - The response's http status code.
--
-- 'devicePositions', 'batchGetDevicePositionResponse_devicePositions' - Contains device position details such as the device ID, position, and
-- timestamps for when the position was received and sampled.
--
-- 'errors', 'batchGetDevicePositionResponse_errors' - Contains error details for each device that failed to send its position
-- to the tracker resource.
newBatchGetDevicePositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetDevicePositionResponse
newBatchGetDevicePositionResponse pHttpStatus_ =
  BatchGetDevicePositionResponse'
    { httpStatus =
        pHttpStatus_,
      devicePositions = Prelude.mempty,
      errors = Prelude.mempty
    }

-- | The response's http status code.
batchGetDevicePositionResponse_httpStatus :: Lens.Lens' BatchGetDevicePositionResponse Prelude.Int
batchGetDevicePositionResponse_httpStatus = Lens.lens (\BatchGetDevicePositionResponse' {httpStatus} -> httpStatus) (\s@BatchGetDevicePositionResponse' {} a -> s {httpStatus = a} :: BatchGetDevicePositionResponse)

-- | Contains device position details such as the device ID, position, and
-- timestamps for when the position was received and sampled.
batchGetDevicePositionResponse_devicePositions :: Lens.Lens' BatchGetDevicePositionResponse [DevicePosition]
batchGetDevicePositionResponse_devicePositions = Lens.lens (\BatchGetDevicePositionResponse' {devicePositions} -> devicePositions) (\s@BatchGetDevicePositionResponse' {} a -> s {devicePositions = a} :: BatchGetDevicePositionResponse) Prelude.. Lens.coerced

-- | Contains error details for each device that failed to send its position
-- to the tracker resource.
batchGetDevicePositionResponse_errors :: Lens.Lens' BatchGetDevicePositionResponse [BatchGetDevicePositionError]
batchGetDevicePositionResponse_errors = Lens.lens (\BatchGetDevicePositionResponse' {errors} -> errors) (\s@BatchGetDevicePositionResponse' {} a -> s {errors = a} :: BatchGetDevicePositionResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetDevicePositionResponse
  where
  rnf BatchGetDevicePositionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf devicePositions
      `Prelude.seq` Prelude.rnf errors
