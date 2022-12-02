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
-- Module      : Amazonka.Location.BatchDeleteDevicePositionHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the position history of one or more devices from a tracker
-- resource.
module Amazonka.Location.BatchDeleteDevicePositionHistory
  ( -- * Creating a Request
    BatchDeleteDevicePositionHistory (..),
    newBatchDeleteDevicePositionHistory,

    -- * Request Lenses
    batchDeleteDevicePositionHistory_deviceIds,
    batchDeleteDevicePositionHistory_trackerName,

    -- * Destructuring the Response
    BatchDeleteDevicePositionHistoryResponse (..),
    newBatchDeleteDevicePositionHistoryResponse,

    -- * Response Lenses
    batchDeleteDevicePositionHistoryResponse_httpStatus,
    batchDeleteDevicePositionHistoryResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteDevicePositionHistory' smart constructor.
data BatchDeleteDevicePositionHistory = BatchDeleteDevicePositionHistory'
  { -- | Devices whose position history you want to delete.
    --
    -- -   For example, for two devices: @“DeviceIds” : [DeviceId1,DeviceId2]@
    deviceIds :: Prelude.NonEmpty Prelude.Text,
    -- | The name of the tracker resource to delete the device position history
    -- from.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDevicePositionHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceIds', 'batchDeleteDevicePositionHistory_deviceIds' - Devices whose position history you want to delete.
--
-- -   For example, for two devices: @“DeviceIds” : [DeviceId1,DeviceId2]@
--
-- 'trackerName', 'batchDeleteDevicePositionHistory_trackerName' - The name of the tracker resource to delete the device position history
-- from.
newBatchDeleteDevicePositionHistory ::
  -- | 'deviceIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  BatchDeleteDevicePositionHistory
newBatchDeleteDevicePositionHistory
  pDeviceIds_
  pTrackerName_ =
    BatchDeleteDevicePositionHistory'
      { deviceIds =
          Lens.coerced Lens.# pDeviceIds_,
        trackerName = pTrackerName_
      }

-- | Devices whose position history you want to delete.
--
-- -   For example, for two devices: @“DeviceIds” : [DeviceId1,DeviceId2]@
batchDeleteDevicePositionHistory_deviceIds :: Lens.Lens' BatchDeleteDevicePositionHistory (Prelude.NonEmpty Prelude.Text)
batchDeleteDevicePositionHistory_deviceIds = Lens.lens (\BatchDeleteDevicePositionHistory' {deviceIds} -> deviceIds) (\s@BatchDeleteDevicePositionHistory' {} a -> s {deviceIds = a} :: BatchDeleteDevicePositionHistory) Prelude.. Lens.coerced

-- | The name of the tracker resource to delete the device position history
-- from.
batchDeleteDevicePositionHistory_trackerName :: Lens.Lens' BatchDeleteDevicePositionHistory Prelude.Text
batchDeleteDevicePositionHistory_trackerName = Lens.lens (\BatchDeleteDevicePositionHistory' {trackerName} -> trackerName) (\s@BatchDeleteDevicePositionHistory' {} a -> s {trackerName = a} :: BatchDeleteDevicePositionHistory)

instance
  Core.AWSRequest
    BatchDeleteDevicePositionHistory
  where
  type
    AWSResponse BatchDeleteDevicePositionHistory =
      BatchDeleteDevicePositionHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteDevicePositionHistoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchDeleteDevicePositionHistory
  where
  hashWithSalt
    _salt
    BatchDeleteDevicePositionHistory' {..} =
      _salt `Prelude.hashWithSalt` deviceIds
        `Prelude.hashWithSalt` trackerName

instance
  Prelude.NFData
    BatchDeleteDevicePositionHistory
  where
  rnf BatchDeleteDevicePositionHistory' {..} =
    Prelude.rnf deviceIds
      `Prelude.seq` Prelude.rnf trackerName

instance
  Data.ToHeaders
    BatchDeleteDevicePositionHistory
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteDevicePositionHistory where
  toJSON BatchDeleteDevicePositionHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DeviceIds" Data..= deviceIds)]
      )

instance Data.ToPath BatchDeleteDevicePositionHistory where
  toPath BatchDeleteDevicePositionHistory' {..} =
    Prelude.mconcat
      [ "/tracking/v0/trackers/",
        Data.toBS trackerName,
        "/delete-positions"
      ]

instance
  Data.ToQuery
    BatchDeleteDevicePositionHistory
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteDevicePositionHistoryResponse' smart constructor.
data BatchDeleteDevicePositionHistoryResponse = BatchDeleteDevicePositionHistoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains error details for each device history that failed to delete.
    errors :: [BatchDeleteDevicePositionHistoryError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDevicePositionHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchDeleteDevicePositionHistoryResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'batchDeleteDevicePositionHistoryResponse_errors' - Contains error details for each device history that failed to delete.
newBatchDeleteDevicePositionHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteDevicePositionHistoryResponse
newBatchDeleteDevicePositionHistoryResponse
  pHttpStatus_ =
    BatchDeleteDevicePositionHistoryResponse'
      { httpStatus =
          pHttpStatus_,
        errors = Prelude.mempty
      }

-- | The response's http status code.
batchDeleteDevicePositionHistoryResponse_httpStatus :: Lens.Lens' BatchDeleteDevicePositionHistoryResponse Prelude.Int
batchDeleteDevicePositionHistoryResponse_httpStatus = Lens.lens (\BatchDeleteDevicePositionHistoryResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteDevicePositionHistoryResponse' {} a -> s {httpStatus = a} :: BatchDeleteDevicePositionHistoryResponse)

-- | Contains error details for each device history that failed to delete.
batchDeleteDevicePositionHistoryResponse_errors :: Lens.Lens' BatchDeleteDevicePositionHistoryResponse [BatchDeleteDevicePositionHistoryError]
batchDeleteDevicePositionHistoryResponse_errors = Lens.lens (\BatchDeleteDevicePositionHistoryResponse' {errors} -> errors) (\s@BatchDeleteDevicePositionHistoryResponse' {} a -> s {errors = a} :: BatchDeleteDevicePositionHistoryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchDeleteDevicePositionHistoryResponse
  where
  rnf BatchDeleteDevicePositionHistoryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
