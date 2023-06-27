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
-- Module      : Amazonka.IoTWireless.GetWirelessDeviceImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an import task and count of device onboarding
-- summary information for the import task.
module Amazonka.IoTWireless.GetWirelessDeviceImportTask
  ( -- * Creating a Request
    GetWirelessDeviceImportTask (..),
    newGetWirelessDeviceImportTask,

    -- * Request Lenses
    getWirelessDeviceImportTask_id,

    -- * Destructuring the Response
    GetWirelessDeviceImportTaskResponse (..),
    newGetWirelessDeviceImportTaskResponse,

    -- * Response Lenses
    getWirelessDeviceImportTaskResponse_arn,
    getWirelessDeviceImportTaskResponse_creationTime,
    getWirelessDeviceImportTaskResponse_destinationName,
    getWirelessDeviceImportTaskResponse_failedImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_id,
    getWirelessDeviceImportTaskResponse_initializedImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_onboardedImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_pendingImportedDeviceCount,
    getWirelessDeviceImportTaskResponse_sidewalk,
    getWirelessDeviceImportTaskResponse_status,
    getWirelessDeviceImportTaskResponse_statusReason,
    getWirelessDeviceImportTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWirelessDeviceImportTask' smart constructor.
data GetWirelessDeviceImportTask = GetWirelessDeviceImportTask'
  { -- | The identifier of the import task for which information is requested.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessDeviceImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getWirelessDeviceImportTask_id' - The identifier of the import task for which information is requested.
newGetWirelessDeviceImportTask ::
  -- | 'id'
  Prelude.Text ->
  GetWirelessDeviceImportTask
newGetWirelessDeviceImportTask pId_ =
  GetWirelessDeviceImportTask' {id = pId_}

-- | The identifier of the import task for which information is requested.
getWirelessDeviceImportTask_id :: Lens.Lens' GetWirelessDeviceImportTask Prelude.Text
getWirelessDeviceImportTask_id = Lens.lens (\GetWirelessDeviceImportTask' {id} -> id) (\s@GetWirelessDeviceImportTask' {} a -> s {id = a} :: GetWirelessDeviceImportTask)

instance Core.AWSRequest GetWirelessDeviceImportTask where
  type
    AWSResponse GetWirelessDeviceImportTask =
      GetWirelessDeviceImportTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWirelessDeviceImportTaskResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DestinationName")
            Prelude.<*> (x Data..?> "FailedImportedDeviceCount")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "InitializedImportedDeviceCount")
            Prelude.<*> (x Data..?> "OnboardedImportedDeviceCount")
            Prelude.<*> (x Data..?> "PendingImportedDeviceCount")
            Prelude.<*> (x Data..?> "Sidewalk")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWirelessDeviceImportTask where
  hashWithSalt _salt GetWirelessDeviceImportTask' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetWirelessDeviceImportTask where
  rnf GetWirelessDeviceImportTask' {..} = Prelude.rnf id

instance Data.ToHeaders GetWirelessDeviceImportTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetWirelessDeviceImportTask where
  toPath GetWirelessDeviceImportTask' {..} =
    Prelude.mconcat
      ["/wireless_device_import_task/", Data.toBS id]

instance Data.ToQuery GetWirelessDeviceImportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWirelessDeviceImportTaskResponse' smart constructor.
data GetWirelessDeviceImportTaskResponse = GetWirelessDeviceImportTaskResponse'
  { -- | The ARN (Amazon Resource Name) of the import task.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the import task was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the destination that\'s assigned to the wireless devices in
    -- the import task.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | The number of devices in the import task that failed to onboard to the
    -- import task.
    failedImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The identifier of the import task for which information is retrieved.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of devices in the import task that are waiting for the
    -- control log to start processing.
    initializedImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of devices in the import task that have been onboarded to the
    -- import task.
    onboardedImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of devices in the import task that are waiting in the import
    -- task queue to be onboarded.
    pendingImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The Sidewalk-related information about an import task.
    sidewalk :: Prelude.Maybe SidewalkGetStartImportInfo,
    -- | The import task status.
    status :: Prelude.Maybe ImportTaskStatus,
    -- | The reason for the provided status information, such as a validation
    -- error that causes the import task to fail.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessDeviceImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getWirelessDeviceImportTaskResponse_arn' - The ARN (Amazon Resource Name) of the import task.
--
-- 'creationTime', 'getWirelessDeviceImportTaskResponse_creationTime' - The time at which the import task was created.
--
-- 'destinationName', 'getWirelessDeviceImportTaskResponse_destinationName' - The name of the destination that\'s assigned to the wireless devices in
-- the import task.
--
-- 'failedImportedDeviceCount', 'getWirelessDeviceImportTaskResponse_failedImportedDeviceCount' - The number of devices in the import task that failed to onboard to the
-- import task.
--
-- 'id', 'getWirelessDeviceImportTaskResponse_id' - The identifier of the import task for which information is retrieved.
--
-- 'initializedImportedDeviceCount', 'getWirelessDeviceImportTaskResponse_initializedImportedDeviceCount' - The number of devices in the import task that are waiting for the
-- control log to start processing.
--
-- 'onboardedImportedDeviceCount', 'getWirelessDeviceImportTaskResponse_onboardedImportedDeviceCount' - The number of devices in the import task that have been onboarded to the
-- import task.
--
-- 'pendingImportedDeviceCount', 'getWirelessDeviceImportTaskResponse_pendingImportedDeviceCount' - The number of devices in the import task that are waiting in the import
-- task queue to be onboarded.
--
-- 'sidewalk', 'getWirelessDeviceImportTaskResponse_sidewalk' - The Sidewalk-related information about an import task.
--
-- 'status', 'getWirelessDeviceImportTaskResponse_status' - The import task status.
--
-- 'statusReason', 'getWirelessDeviceImportTaskResponse_statusReason' - The reason for the provided status information, such as a validation
-- error that causes the import task to fail.
--
-- 'httpStatus', 'getWirelessDeviceImportTaskResponse_httpStatus' - The response's http status code.
newGetWirelessDeviceImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessDeviceImportTaskResponse
newGetWirelessDeviceImportTaskResponse pHttpStatus_ =
  GetWirelessDeviceImportTaskResponse'
    { arn =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      failedImportedDeviceCount =
        Prelude.Nothing,
      id = Prelude.Nothing,
      initializedImportedDeviceCount =
        Prelude.Nothing,
      onboardedImportedDeviceCount =
        Prelude.Nothing,
      pendingImportedDeviceCount =
        Prelude.Nothing,
      sidewalk = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN (Amazon Resource Name) of the import task.
getWirelessDeviceImportTaskResponse_arn :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceImportTaskResponse_arn = Lens.lens (\GetWirelessDeviceImportTaskResponse' {arn} -> arn) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {arn = a} :: GetWirelessDeviceImportTaskResponse)

-- | The time at which the import task was created.
getWirelessDeviceImportTaskResponse_creationTime :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.UTCTime)
getWirelessDeviceImportTaskResponse_creationTime = Lens.lens (\GetWirelessDeviceImportTaskResponse' {creationTime} -> creationTime) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {creationTime = a} :: GetWirelessDeviceImportTaskResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the destination that\'s assigned to the wireless devices in
-- the import task.
getWirelessDeviceImportTaskResponse_destinationName :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceImportTaskResponse_destinationName = Lens.lens (\GetWirelessDeviceImportTaskResponse' {destinationName} -> destinationName) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {destinationName = a} :: GetWirelessDeviceImportTaskResponse)

-- | The number of devices in the import task that failed to onboard to the
-- import task.
getWirelessDeviceImportTaskResponse_failedImportedDeviceCount :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Integer)
getWirelessDeviceImportTaskResponse_failedImportedDeviceCount = Lens.lens (\GetWirelessDeviceImportTaskResponse' {failedImportedDeviceCount} -> failedImportedDeviceCount) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {failedImportedDeviceCount = a} :: GetWirelessDeviceImportTaskResponse)

-- | The identifier of the import task for which information is retrieved.
getWirelessDeviceImportTaskResponse_id :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceImportTaskResponse_id = Lens.lens (\GetWirelessDeviceImportTaskResponse' {id} -> id) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {id = a} :: GetWirelessDeviceImportTaskResponse)

-- | The number of devices in the import task that are waiting for the
-- control log to start processing.
getWirelessDeviceImportTaskResponse_initializedImportedDeviceCount :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Integer)
getWirelessDeviceImportTaskResponse_initializedImportedDeviceCount = Lens.lens (\GetWirelessDeviceImportTaskResponse' {initializedImportedDeviceCount} -> initializedImportedDeviceCount) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {initializedImportedDeviceCount = a} :: GetWirelessDeviceImportTaskResponse)

-- | The number of devices in the import task that have been onboarded to the
-- import task.
getWirelessDeviceImportTaskResponse_onboardedImportedDeviceCount :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Integer)
getWirelessDeviceImportTaskResponse_onboardedImportedDeviceCount = Lens.lens (\GetWirelessDeviceImportTaskResponse' {onboardedImportedDeviceCount} -> onboardedImportedDeviceCount) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {onboardedImportedDeviceCount = a} :: GetWirelessDeviceImportTaskResponse)

-- | The number of devices in the import task that are waiting in the import
-- task queue to be onboarded.
getWirelessDeviceImportTaskResponse_pendingImportedDeviceCount :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Integer)
getWirelessDeviceImportTaskResponse_pendingImportedDeviceCount = Lens.lens (\GetWirelessDeviceImportTaskResponse' {pendingImportedDeviceCount} -> pendingImportedDeviceCount) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {pendingImportedDeviceCount = a} :: GetWirelessDeviceImportTaskResponse)

-- | The Sidewalk-related information about an import task.
getWirelessDeviceImportTaskResponse_sidewalk :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe SidewalkGetStartImportInfo)
getWirelessDeviceImportTaskResponse_sidewalk = Lens.lens (\GetWirelessDeviceImportTaskResponse' {sidewalk} -> sidewalk) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {sidewalk = a} :: GetWirelessDeviceImportTaskResponse)

-- | The import task status.
getWirelessDeviceImportTaskResponse_status :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe ImportTaskStatus)
getWirelessDeviceImportTaskResponse_status = Lens.lens (\GetWirelessDeviceImportTaskResponse' {status} -> status) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {status = a} :: GetWirelessDeviceImportTaskResponse)

-- | The reason for the provided status information, such as a validation
-- error that causes the import task to fail.
getWirelessDeviceImportTaskResponse_statusReason :: Lens.Lens' GetWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessDeviceImportTaskResponse_statusReason = Lens.lens (\GetWirelessDeviceImportTaskResponse' {statusReason} -> statusReason) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {statusReason = a} :: GetWirelessDeviceImportTaskResponse)

-- | The response's http status code.
getWirelessDeviceImportTaskResponse_httpStatus :: Lens.Lens' GetWirelessDeviceImportTaskResponse Prelude.Int
getWirelessDeviceImportTaskResponse_httpStatus = Lens.lens (\GetWirelessDeviceImportTaskResponse' {httpStatus} -> httpStatus) (\s@GetWirelessDeviceImportTaskResponse' {} a -> s {httpStatus = a} :: GetWirelessDeviceImportTaskResponse)

instance
  Prelude.NFData
    GetWirelessDeviceImportTaskResponse
  where
  rnf GetWirelessDeviceImportTaskResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf failedImportedDeviceCount
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf initializedImportedDeviceCount
      `Prelude.seq` Prelude.rnf onboardedImportedDeviceCount
      `Prelude.seq` Prelude.rnf pendingImportedDeviceCount
      `Prelude.seq` Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf httpStatus
