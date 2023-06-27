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
-- Module      : Amazonka.IoTWireless.StartSingleWirelessDeviceImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start import task for a single wireless device.
module Amazonka.IoTWireless.StartSingleWirelessDeviceImportTask
  ( -- * Creating a Request
    StartSingleWirelessDeviceImportTask (..),
    newStartSingleWirelessDeviceImportTask,

    -- * Request Lenses
    startSingleWirelessDeviceImportTask_clientRequestToken,
    startSingleWirelessDeviceImportTask_deviceName,
    startSingleWirelessDeviceImportTask_tags,
    startSingleWirelessDeviceImportTask_destinationName,
    startSingleWirelessDeviceImportTask_sidewalk,

    -- * Destructuring the Response
    StartSingleWirelessDeviceImportTaskResponse (..),
    newStartSingleWirelessDeviceImportTaskResponse,

    -- * Response Lenses
    startSingleWirelessDeviceImportTaskResponse_arn,
    startSingleWirelessDeviceImportTaskResponse_id,
    startSingleWirelessDeviceImportTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSingleWirelessDeviceImportTask' smart constructor.
data StartSingleWirelessDeviceImportTask = StartSingleWirelessDeviceImportTask'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the wireless device for which an import task is being
    -- started.
    deviceName :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The name of the Sidewalk destination that describes the IoT rule to
    -- route messages from the device in the import task that will be onboarded
    -- to AWS IoT Wireless.
    destinationName :: Prelude.Text,
    -- | The Sidewalk-related parameters for importing a single wireless device.
    sidewalk :: SidewalkSingleStartImportInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSingleWirelessDeviceImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startSingleWirelessDeviceImportTask_clientRequestToken' - Undocumented member.
--
-- 'deviceName', 'startSingleWirelessDeviceImportTask_deviceName' - The name of the wireless device for which an import task is being
-- started.
--
-- 'tags', 'startSingleWirelessDeviceImportTask_tags' - Undocumented member.
--
-- 'destinationName', 'startSingleWirelessDeviceImportTask_destinationName' - The name of the Sidewalk destination that describes the IoT rule to
-- route messages from the device in the import task that will be onboarded
-- to AWS IoT Wireless.
--
-- 'sidewalk', 'startSingleWirelessDeviceImportTask_sidewalk' - The Sidewalk-related parameters for importing a single wireless device.
newStartSingleWirelessDeviceImportTask ::
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'sidewalk'
  SidewalkSingleStartImportInfo ->
  StartSingleWirelessDeviceImportTask
newStartSingleWirelessDeviceImportTask
  pDestinationName_
  pSidewalk_ =
    StartSingleWirelessDeviceImportTask'
      { clientRequestToken =
          Prelude.Nothing,
        deviceName = Prelude.Nothing,
        tags = Prelude.Nothing,
        destinationName = pDestinationName_,
        sidewalk = pSidewalk_
      }

-- | Undocumented member.
startSingleWirelessDeviceImportTask_clientRequestToken :: Lens.Lens' StartSingleWirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
startSingleWirelessDeviceImportTask_clientRequestToken = Lens.lens (\StartSingleWirelessDeviceImportTask' {clientRequestToken} -> clientRequestToken) (\s@StartSingleWirelessDeviceImportTask' {} a -> s {clientRequestToken = a} :: StartSingleWirelessDeviceImportTask)

-- | The name of the wireless device for which an import task is being
-- started.
startSingleWirelessDeviceImportTask_deviceName :: Lens.Lens' StartSingleWirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
startSingleWirelessDeviceImportTask_deviceName = Lens.lens (\StartSingleWirelessDeviceImportTask' {deviceName} -> deviceName) (\s@StartSingleWirelessDeviceImportTask' {} a -> s {deviceName = a} :: StartSingleWirelessDeviceImportTask)

-- | Undocumented member.
startSingleWirelessDeviceImportTask_tags :: Lens.Lens' StartSingleWirelessDeviceImportTask (Prelude.Maybe [Tag])
startSingleWirelessDeviceImportTask_tags = Lens.lens (\StartSingleWirelessDeviceImportTask' {tags} -> tags) (\s@StartSingleWirelessDeviceImportTask' {} a -> s {tags = a} :: StartSingleWirelessDeviceImportTask) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Sidewalk destination that describes the IoT rule to
-- route messages from the device in the import task that will be onboarded
-- to AWS IoT Wireless.
startSingleWirelessDeviceImportTask_destinationName :: Lens.Lens' StartSingleWirelessDeviceImportTask Prelude.Text
startSingleWirelessDeviceImportTask_destinationName = Lens.lens (\StartSingleWirelessDeviceImportTask' {destinationName} -> destinationName) (\s@StartSingleWirelessDeviceImportTask' {} a -> s {destinationName = a} :: StartSingleWirelessDeviceImportTask)

-- | The Sidewalk-related parameters for importing a single wireless device.
startSingleWirelessDeviceImportTask_sidewalk :: Lens.Lens' StartSingleWirelessDeviceImportTask SidewalkSingleStartImportInfo
startSingleWirelessDeviceImportTask_sidewalk = Lens.lens (\StartSingleWirelessDeviceImportTask' {sidewalk} -> sidewalk) (\s@StartSingleWirelessDeviceImportTask' {} a -> s {sidewalk = a} :: StartSingleWirelessDeviceImportTask)

instance
  Core.AWSRequest
    StartSingleWirelessDeviceImportTask
  where
  type
    AWSResponse StartSingleWirelessDeviceImportTask =
      StartSingleWirelessDeviceImportTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSingleWirelessDeviceImportTaskResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartSingleWirelessDeviceImportTask
  where
  hashWithSalt
    _salt
    StartSingleWirelessDeviceImportTask' {..} =
      _salt
        `Prelude.hashWithSalt` clientRequestToken
        `Prelude.hashWithSalt` deviceName
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` destinationName
        `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    StartSingleWirelessDeviceImportTask
  where
  rnf StartSingleWirelessDeviceImportTask' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf sidewalk

instance
  Data.ToHeaders
    StartSingleWirelessDeviceImportTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    StartSingleWirelessDeviceImportTask
  where
  toJSON StartSingleWirelessDeviceImportTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("DeviceName" Data..=) Prelude.<$> deviceName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("DestinationName" Data..= destinationName),
            Prelude.Just ("Sidewalk" Data..= sidewalk)
          ]
      )

instance
  Data.ToPath
    StartSingleWirelessDeviceImportTask
  where
  toPath =
    Prelude.const "/wireless_single_device_import_task"

instance
  Data.ToQuery
    StartSingleWirelessDeviceImportTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSingleWirelessDeviceImportTaskResponse' smart constructor.
data StartSingleWirelessDeviceImportTaskResponse = StartSingleWirelessDeviceImportTaskResponse'
  { -- | The ARN (Amazon Resource Name) of the import task.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The import task ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSingleWirelessDeviceImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startSingleWirelessDeviceImportTaskResponse_arn' - The ARN (Amazon Resource Name) of the import task.
--
-- 'id', 'startSingleWirelessDeviceImportTaskResponse_id' - The import task ID.
--
-- 'httpStatus', 'startSingleWirelessDeviceImportTaskResponse_httpStatus' - The response's http status code.
newStartSingleWirelessDeviceImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSingleWirelessDeviceImportTaskResponse
newStartSingleWirelessDeviceImportTaskResponse
  pHttpStatus_ =
    StartSingleWirelessDeviceImportTaskResponse'
      { arn =
          Prelude.Nothing,
        id = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN (Amazon Resource Name) of the import task.
startSingleWirelessDeviceImportTaskResponse_arn :: Lens.Lens' StartSingleWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
startSingleWirelessDeviceImportTaskResponse_arn = Lens.lens (\StartSingleWirelessDeviceImportTaskResponse' {arn} -> arn) (\s@StartSingleWirelessDeviceImportTaskResponse' {} a -> s {arn = a} :: StartSingleWirelessDeviceImportTaskResponse)

-- | The import task ID.
startSingleWirelessDeviceImportTaskResponse_id :: Lens.Lens' StartSingleWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
startSingleWirelessDeviceImportTaskResponse_id = Lens.lens (\StartSingleWirelessDeviceImportTaskResponse' {id} -> id) (\s@StartSingleWirelessDeviceImportTaskResponse' {} a -> s {id = a} :: StartSingleWirelessDeviceImportTaskResponse)

-- | The response's http status code.
startSingleWirelessDeviceImportTaskResponse_httpStatus :: Lens.Lens' StartSingleWirelessDeviceImportTaskResponse Prelude.Int
startSingleWirelessDeviceImportTaskResponse_httpStatus = Lens.lens (\StartSingleWirelessDeviceImportTaskResponse' {httpStatus} -> httpStatus) (\s@StartSingleWirelessDeviceImportTaskResponse' {} a -> s {httpStatus = a} :: StartSingleWirelessDeviceImportTaskResponse)

instance
  Prelude.NFData
    StartSingleWirelessDeviceImportTaskResponse
  where
  rnf StartSingleWirelessDeviceImportTaskResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
