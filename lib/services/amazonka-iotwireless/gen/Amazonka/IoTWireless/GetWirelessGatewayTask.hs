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
-- Module      : Amazonka.IoTWireless.GetWirelessGatewayTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a wireless gateway task.
module Amazonka.IoTWireless.GetWirelessGatewayTask
  ( -- * Creating a Request
    GetWirelessGatewayTask (..),
    newGetWirelessGatewayTask,

    -- * Request Lenses
    getWirelessGatewayTask_id,

    -- * Destructuring the Response
    GetWirelessGatewayTaskResponse (..),
    newGetWirelessGatewayTaskResponse,

    -- * Response Lenses
    getWirelessGatewayTaskResponse_lastUplinkReceivedAt,
    getWirelessGatewayTaskResponse_wirelessGatewayId,
    getWirelessGatewayTaskResponse_status,
    getWirelessGatewayTaskResponse_taskCreatedAt,
    getWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId,
    getWirelessGatewayTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWirelessGatewayTask' smart constructor.
data GetWirelessGatewayTask = GetWirelessGatewayTask'
  { -- | The ID of the resource to get.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGatewayTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getWirelessGatewayTask_id' - The ID of the resource to get.
newGetWirelessGatewayTask ::
  -- | 'id'
  Prelude.Text ->
  GetWirelessGatewayTask
newGetWirelessGatewayTask pId_ =
  GetWirelessGatewayTask' {id = pId_}

-- | The ID of the resource to get.
getWirelessGatewayTask_id :: Lens.Lens' GetWirelessGatewayTask Prelude.Text
getWirelessGatewayTask_id = Lens.lens (\GetWirelessGatewayTask' {id} -> id) (\s@GetWirelessGatewayTask' {} a -> s {id = a} :: GetWirelessGatewayTask)

instance Core.AWSRequest GetWirelessGatewayTask where
  type
    AWSResponse GetWirelessGatewayTask =
      GetWirelessGatewayTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWirelessGatewayTaskResponse'
            Prelude.<$> (x Data..?> "LastUplinkReceivedAt")
            Prelude.<*> (x Data..?> "WirelessGatewayId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "TaskCreatedAt")
            Prelude.<*> (x Data..?> "WirelessGatewayTaskDefinitionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWirelessGatewayTask where
  hashWithSalt _salt GetWirelessGatewayTask' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetWirelessGatewayTask where
  rnf GetWirelessGatewayTask' {..} = Prelude.rnf id

instance Data.ToHeaders GetWirelessGatewayTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetWirelessGatewayTask where
  toPath GetWirelessGatewayTask' {..} =
    Prelude.mconcat
      ["/wireless-gateways/", Data.toBS id, "/tasks"]

instance Data.ToQuery GetWirelessGatewayTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWirelessGatewayTaskResponse' smart constructor.
data GetWirelessGatewayTaskResponse = GetWirelessGatewayTaskResponse'
  { -- | The date and time when the most recent uplink was received.
    lastUplinkReceivedAt :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless gateway.
    wirelessGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The status of the request.
    status :: Prelude.Maybe WirelessGatewayTaskStatus,
    -- | The date and time when the task was created.
    taskCreatedAt :: Prelude.Maybe Prelude.Text,
    -- | The ID of the WirelessGatewayTask.
    wirelessGatewayTaskDefinitionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGatewayTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUplinkReceivedAt', 'getWirelessGatewayTaskResponse_lastUplinkReceivedAt' - The date and time when the most recent uplink was received.
--
-- 'wirelessGatewayId', 'getWirelessGatewayTaskResponse_wirelessGatewayId' - The ID of the wireless gateway.
--
-- 'status', 'getWirelessGatewayTaskResponse_status' - The status of the request.
--
-- 'taskCreatedAt', 'getWirelessGatewayTaskResponse_taskCreatedAt' - The date and time when the task was created.
--
-- 'wirelessGatewayTaskDefinitionId', 'getWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId' - The ID of the WirelessGatewayTask.
--
-- 'httpStatus', 'getWirelessGatewayTaskResponse_httpStatus' - The response's http status code.
newGetWirelessGatewayTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessGatewayTaskResponse
newGetWirelessGatewayTaskResponse pHttpStatus_ =
  GetWirelessGatewayTaskResponse'
    { lastUplinkReceivedAt =
        Prelude.Nothing,
      wirelessGatewayId = Prelude.Nothing,
      status = Prelude.Nothing,
      taskCreatedAt = Prelude.Nothing,
      wirelessGatewayTaskDefinitionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the most recent uplink was received.
getWirelessGatewayTaskResponse_lastUplinkReceivedAt :: Lens.Lens' GetWirelessGatewayTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayTaskResponse_lastUplinkReceivedAt = Lens.lens (\GetWirelessGatewayTaskResponse' {lastUplinkReceivedAt} -> lastUplinkReceivedAt) (\s@GetWirelessGatewayTaskResponse' {} a -> s {lastUplinkReceivedAt = a} :: GetWirelessGatewayTaskResponse)

-- | The ID of the wireless gateway.
getWirelessGatewayTaskResponse_wirelessGatewayId :: Lens.Lens' GetWirelessGatewayTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayTaskResponse_wirelessGatewayId = Lens.lens (\GetWirelessGatewayTaskResponse' {wirelessGatewayId} -> wirelessGatewayId) (\s@GetWirelessGatewayTaskResponse' {} a -> s {wirelessGatewayId = a} :: GetWirelessGatewayTaskResponse)

-- | The status of the request.
getWirelessGatewayTaskResponse_status :: Lens.Lens' GetWirelessGatewayTaskResponse (Prelude.Maybe WirelessGatewayTaskStatus)
getWirelessGatewayTaskResponse_status = Lens.lens (\GetWirelessGatewayTaskResponse' {status} -> status) (\s@GetWirelessGatewayTaskResponse' {} a -> s {status = a} :: GetWirelessGatewayTaskResponse)

-- | The date and time when the task was created.
getWirelessGatewayTaskResponse_taskCreatedAt :: Lens.Lens' GetWirelessGatewayTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayTaskResponse_taskCreatedAt = Lens.lens (\GetWirelessGatewayTaskResponse' {taskCreatedAt} -> taskCreatedAt) (\s@GetWirelessGatewayTaskResponse' {} a -> s {taskCreatedAt = a} :: GetWirelessGatewayTaskResponse)

-- | The ID of the WirelessGatewayTask.
getWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId :: Lens.Lens' GetWirelessGatewayTaskResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayTaskResponse_wirelessGatewayTaskDefinitionId = Lens.lens (\GetWirelessGatewayTaskResponse' {wirelessGatewayTaskDefinitionId} -> wirelessGatewayTaskDefinitionId) (\s@GetWirelessGatewayTaskResponse' {} a -> s {wirelessGatewayTaskDefinitionId = a} :: GetWirelessGatewayTaskResponse)

-- | The response's http status code.
getWirelessGatewayTaskResponse_httpStatus :: Lens.Lens' GetWirelessGatewayTaskResponse Prelude.Int
getWirelessGatewayTaskResponse_httpStatus = Lens.lens (\GetWirelessGatewayTaskResponse' {httpStatus} -> httpStatus) (\s@GetWirelessGatewayTaskResponse' {} a -> s {httpStatus = a} :: GetWirelessGatewayTaskResponse)

instance
  Prelude.NFData
    GetWirelessGatewayTaskResponse
  where
  rnf GetWirelessGatewayTaskResponse' {..} =
    Prelude.rnf lastUplinkReceivedAt
      `Prelude.seq` Prelude.rnf wirelessGatewayId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf taskCreatedAt
      `Prelude.seq` Prelude.rnf wirelessGatewayTaskDefinitionId
      `Prelude.seq` Prelude.rnf httpStatus
