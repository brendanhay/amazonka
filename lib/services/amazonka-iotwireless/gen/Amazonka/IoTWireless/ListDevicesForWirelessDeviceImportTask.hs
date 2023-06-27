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
-- Module      : Amazonka.IoTWireless.ListDevicesForWirelessDeviceImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the Sidewalk devices in an import task and their onboarding status.
module Amazonka.IoTWireless.ListDevicesForWirelessDeviceImportTask
  ( -- * Creating a Request
    ListDevicesForWirelessDeviceImportTask (..),
    newListDevicesForWirelessDeviceImportTask,

    -- * Request Lenses
    listDevicesForWirelessDeviceImportTask_maxResults,
    listDevicesForWirelessDeviceImportTask_nextToken,
    listDevicesForWirelessDeviceImportTask_status,
    listDevicesForWirelessDeviceImportTask_id,

    -- * Destructuring the Response
    ListDevicesForWirelessDeviceImportTaskResponse (..),
    newListDevicesForWirelessDeviceImportTaskResponse,

    -- * Response Lenses
    listDevicesForWirelessDeviceImportTaskResponse_destinationName,
    listDevicesForWirelessDeviceImportTaskResponse_importedWirelessDeviceList,
    listDevicesForWirelessDeviceImportTaskResponse_nextToken,
    listDevicesForWirelessDeviceImportTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDevicesForWirelessDeviceImportTask' smart constructor.
data ListDevicesForWirelessDeviceImportTask = ListDevicesForWirelessDeviceImportTask'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise @null@ to receive the first set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the devices in the import task.
    status :: Prelude.Maybe OnboardStatus,
    -- | The identifier of the import task for which wireless devices are listed.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicesForWirelessDeviceImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDevicesForWirelessDeviceImportTask_maxResults' - Undocumented member.
--
-- 'nextToken', 'listDevicesForWirelessDeviceImportTask_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise @null@ to receive the first set of results.
--
-- 'status', 'listDevicesForWirelessDeviceImportTask_status' - The status of the devices in the import task.
--
-- 'id', 'listDevicesForWirelessDeviceImportTask_id' - The identifier of the import task for which wireless devices are listed.
newListDevicesForWirelessDeviceImportTask ::
  -- | 'id'
  Prelude.Text ->
  ListDevicesForWirelessDeviceImportTask
newListDevicesForWirelessDeviceImportTask pId_ =
  ListDevicesForWirelessDeviceImportTask'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
listDevicesForWirelessDeviceImportTask_maxResults :: Lens.Lens' ListDevicesForWirelessDeviceImportTask (Prelude.Maybe Prelude.Natural)
listDevicesForWirelessDeviceImportTask_maxResults = Lens.lens (\ListDevicesForWirelessDeviceImportTask' {maxResults} -> maxResults) (\s@ListDevicesForWirelessDeviceImportTask' {} a -> s {maxResults = a} :: ListDevicesForWirelessDeviceImportTask)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise @null@ to receive the first set of results.
listDevicesForWirelessDeviceImportTask_nextToken :: Lens.Lens' ListDevicesForWirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
listDevicesForWirelessDeviceImportTask_nextToken = Lens.lens (\ListDevicesForWirelessDeviceImportTask' {nextToken} -> nextToken) (\s@ListDevicesForWirelessDeviceImportTask' {} a -> s {nextToken = a} :: ListDevicesForWirelessDeviceImportTask)

-- | The status of the devices in the import task.
listDevicesForWirelessDeviceImportTask_status :: Lens.Lens' ListDevicesForWirelessDeviceImportTask (Prelude.Maybe OnboardStatus)
listDevicesForWirelessDeviceImportTask_status = Lens.lens (\ListDevicesForWirelessDeviceImportTask' {status} -> status) (\s@ListDevicesForWirelessDeviceImportTask' {} a -> s {status = a} :: ListDevicesForWirelessDeviceImportTask)

-- | The identifier of the import task for which wireless devices are listed.
listDevicesForWirelessDeviceImportTask_id :: Lens.Lens' ListDevicesForWirelessDeviceImportTask Prelude.Text
listDevicesForWirelessDeviceImportTask_id = Lens.lens (\ListDevicesForWirelessDeviceImportTask' {id} -> id) (\s@ListDevicesForWirelessDeviceImportTask' {} a -> s {id = a} :: ListDevicesForWirelessDeviceImportTask)

instance
  Core.AWSRequest
    ListDevicesForWirelessDeviceImportTask
  where
  type
    AWSResponse
      ListDevicesForWirelessDeviceImportTask =
      ListDevicesForWirelessDeviceImportTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesForWirelessDeviceImportTaskResponse'
            Prelude.<$> (x Data..?> "DestinationName")
            Prelude.<*> ( x
                            Data..?> "ImportedWirelessDeviceList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDevicesForWirelessDeviceImportTask
  where
  hashWithSalt
    _salt
    ListDevicesForWirelessDeviceImportTask' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    ListDevicesForWirelessDeviceImportTask
  where
  rnf ListDevicesForWirelessDeviceImportTask' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    ListDevicesForWirelessDeviceImportTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListDevicesForWirelessDeviceImportTask
  where
  toPath = Prelude.const "/wireless_device_import_task"

instance
  Data.ToQuery
    ListDevicesForWirelessDeviceImportTask
  where
  toQuery ListDevicesForWirelessDeviceImportTask' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status,
        "id" Data.=: id
      ]

-- | /See:/ 'newListDevicesForWirelessDeviceImportTaskResponse' smart constructor.
data ListDevicesForWirelessDeviceImportTaskResponse = ListDevicesForWirelessDeviceImportTaskResponse'
  { -- | The name of the Sidewalk destination that describes the IoT rule to
    -- route messages received from devices in an import task that are
    -- onboarded to AWS IoT Wireless.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | List of wireless devices in an import task and their onboarding status.
    importedWirelessDeviceList :: Prelude.Maybe [ImportedWirelessDevice],
    -- | The token to use to get the next set of results, or @null@ if there are
    -- no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicesForWirelessDeviceImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationName', 'listDevicesForWirelessDeviceImportTaskResponse_destinationName' - The name of the Sidewalk destination that describes the IoT rule to
-- route messages received from devices in an import task that are
-- onboarded to AWS IoT Wireless.
--
-- 'importedWirelessDeviceList', 'listDevicesForWirelessDeviceImportTaskResponse_importedWirelessDeviceList' - List of wireless devices in an import task and their onboarding status.
--
-- 'nextToken', 'listDevicesForWirelessDeviceImportTaskResponse_nextToken' - The token to use to get the next set of results, or @null@ if there are
-- no additional results.
--
-- 'httpStatus', 'listDevicesForWirelessDeviceImportTaskResponse_httpStatus' - The response's http status code.
newListDevicesForWirelessDeviceImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevicesForWirelessDeviceImportTaskResponse
newListDevicesForWirelessDeviceImportTaskResponse
  pHttpStatus_ =
    ListDevicesForWirelessDeviceImportTaskResponse'
      { destinationName =
          Prelude.Nothing,
        importedWirelessDeviceList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the Sidewalk destination that describes the IoT rule to
-- route messages received from devices in an import task that are
-- onboarded to AWS IoT Wireless.
listDevicesForWirelessDeviceImportTaskResponse_destinationName :: Lens.Lens' ListDevicesForWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
listDevicesForWirelessDeviceImportTaskResponse_destinationName = Lens.lens (\ListDevicesForWirelessDeviceImportTaskResponse' {destinationName} -> destinationName) (\s@ListDevicesForWirelessDeviceImportTaskResponse' {} a -> s {destinationName = a} :: ListDevicesForWirelessDeviceImportTaskResponse)

-- | List of wireless devices in an import task and their onboarding status.
listDevicesForWirelessDeviceImportTaskResponse_importedWirelessDeviceList :: Lens.Lens' ListDevicesForWirelessDeviceImportTaskResponse (Prelude.Maybe [ImportedWirelessDevice])
listDevicesForWirelessDeviceImportTaskResponse_importedWirelessDeviceList = Lens.lens (\ListDevicesForWirelessDeviceImportTaskResponse' {importedWirelessDeviceList} -> importedWirelessDeviceList) (\s@ListDevicesForWirelessDeviceImportTaskResponse' {} a -> s {importedWirelessDeviceList = a} :: ListDevicesForWirelessDeviceImportTaskResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next set of results, or @null@ if there are
-- no additional results.
listDevicesForWirelessDeviceImportTaskResponse_nextToken :: Lens.Lens' ListDevicesForWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
listDevicesForWirelessDeviceImportTaskResponse_nextToken = Lens.lens (\ListDevicesForWirelessDeviceImportTaskResponse' {nextToken} -> nextToken) (\s@ListDevicesForWirelessDeviceImportTaskResponse' {} a -> s {nextToken = a} :: ListDevicesForWirelessDeviceImportTaskResponse)

-- | The response's http status code.
listDevicesForWirelessDeviceImportTaskResponse_httpStatus :: Lens.Lens' ListDevicesForWirelessDeviceImportTaskResponse Prelude.Int
listDevicesForWirelessDeviceImportTaskResponse_httpStatus = Lens.lens (\ListDevicesForWirelessDeviceImportTaskResponse' {httpStatus} -> httpStatus) (\s@ListDevicesForWirelessDeviceImportTaskResponse' {} a -> s {httpStatus = a} :: ListDevicesForWirelessDeviceImportTaskResponse)

instance
  Prelude.NFData
    ListDevicesForWirelessDeviceImportTaskResponse
  where
  rnf
    ListDevicesForWirelessDeviceImportTaskResponse' {..} =
      Prelude.rnf destinationName
        `Prelude.seq` Prelude.rnf importedWirelessDeviceList
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
