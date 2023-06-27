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
-- Module      : Amazonka.IoTWireless.StartWirelessDeviceImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start import task for provisioning Sidewalk devices in bulk using an S3
-- CSV file.
module Amazonka.IoTWireless.StartWirelessDeviceImportTask
  ( -- * Creating a Request
    StartWirelessDeviceImportTask (..),
    newStartWirelessDeviceImportTask,

    -- * Request Lenses
    startWirelessDeviceImportTask_clientRequestToken,
    startWirelessDeviceImportTask_tags,
    startWirelessDeviceImportTask_destinationName,
    startWirelessDeviceImportTask_sidewalk,

    -- * Destructuring the Response
    StartWirelessDeviceImportTaskResponse (..),
    newStartWirelessDeviceImportTaskResponse,

    -- * Response Lenses
    startWirelessDeviceImportTaskResponse_arn,
    startWirelessDeviceImportTaskResponse_id,
    startWirelessDeviceImportTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartWirelessDeviceImportTask' smart constructor.
data StartWirelessDeviceImportTask = StartWirelessDeviceImportTask'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The name of the Sidewalk destination that describes the IoT rule to
    -- route messages from the devices in the import task that are onboarded to
    -- AWS IoT Wireless.
    destinationName :: Prelude.Text,
    -- | The Sidewalk-related parameters for importing wireless devices that need
    -- to be provisioned in bulk.
    sidewalk :: SidewalkStartImportInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWirelessDeviceImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'startWirelessDeviceImportTask_clientRequestToken' - Undocumented member.
--
-- 'tags', 'startWirelessDeviceImportTask_tags' - Undocumented member.
--
-- 'destinationName', 'startWirelessDeviceImportTask_destinationName' - The name of the Sidewalk destination that describes the IoT rule to
-- route messages from the devices in the import task that are onboarded to
-- AWS IoT Wireless.
--
-- 'sidewalk', 'startWirelessDeviceImportTask_sidewalk' - The Sidewalk-related parameters for importing wireless devices that need
-- to be provisioned in bulk.
newStartWirelessDeviceImportTask ::
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'sidewalk'
  SidewalkStartImportInfo ->
  StartWirelessDeviceImportTask
newStartWirelessDeviceImportTask
  pDestinationName_
  pSidewalk_ =
    StartWirelessDeviceImportTask'
      { clientRequestToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        destinationName = pDestinationName_,
        sidewalk = pSidewalk_
      }

-- | Undocumented member.
startWirelessDeviceImportTask_clientRequestToken :: Lens.Lens' StartWirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
startWirelessDeviceImportTask_clientRequestToken = Lens.lens (\StartWirelessDeviceImportTask' {clientRequestToken} -> clientRequestToken) (\s@StartWirelessDeviceImportTask' {} a -> s {clientRequestToken = a} :: StartWirelessDeviceImportTask)

-- | Undocumented member.
startWirelessDeviceImportTask_tags :: Lens.Lens' StartWirelessDeviceImportTask (Prelude.Maybe [Tag])
startWirelessDeviceImportTask_tags = Lens.lens (\StartWirelessDeviceImportTask' {tags} -> tags) (\s@StartWirelessDeviceImportTask' {} a -> s {tags = a} :: StartWirelessDeviceImportTask) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Sidewalk destination that describes the IoT rule to
-- route messages from the devices in the import task that are onboarded to
-- AWS IoT Wireless.
startWirelessDeviceImportTask_destinationName :: Lens.Lens' StartWirelessDeviceImportTask Prelude.Text
startWirelessDeviceImportTask_destinationName = Lens.lens (\StartWirelessDeviceImportTask' {destinationName} -> destinationName) (\s@StartWirelessDeviceImportTask' {} a -> s {destinationName = a} :: StartWirelessDeviceImportTask)

-- | The Sidewalk-related parameters for importing wireless devices that need
-- to be provisioned in bulk.
startWirelessDeviceImportTask_sidewalk :: Lens.Lens' StartWirelessDeviceImportTask SidewalkStartImportInfo
startWirelessDeviceImportTask_sidewalk = Lens.lens (\StartWirelessDeviceImportTask' {sidewalk} -> sidewalk) (\s@StartWirelessDeviceImportTask' {} a -> s {sidewalk = a} :: StartWirelessDeviceImportTask)

instance
  Core.AWSRequest
    StartWirelessDeviceImportTask
  where
  type
    AWSResponse StartWirelessDeviceImportTask =
      StartWirelessDeviceImportTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWirelessDeviceImportTaskResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartWirelessDeviceImportTask
  where
  hashWithSalt _salt StartWirelessDeviceImportTask' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` sidewalk

instance Prelude.NFData StartWirelessDeviceImportTask where
  rnf StartWirelessDeviceImportTask' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf sidewalk

instance Data.ToHeaders StartWirelessDeviceImportTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON StartWirelessDeviceImportTask where
  toJSON StartWirelessDeviceImportTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("DestinationName" Data..= destinationName),
            Prelude.Just ("Sidewalk" Data..= sidewalk)
          ]
      )

instance Data.ToPath StartWirelessDeviceImportTask where
  toPath = Prelude.const "/wireless_device_import_task"

instance Data.ToQuery StartWirelessDeviceImportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartWirelessDeviceImportTaskResponse' smart constructor.
data StartWirelessDeviceImportTaskResponse = StartWirelessDeviceImportTaskResponse'
  { -- | The ARN (Amazon Resource Name) of the import task.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The import task ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartWirelessDeviceImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startWirelessDeviceImportTaskResponse_arn' - The ARN (Amazon Resource Name) of the import task.
--
-- 'id', 'startWirelessDeviceImportTaskResponse_id' - The import task ID.
--
-- 'httpStatus', 'startWirelessDeviceImportTaskResponse_httpStatus' - The response's http status code.
newStartWirelessDeviceImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartWirelessDeviceImportTaskResponse
newStartWirelessDeviceImportTaskResponse pHttpStatus_ =
  StartWirelessDeviceImportTaskResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN (Amazon Resource Name) of the import task.
startWirelessDeviceImportTaskResponse_arn :: Lens.Lens' StartWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
startWirelessDeviceImportTaskResponse_arn = Lens.lens (\StartWirelessDeviceImportTaskResponse' {arn} -> arn) (\s@StartWirelessDeviceImportTaskResponse' {} a -> s {arn = a} :: StartWirelessDeviceImportTaskResponse)

-- | The import task ID.
startWirelessDeviceImportTaskResponse_id :: Lens.Lens' StartWirelessDeviceImportTaskResponse (Prelude.Maybe Prelude.Text)
startWirelessDeviceImportTaskResponse_id = Lens.lens (\StartWirelessDeviceImportTaskResponse' {id} -> id) (\s@StartWirelessDeviceImportTaskResponse' {} a -> s {id = a} :: StartWirelessDeviceImportTaskResponse)

-- | The response's http status code.
startWirelessDeviceImportTaskResponse_httpStatus :: Lens.Lens' StartWirelessDeviceImportTaskResponse Prelude.Int
startWirelessDeviceImportTaskResponse_httpStatus = Lens.lens (\StartWirelessDeviceImportTaskResponse' {httpStatus} -> httpStatus) (\s@StartWirelessDeviceImportTaskResponse' {} a -> s {httpStatus = a} :: StartWirelessDeviceImportTaskResponse)

instance
  Prelude.NFData
    StartWirelessDeviceImportTaskResponse
  where
  rnf StartWirelessDeviceImportTaskResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
