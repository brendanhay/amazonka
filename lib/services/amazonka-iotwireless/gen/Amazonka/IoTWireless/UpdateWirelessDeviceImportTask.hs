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
-- Module      : Amazonka.IoTWireless.UpdateWirelessDeviceImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an import task to add more devices to the task.
module Amazonka.IoTWireless.UpdateWirelessDeviceImportTask
  ( -- * Creating a Request
    UpdateWirelessDeviceImportTask (..),
    newUpdateWirelessDeviceImportTask,

    -- * Request Lenses
    updateWirelessDeviceImportTask_id,
    updateWirelessDeviceImportTask_sidewalk,

    -- * Destructuring the Response
    UpdateWirelessDeviceImportTaskResponse (..),
    newUpdateWirelessDeviceImportTaskResponse,

    -- * Response Lenses
    updateWirelessDeviceImportTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWirelessDeviceImportTask' smart constructor.
data UpdateWirelessDeviceImportTask = UpdateWirelessDeviceImportTask'
  { -- | The identifier of the import task to be updated.
    id :: Prelude.Text,
    -- | The Sidewalk-related parameters of the import task to be updated.
    sidewalk :: SidewalkUpdateImportInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessDeviceImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateWirelessDeviceImportTask_id' - The identifier of the import task to be updated.
--
-- 'sidewalk', 'updateWirelessDeviceImportTask_sidewalk' - The Sidewalk-related parameters of the import task to be updated.
newUpdateWirelessDeviceImportTask ::
  -- | 'id'
  Prelude.Text ->
  -- | 'sidewalk'
  SidewalkUpdateImportInfo ->
  UpdateWirelessDeviceImportTask
newUpdateWirelessDeviceImportTask pId_ pSidewalk_ =
  UpdateWirelessDeviceImportTask'
    { id = pId_,
      sidewalk = pSidewalk_
    }

-- | The identifier of the import task to be updated.
updateWirelessDeviceImportTask_id :: Lens.Lens' UpdateWirelessDeviceImportTask Prelude.Text
updateWirelessDeviceImportTask_id = Lens.lens (\UpdateWirelessDeviceImportTask' {id} -> id) (\s@UpdateWirelessDeviceImportTask' {} a -> s {id = a} :: UpdateWirelessDeviceImportTask)

-- | The Sidewalk-related parameters of the import task to be updated.
updateWirelessDeviceImportTask_sidewalk :: Lens.Lens' UpdateWirelessDeviceImportTask SidewalkUpdateImportInfo
updateWirelessDeviceImportTask_sidewalk = Lens.lens (\UpdateWirelessDeviceImportTask' {sidewalk} -> sidewalk) (\s@UpdateWirelessDeviceImportTask' {} a -> s {sidewalk = a} :: UpdateWirelessDeviceImportTask)

instance
  Core.AWSRequest
    UpdateWirelessDeviceImportTask
  where
  type
    AWSResponse UpdateWirelessDeviceImportTask =
      UpdateWirelessDeviceImportTaskResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateWirelessDeviceImportTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateWirelessDeviceImportTask
  where
  hashWithSalt
    _salt
    UpdateWirelessDeviceImportTask' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    UpdateWirelessDeviceImportTask
  where
  rnf UpdateWirelessDeviceImportTask' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf sidewalk

instance
  Data.ToHeaders
    UpdateWirelessDeviceImportTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateWirelessDeviceImportTask where
  toJSON UpdateWirelessDeviceImportTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Sidewalk" Data..= sidewalk)]
      )

instance Data.ToPath UpdateWirelessDeviceImportTask where
  toPath UpdateWirelessDeviceImportTask' {..} =
    Prelude.mconcat
      ["/wireless_device_import_task/", Data.toBS id]

instance Data.ToQuery UpdateWirelessDeviceImportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWirelessDeviceImportTaskResponse' smart constructor.
data UpdateWirelessDeviceImportTaskResponse = UpdateWirelessDeviceImportTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessDeviceImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWirelessDeviceImportTaskResponse_httpStatus' - The response's http status code.
newUpdateWirelessDeviceImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWirelessDeviceImportTaskResponse
newUpdateWirelessDeviceImportTaskResponse
  pHttpStatus_ =
    UpdateWirelessDeviceImportTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateWirelessDeviceImportTaskResponse_httpStatus :: Lens.Lens' UpdateWirelessDeviceImportTaskResponse Prelude.Int
updateWirelessDeviceImportTaskResponse_httpStatus = Lens.lens (\UpdateWirelessDeviceImportTaskResponse' {httpStatus} -> httpStatus) (\s@UpdateWirelessDeviceImportTaskResponse' {} a -> s {httpStatus = a} :: UpdateWirelessDeviceImportTaskResponse)

instance
  Prelude.NFData
    UpdateWirelessDeviceImportTaskResponse
  where
  rnf UpdateWirelessDeviceImportTaskResponse' {..} =
    Prelude.rnf httpStatus
