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
-- Module      : Amazonka.IoTWireless.DeleteWirelessDeviceImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an import task.
module Amazonka.IoTWireless.DeleteWirelessDeviceImportTask
  ( -- * Creating a Request
    DeleteWirelessDeviceImportTask (..),
    newDeleteWirelessDeviceImportTask,

    -- * Request Lenses
    deleteWirelessDeviceImportTask_id,

    -- * Destructuring the Response
    DeleteWirelessDeviceImportTaskResponse (..),
    newDeleteWirelessDeviceImportTaskResponse,

    -- * Response Lenses
    deleteWirelessDeviceImportTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWirelessDeviceImportTask' smart constructor.
data DeleteWirelessDeviceImportTask = DeleteWirelessDeviceImportTask'
  { -- | The unique identifier of the import task to be deleted.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWirelessDeviceImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteWirelessDeviceImportTask_id' - The unique identifier of the import task to be deleted.
newDeleteWirelessDeviceImportTask ::
  -- | 'id'
  Prelude.Text ->
  DeleteWirelessDeviceImportTask
newDeleteWirelessDeviceImportTask pId_ =
  DeleteWirelessDeviceImportTask' {id = pId_}

-- | The unique identifier of the import task to be deleted.
deleteWirelessDeviceImportTask_id :: Lens.Lens' DeleteWirelessDeviceImportTask Prelude.Text
deleteWirelessDeviceImportTask_id = Lens.lens (\DeleteWirelessDeviceImportTask' {id} -> id) (\s@DeleteWirelessDeviceImportTask' {} a -> s {id = a} :: DeleteWirelessDeviceImportTask)

instance
  Core.AWSRequest
    DeleteWirelessDeviceImportTask
  where
  type
    AWSResponse DeleteWirelessDeviceImportTask =
      DeleteWirelessDeviceImportTaskResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWirelessDeviceImportTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteWirelessDeviceImportTask
  where
  hashWithSalt
    _salt
    DeleteWirelessDeviceImportTask' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteWirelessDeviceImportTask
  where
  rnf DeleteWirelessDeviceImportTask' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    DeleteWirelessDeviceImportTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteWirelessDeviceImportTask where
  toPath DeleteWirelessDeviceImportTask' {..} =
    Prelude.mconcat
      ["/wireless_device_import_task/", Data.toBS id]

instance Data.ToQuery DeleteWirelessDeviceImportTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWirelessDeviceImportTaskResponse' smart constructor.
data DeleteWirelessDeviceImportTaskResponse = DeleteWirelessDeviceImportTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWirelessDeviceImportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWirelessDeviceImportTaskResponse_httpStatus' - The response's http status code.
newDeleteWirelessDeviceImportTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWirelessDeviceImportTaskResponse
newDeleteWirelessDeviceImportTaskResponse
  pHttpStatus_ =
    DeleteWirelessDeviceImportTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteWirelessDeviceImportTaskResponse_httpStatus :: Lens.Lens' DeleteWirelessDeviceImportTaskResponse Prelude.Int
deleteWirelessDeviceImportTaskResponse_httpStatus = Lens.lens (\DeleteWirelessDeviceImportTaskResponse' {httpStatus} -> httpStatus) (\s@DeleteWirelessDeviceImportTaskResponse' {} a -> s {httpStatus = a} :: DeleteWirelessDeviceImportTaskResponse)

instance
  Prelude.NFData
    DeleteWirelessDeviceImportTaskResponse
  where
  rnf DeleteWirelessDeviceImportTaskResponse' {..} =
    Prelude.rnf httpStatus
