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
-- Module      : Amazonka.SSM.DeleteMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a maintenance window.
module Amazonka.SSM.DeleteMaintenanceWindow
  ( -- * Creating a Request
    DeleteMaintenanceWindow (..),
    newDeleteMaintenanceWindow,

    -- * Request Lenses
    deleteMaintenanceWindow_windowId,

    -- * Destructuring the Response
    DeleteMaintenanceWindowResponse (..),
    newDeleteMaintenanceWindowResponse,

    -- * Response Lenses
    deleteMaintenanceWindowResponse_windowId,
    deleteMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteMaintenanceWindow' smart constructor.
data DeleteMaintenanceWindow = DeleteMaintenanceWindow'
  { -- | The ID of the maintenance window to delete.
    windowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'deleteMaintenanceWindow_windowId' - The ID of the maintenance window to delete.
newDeleteMaintenanceWindow ::
  -- | 'windowId'
  Prelude.Text ->
  DeleteMaintenanceWindow
newDeleteMaintenanceWindow pWindowId_ =
  DeleteMaintenanceWindow' {windowId = pWindowId_}

-- | The ID of the maintenance window to delete.
deleteMaintenanceWindow_windowId :: Lens.Lens' DeleteMaintenanceWindow Prelude.Text
deleteMaintenanceWindow_windowId = Lens.lens (\DeleteMaintenanceWindow' {windowId} -> windowId) (\s@DeleteMaintenanceWindow' {} a -> s {windowId = a} :: DeleteMaintenanceWindow)

instance Core.AWSRequest DeleteMaintenanceWindow where
  type
    AWSResponse DeleteMaintenanceWindow =
      DeleteMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMaintenanceWindowResponse'
            Prelude.<$> (x Data..?> "WindowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMaintenanceWindow where
  hashWithSalt _salt DeleteMaintenanceWindow' {..} =
    _salt `Prelude.hashWithSalt` windowId

instance Prelude.NFData DeleteMaintenanceWindow where
  rnf DeleteMaintenanceWindow' {..} =
    Prelude.rnf windowId

instance Data.ToHeaders DeleteMaintenanceWindow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DeleteMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMaintenanceWindow where
  toJSON DeleteMaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WindowId" Data..= windowId)]
      )

instance Data.ToPath DeleteMaintenanceWindow where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMaintenanceWindow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMaintenanceWindowResponse' smart constructor.
data DeleteMaintenanceWindowResponse = DeleteMaintenanceWindowResponse'
  { -- | The ID of the deleted maintenance window.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'deleteMaintenanceWindowResponse_windowId' - The ID of the deleted maintenance window.
--
-- 'httpStatus', 'deleteMaintenanceWindowResponse_httpStatus' - The response's http status code.
newDeleteMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMaintenanceWindowResponse
newDeleteMaintenanceWindowResponse pHttpStatus_ =
  DeleteMaintenanceWindowResponse'
    { windowId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deleted maintenance window.
deleteMaintenanceWindowResponse_windowId :: Lens.Lens' DeleteMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deleteMaintenanceWindowResponse_windowId = Lens.lens (\DeleteMaintenanceWindowResponse' {windowId} -> windowId) (\s@DeleteMaintenanceWindowResponse' {} a -> s {windowId = a} :: DeleteMaintenanceWindowResponse)

-- | The response's http status code.
deleteMaintenanceWindowResponse_httpStatus :: Lens.Lens' DeleteMaintenanceWindowResponse Prelude.Int
deleteMaintenanceWindowResponse_httpStatus = Lens.lens (\DeleteMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@DeleteMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: DeleteMaintenanceWindowResponse)

instance
  Prelude.NFData
    DeleteMaintenanceWindowResponse
  where
  rnf DeleteMaintenanceWindowResponse' {..} =
    Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf httpStatus
