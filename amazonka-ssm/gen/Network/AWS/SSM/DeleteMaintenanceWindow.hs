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
-- Module      : Network.AWS.SSM.DeleteMaintenanceWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a maintenance window.
module Network.AWS.SSM.DeleteMaintenanceWindow
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMaintenanceWindowResponse'
            Prelude.<$> (x Core..?> "WindowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMaintenanceWindow

instance Prelude.NFData DeleteMaintenanceWindow

instance Core.ToHeaders DeleteMaintenanceWindow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DeleteMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteMaintenanceWindow where
  toJSON DeleteMaintenanceWindow' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("WindowId" Core..= windowId)]
      )

instance Core.ToPath DeleteMaintenanceWindow where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteMaintenanceWindow where
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
