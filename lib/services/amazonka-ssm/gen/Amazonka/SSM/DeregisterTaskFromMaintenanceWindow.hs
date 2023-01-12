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
-- Module      : Amazonka.SSM.DeregisterTaskFromMaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a task from a maintenance window.
module Amazonka.SSM.DeregisterTaskFromMaintenanceWindow
  ( -- * Creating a Request
    DeregisterTaskFromMaintenanceWindow (..),
    newDeregisterTaskFromMaintenanceWindow,

    -- * Request Lenses
    deregisterTaskFromMaintenanceWindow_windowId,
    deregisterTaskFromMaintenanceWindow_windowTaskId,

    -- * Destructuring the Response
    DeregisterTaskFromMaintenanceWindowResponse (..),
    newDeregisterTaskFromMaintenanceWindowResponse,

    -- * Response Lenses
    deregisterTaskFromMaintenanceWindowResponse_windowId,
    deregisterTaskFromMaintenanceWindowResponse_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeregisterTaskFromMaintenanceWindow' smart constructor.
data DeregisterTaskFromMaintenanceWindow = DeregisterTaskFromMaintenanceWindow'
  { -- | The ID of the maintenance window the task should be removed from.
    windowId :: Prelude.Text,
    -- | The ID of the task to remove from the maintenance window.
    windowTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTaskFromMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'deregisterTaskFromMaintenanceWindow_windowId' - The ID of the maintenance window the task should be removed from.
--
-- 'windowTaskId', 'deregisterTaskFromMaintenanceWindow_windowTaskId' - The ID of the task to remove from the maintenance window.
newDeregisterTaskFromMaintenanceWindow ::
  -- | 'windowId'
  Prelude.Text ->
  -- | 'windowTaskId'
  Prelude.Text ->
  DeregisterTaskFromMaintenanceWindow
newDeregisterTaskFromMaintenanceWindow
  pWindowId_
  pWindowTaskId_ =
    DeregisterTaskFromMaintenanceWindow'
      { windowId =
          pWindowId_,
        windowTaskId = pWindowTaskId_
      }

-- | The ID of the maintenance window the task should be removed from.
deregisterTaskFromMaintenanceWindow_windowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Prelude.Text
deregisterTaskFromMaintenanceWindow_windowId = Lens.lens (\DeregisterTaskFromMaintenanceWindow' {windowId} -> windowId) (\s@DeregisterTaskFromMaintenanceWindow' {} a -> s {windowId = a} :: DeregisterTaskFromMaintenanceWindow)

-- | The ID of the task to remove from the maintenance window.
deregisterTaskFromMaintenanceWindow_windowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Prelude.Text
deregisterTaskFromMaintenanceWindow_windowTaskId = Lens.lens (\DeregisterTaskFromMaintenanceWindow' {windowTaskId} -> windowTaskId) (\s@DeregisterTaskFromMaintenanceWindow' {} a -> s {windowTaskId = a} :: DeregisterTaskFromMaintenanceWindow)

instance
  Core.AWSRequest
    DeregisterTaskFromMaintenanceWindow
  where
  type
    AWSResponse DeregisterTaskFromMaintenanceWindow =
      DeregisterTaskFromMaintenanceWindowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTaskFromMaintenanceWindowResponse'
            Prelude.<$> (x Data..?> "WindowId")
              Prelude.<*> (x Data..?> "WindowTaskId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterTaskFromMaintenanceWindow
  where
  hashWithSalt
    _salt
    DeregisterTaskFromMaintenanceWindow' {..} =
      _salt `Prelude.hashWithSalt` windowId
        `Prelude.hashWithSalt` windowTaskId

instance
  Prelude.NFData
    DeregisterTaskFromMaintenanceWindow
  where
  rnf DeregisterTaskFromMaintenanceWindow' {..} =
    Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf windowTaskId

instance
  Data.ToHeaders
    DeregisterTaskFromMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DeregisterTaskFromMaintenanceWindow" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeregisterTaskFromMaintenanceWindow
  where
  toJSON DeregisterTaskFromMaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WindowId" Data..= windowId),
            Prelude.Just ("WindowTaskId" Data..= windowTaskId)
          ]
      )

instance
  Data.ToPath
    DeregisterTaskFromMaintenanceWindow
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeregisterTaskFromMaintenanceWindow
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterTaskFromMaintenanceWindowResponse' smart constructor.
data DeregisterTaskFromMaintenanceWindowResponse = DeregisterTaskFromMaintenanceWindowResponse'
  { -- | The ID of the maintenance window the task was removed from.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task removed from the maintenance window.
    windowTaskId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTaskFromMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowId', 'deregisterTaskFromMaintenanceWindowResponse_windowId' - The ID of the maintenance window the task was removed from.
--
-- 'windowTaskId', 'deregisterTaskFromMaintenanceWindowResponse_windowTaskId' - The ID of the task removed from the maintenance window.
--
-- 'httpStatus', 'deregisterTaskFromMaintenanceWindowResponse_httpStatus' - The response's http status code.
newDeregisterTaskFromMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTaskFromMaintenanceWindowResponse
newDeregisterTaskFromMaintenanceWindowResponse
  pHttpStatus_ =
    DeregisterTaskFromMaintenanceWindowResponse'
      { windowId =
          Prelude.Nothing,
        windowTaskId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the maintenance window the task was removed from.
deregisterTaskFromMaintenanceWindowResponse_windowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTaskFromMaintenanceWindowResponse_windowId = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {windowId} -> windowId) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {windowId = a} :: DeregisterTaskFromMaintenanceWindowResponse)

-- | The ID of the task removed from the maintenance window.
deregisterTaskFromMaintenanceWindowResponse_windowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTaskFromMaintenanceWindowResponse_windowTaskId = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {windowTaskId} -> windowTaskId) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {windowTaskId = a} :: DeregisterTaskFromMaintenanceWindowResponse)

-- | The response's http status code.
deregisterTaskFromMaintenanceWindowResponse_httpStatus :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse Prelude.Int
deregisterTaskFromMaintenanceWindowResponse_httpStatus = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: DeregisterTaskFromMaintenanceWindowResponse)

instance
  Prelude.NFData
    DeregisterTaskFromMaintenanceWindowResponse
  where
  rnf DeregisterTaskFromMaintenanceWindowResponse' {..} =
    Prelude.rnf windowId
      `Prelude.seq` Prelude.rnf windowTaskId
      `Prelude.seq` Prelude.rnf httpStatus
