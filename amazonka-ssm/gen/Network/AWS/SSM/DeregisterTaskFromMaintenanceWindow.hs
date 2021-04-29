{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a task from a maintenance window.
module Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
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
    deregisterTaskFromMaintenanceWindowResponse_windowTaskId,
    deregisterTaskFromMaintenanceWindowResponse_windowId,
    deregisterTaskFromMaintenanceWindowResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeregisterTaskFromMaintenanceWindow' smart constructor.
data DeregisterTaskFromMaintenanceWindow = DeregisterTaskFromMaintenanceWindow'
  { -- | The ID of the maintenance window the task should be removed from.
    windowId :: Prelude.Text,
    -- | The ID of the task to remove from the maintenance window.
    windowTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeregisterTaskFromMaintenanceWindow
  where
  type
    Rs DeregisterTaskFromMaintenanceWindow =
      DeregisterTaskFromMaintenanceWindowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTaskFromMaintenanceWindowResponse'
            Prelude.<$> (x Prelude..?> "WindowTaskId")
              Prelude.<*> (x Prelude..?> "WindowId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterTaskFromMaintenanceWindow

instance
  Prelude.NFData
    DeregisterTaskFromMaintenanceWindow

instance
  Prelude.ToHeaders
    DeregisterTaskFromMaintenanceWindow
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DeregisterTaskFromMaintenanceWindow" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeregisterTaskFromMaintenanceWindow
  where
  toJSON DeregisterTaskFromMaintenanceWindow' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WindowId" Prelude..= windowId),
            Prelude.Just
              ("WindowTaskId" Prelude..= windowTaskId)
          ]
      )

instance
  Prelude.ToPath
    DeregisterTaskFromMaintenanceWindow
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeregisterTaskFromMaintenanceWindow
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterTaskFromMaintenanceWindowResponse' smart constructor.
data DeregisterTaskFromMaintenanceWindowResponse = DeregisterTaskFromMaintenanceWindowResponse'
  { -- | The ID of the task removed from the maintenance window.
    windowTaskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window the task was removed from.
    windowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTaskFromMaintenanceWindowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowTaskId', 'deregisterTaskFromMaintenanceWindowResponse_windowTaskId' - The ID of the task removed from the maintenance window.
--
-- 'windowId', 'deregisterTaskFromMaintenanceWindowResponse_windowId' - The ID of the maintenance window the task was removed from.
--
-- 'httpStatus', 'deregisterTaskFromMaintenanceWindowResponse_httpStatus' - The response's http status code.
newDeregisterTaskFromMaintenanceWindowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTaskFromMaintenanceWindowResponse
newDeregisterTaskFromMaintenanceWindowResponse
  pHttpStatus_ =
    DeregisterTaskFromMaintenanceWindowResponse'
      { windowTaskId =
          Prelude.Nothing,
        windowId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the task removed from the maintenance window.
deregisterTaskFromMaintenanceWindowResponse_windowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTaskFromMaintenanceWindowResponse_windowTaskId = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {windowTaskId} -> windowTaskId) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {windowTaskId = a} :: DeregisterTaskFromMaintenanceWindowResponse)

-- | The ID of the maintenance window the task was removed from.
deregisterTaskFromMaintenanceWindowResponse_windowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Prelude.Maybe Prelude.Text)
deregisterTaskFromMaintenanceWindowResponse_windowId = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {windowId} -> windowId) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {windowId = a} :: DeregisterTaskFromMaintenanceWindowResponse)

-- | The response's http status code.
deregisterTaskFromMaintenanceWindowResponse_httpStatus :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse Prelude.Int
deregisterTaskFromMaintenanceWindowResponse_httpStatus = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: DeregisterTaskFromMaintenanceWindowResponse)

instance
  Prelude.NFData
    DeregisterTaskFromMaintenanceWindowResponse
