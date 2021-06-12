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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeregisterTaskFromMaintenanceWindow' smart constructor.
data DeregisterTaskFromMaintenanceWindow = DeregisterTaskFromMaintenanceWindow'
  { -- | The ID of the maintenance window the task should be removed from.
    windowId :: Core.Text,
    -- | The ID of the task to remove from the maintenance window.
    windowTaskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'windowTaskId'
  Core.Text ->
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
deregisterTaskFromMaintenanceWindow_windowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Core.Text
deregisterTaskFromMaintenanceWindow_windowId = Lens.lens (\DeregisterTaskFromMaintenanceWindow' {windowId} -> windowId) (\s@DeregisterTaskFromMaintenanceWindow' {} a -> s {windowId = a} :: DeregisterTaskFromMaintenanceWindow)

-- | The ID of the task to remove from the maintenance window.
deregisterTaskFromMaintenanceWindow_windowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Core.Text
deregisterTaskFromMaintenanceWindow_windowTaskId = Lens.lens (\DeregisterTaskFromMaintenanceWindow' {windowTaskId} -> windowTaskId) (\s@DeregisterTaskFromMaintenanceWindow' {} a -> s {windowTaskId = a} :: DeregisterTaskFromMaintenanceWindow)

instance
  Core.AWSRequest
    DeregisterTaskFromMaintenanceWindow
  where
  type
    AWSResponse DeregisterTaskFromMaintenanceWindow =
      DeregisterTaskFromMaintenanceWindowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTaskFromMaintenanceWindowResponse'
            Core.<$> (x Core..?> "WindowTaskId")
            Core.<*> (x Core..?> "WindowId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeregisterTaskFromMaintenanceWindow

instance
  Core.NFData
    DeregisterTaskFromMaintenanceWindow

instance
  Core.ToHeaders
    DeregisterTaskFromMaintenanceWindow
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DeregisterTaskFromMaintenanceWindow" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DeregisterTaskFromMaintenanceWindow
  where
  toJSON DeregisterTaskFromMaintenanceWindow' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WindowId" Core..= windowId),
            Core.Just ("WindowTaskId" Core..= windowTaskId)
          ]
      )

instance
  Core.ToPath
    DeregisterTaskFromMaintenanceWindow
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeregisterTaskFromMaintenanceWindow
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeregisterTaskFromMaintenanceWindowResponse' smart constructor.
data DeregisterTaskFromMaintenanceWindowResponse = DeregisterTaskFromMaintenanceWindowResponse'
  { -- | The ID of the task removed from the maintenance window.
    windowTaskId :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window the task was removed from.
    windowId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeregisterTaskFromMaintenanceWindowResponse
newDeregisterTaskFromMaintenanceWindowResponse
  pHttpStatus_ =
    DeregisterTaskFromMaintenanceWindowResponse'
      { windowTaskId =
          Core.Nothing,
        windowId = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the task removed from the maintenance window.
deregisterTaskFromMaintenanceWindowResponse_windowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Core.Maybe Core.Text)
deregisterTaskFromMaintenanceWindowResponse_windowTaskId = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {windowTaskId} -> windowTaskId) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {windowTaskId = a} :: DeregisterTaskFromMaintenanceWindowResponse)

-- | The ID of the maintenance window the task was removed from.
deregisterTaskFromMaintenanceWindowResponse_windowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Core.Maybe Core.Text)
deregisterTaskFromMaintenanceWindowResponse_windowId = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {windowId} -> windowId) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {windowId = a} :: DeregisterTaskFromMaintenanceWindowResponse)

-- | The response's http status code.
deregisterTaskFromMaintenanceWindowResponse_httpStatus :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse Core.Int
deregisterTaskFromMaintenanceWindowResponse_httpStatus = Lens.lens (\DeregisterTaskFromMaintenanceWindowResponse' {httpStatus} -> httpStatus) (\s@DeregisterTaskFromMaintenanceWindowResponse' {} a -> s {httpStatus = a} :: DeregisterTaskFromMaintenanceWindowResponse)

instance
  Core.NFData
    DeregisterTaskFromMaintenanceWindowResponse
