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
-- Module      : Network.AWS.SSM.CancelMaintenanceWindowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a maintenance window execution that is already in progress and
-- cancels any tasks in the window that have not already starting running.
-- (Tasks already in progress will continue to completion.)
module Network.AWS.SSM.CancelMaintenanceWindowExecution
  ( -- * Creating a Request
    CancelMaintenanceWindowExecution (..),
    newCancelMaintenanceWindowExecution,

    -- * Request Lenses
    cancelMaintenanceWindowExecution_windowExecutionId,

    -- * Destructuring the Response
    CancelMaintenanceWindowExecutionResponse (..),
    newCancelMaintenanceWindowExecutionResponse,

    -- * Response Lenses
    cancelMaintenanceWindowExecutionResponse_windowExecutionId,
    cancelMaintenanceWindowExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCancelMaintenanceWindowExecution' smart constructor.
data CancelMaintenanceWindowExecution = CancelMaintenanceWindowExecution'
  { -- | The ID of the maintenance window execution to stop.
    windowExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelMaintenanceWindowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowExecutionId', 'cancelMaintenanceWindowExecution_windowExecutionId' - The ID of the maintenance window execution to stop.
newCancelMaintenanceWindowExecution ::
  -- | 'windowExecutionId'
  Core.Text ->
  CancelMaintenanceWindowExecution
newCancelMaintenanceWindowExecution
  pWindowExecutionId_ =
    CancelMaintenanceWindowExecution'
      { windowExecutionId =
          pWindowExecutionId_
      }

-- | The ID of the maintenance window execution to stop.
cancelMaintenanceWindowExecution_windowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecution Core.Text
cancelMaintenanceWindowExecution_windowExecutionId = Lens.lens (\CancelMaintenanceWindowExecution' {windowExecutionId} -> windowExecutionId) (\s@CancelMaintenanceWindowExecution' {} a -> s {windowExecutionId = a} :: CancelMaintenanceWindowExecution)

instance
  Core.AWSRequest
    CancelMaintenanceWindowExecution
  where
  type
    AWSResponse CancelMaintenanceWindowExecution =
      CancelMaintenanceWindowExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelMaintenanceWindowExecutionResponse'
            Core.<$> (x Core..?> "WindowExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CancelMaintenanceWindowExecution

instance Core.NFData CancelMaintenanceWindowExecution

instance
  Core.ToHeaders
    CancelMaintenanceWindowExecution
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.CancelMaintenanceWindowExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CancelMaintenanceWindowExecution where
  toJSON CancelMaintenanceWindowExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("WindowExecutionId" Core..= windowExecutionId)
          ]
      )

instance Core.ToPath CancelMaintenanceWindowExecution where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CancelMaintenanceWindowExecution
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCancelMaintenanceWindowExecutionResponse' smart constructor.
data CancelMaintenanceWindowExecutionResponse = CancelMaintenanceWindowExecutionResponse'
  { -- | The ID of the maintenance window execution that has been stopped.
    windowExecutionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelMaintenanceWindowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowExecutionId', 'cancelMaintenanceWindowExecutionResponse_windowExecutionId' - The ID of the maintenance window execution that has been stopped.
--
-- 'httpStatus', 'cancelMaintenanceWindowExecutionResponse_httpStatus' - The response's http status code.
newCancelMaintenanceWindowExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelMaintenanceWindowExecutionResponse
newCancelMaintenanceWindowExecutionResponse
  pHttpStatus_ =
    CancelMaintenanceWindowExecutionResponse'
      { windowExecutionId =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the maintenance window execution that has been stopped.
cancelMaintenanceWindowExecutionResponse_windowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecutionResponse (Core.Maybe Core.Text)
cancelMaintenanceWindowExecutionResponse_windowExecutionId = Lens.lens (\CancelMaintenanceWindowExecutionResponse' {windowExecutionId} -> windowExecutionId) (\s@CancelMaintenanceWindowExecutionResponse' {} a -> s {windowExecutionId = a} :: CancelMaintenanceWindowExecutionResponse)

-- | The response's http status code.
cancelMaintenanceWindowExecutionResponse_httpStatus :: Lens.Lens' CancelMaintenanceWindowExecutionResponse Core.Int
cancelMaintenanceWindowExecutionResponse_httpStatus = Lens.lens (\CancelMaintenanceWindowExecutionResponse' {httpStatus} -> httpStatus) (\s@CancelMaintenanceWindowExecutionResponse' {} a -> s {httpStatus = a} :: CancelMaintenanceWindowExecutionResponse)

instance
  Core.NFData
    CancelMaintenanceWindowExecutionResponse
