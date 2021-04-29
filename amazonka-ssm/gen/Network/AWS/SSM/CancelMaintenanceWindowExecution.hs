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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCancelMaintenanceWindowExecution' smart constructor.
data CancelMaintenanceWindowExecution = CancelMaintenanceWindowExecution'
  { -- | The ID of the maintenance window execution to stop.
    windowExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CancelMaintenanceWindowExecution
newCancelMaintenanceWindowExecution
  pWindowExecutionId_ =
    CancelMaintenanceWindowExecution'
      { windowExecutionId =
          pWindowExecutionId_
      }

-- | The ID of the maintenance window execution to stop.
cancelMaintenanceWindowExecution_windowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecution Prelude.Text
cancelMaintenanceWindowExecution_windowExecutionId = Lens.lens (\CancelMaintenanceWindowExecution' {windowExecutionId} -> windowExecutionId) (\s@CancelMaintenanceWindowExecution' {} a -> s {windowExecutionId = a} :: CancelMaintenanceWindowExecution)

instance
  Prelude.AWSRequest
    CancelMaintenanceWindowExecution
  where
  type
    Rs CancelMaintenanceWindowExecution =
      CancelMaintenanceWindowExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelMaintenanceWindowExecutionResponse'
            Prelude.<$> (x Prelude..?> "WindowExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelMaintenanceWindowExecution

instance
  Prelude.NFData
    CancelMaintenanceWindowExecution

instance
  Prelude.ToHeaders
    CancelMaintenanceWindowExecution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.CancelMaintenanceWindowExecution" ::
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
    CancelMaintenanceWindowExecution
  where
  toJSON CancelMaintenanceWindowExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WindowExecutionId" Prelude..= windowExecutionId)
          ]
      )

instance
  Prelude.ToPath
    CancelMaintenanceWindowExecution
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CancelMaintenanceWindowExecution
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelMaintenanceWindowExecutionResponse' smart constructor.
data CancelMaintenanceWindowExecutionResponse = CancelMaintenanceWindowExecutionResponse'
  { -- | The ID of the maintenance window execution that has been stopped.
    windowExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CancelMaintenanceWindowExecutionResponse
newCancelMaintenanceWindowExecutionResponse
  pHttpStatus_ =
    CancelMaintenanceWindowExecutionResponse'
      { windowExecutionId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the maintenance window execution that has been stopped.
cancelMaintenanceWindowExecutionResponse_windowExecutionId :: Lens.Lens' CancelMaintenanceWindowExecutionResponse (Prelude.Maybe Prelude.Text)
cancelMaintenanceWindowExecutionResponse_windowExecutionId = Lens.lens (\CancelMaintenanceWindowExecutionResponse' {windowExecutionId} -> windowExecutionId) (\s@CancelMaintenanceWindowExecutionResponse' {} a -> s {windowExecutionId = a} :: CancelMaintenanceWindowExecutionResponse)

-- | The response's http status code.
cancelMaintenanceWindowExecutionResponse_httpStatus :: Lens.Lens' CancelMaintenanceWindowExecutionResponse Prelude.Int
cancelMaintenanceWindowExecutionResponse_httpStatus = Lens.lens (\CancelMaintenanceWindowExecutionResponse' {httpStatus} -> httpStatus) (\s@CancelMaintenanceWindowExecutionResponse' {} a -> s {httpStatus = a} :: CancelMaintenanceWindowExecutionResponse)

instance
  Prelude.NFData
    CancelMaintenanceWindowExecutionResponse
