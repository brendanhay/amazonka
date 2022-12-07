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
-- Module      : Amazonka.SSM.CancelMaintenanceWindowExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a maintenance window execution that is already in progress and
-- cancels any tasks in the window that haven\'t already starting running.
-- Tasks already in progress will continue to completion.
module Amazonka.SSM.CancelMaintenanceWindowExecution
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCancelMaintenanceWindowExecution' smart constructor.
data CancelMaintenanceWindowExecution = CancelMaintenanceWindowExecution'
  { -- | The ID of the maintenance window execution to stop.
    windowExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    CancelMaintenanceWindowExecution
  where
  type
    AWSResponse CancelMaintenanceWindowExecution =
      CancelMaintenanceWindowExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelMaintenanceWindowExecutionResponse'
            Prelude.<$> (x Data..?> "WindowExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CancelMaintenanceWindowExecution
  where
  hashWithSalt
    _salt
    CancelMaintenanceWindowExecution' {..} =
      _salt `Prelude.hashWithSalt` windowExecutionId

instance
  Prelude.NFData
    CancelMaintenanceWindowExecution
  where
  rnf CancelMaintenanceWindowExecution' {..} =
    Prelude.rnf windowExecutionId

instance
  Data.ToHeaders
    CancelMaintenanceWindowExecution
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.CancelMaintenanceWindowExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelMaintenanceWindowExecution where
  toJSON CancelMaintenanceWindowExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WindowExecutionId" Data..= windowExecutionId)
          ]
      )

instance Data.ToPath CancelMaintenanceWindowExecution where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CancelMaintenanceWindowExecutionResponse' {..} =
    Prelude.rnf windowExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
