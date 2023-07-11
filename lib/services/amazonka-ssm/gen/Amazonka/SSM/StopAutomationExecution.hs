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
-- Module      : Amazonka.SSM.StopAutomationExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop an Automation that is currently running.
module Amazonka.SSM.StopAutomationExecution
  ( -- * Creating a Request
    StopAutomationExecution (..),
    newStopAutomationExecution,

    -- * Request Lenses
    stopAutomationExecution_type,
    stopAutomationExecution_automationExecutionId,

    -- * Destructuring the Response
    StopAutomationExecutionResponse (..),
    newStopAutomationExecutionResponse,

    -- * Response Lenses
    stopAutomationExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newStopAutomationExecution' smart constructor.
data StopAutomationExecution = StopAutomationExecution'
  { -- | The stop request type. Valid types include the following: Cancel and
    -- Complete. The default type is Cancel.
    type' :: Prelude.Maybe StopType,
    -- | The execution ID of the Automation to stop.
    automationExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAutomationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'stopAutomationExecution_type' - The stop request type. Valid types include the following: Cancel and
-- Complete. The default type is Cancel.
--
-- 'automationExecutionId', 'stopAutomationExecution_automationExecutionId' - The execution ID of the Automation to stop.
newStopAutomationExecution ::
  -- | 'automationExecutionId'
  Prelude.Text ->
  StopAutomationExecution
newStopAutomationExecution pAutomationExecutionId_ =
  StopAutomationExecution'
    { type' = Prelude.Nothing,
      automationExecutionId = pAutomationExecutionId_
    }

-- | The stop request type. Valid types include the following: Cancel and
-- Complete. The default type is Cancel.
stopAutomationExecution_type :: Lens.Lens' StopAutomationExecution (Prelude.Maybe StopType)
stopAutomationExecution_type = Lens.lens (\StopAutomationExecution' {type'} -> type') (\s@StopAutomationExecution' {} a -> s {type' = a} :: StopAutomationExecution)

-- | The execution ID of the Automation to stop.
stopAutomationExecution_automationExecutionId :: Lens.Lens' StopAutomationExecution Prelude.Text
stopAutomationExecution_automationExecutionId = Lens.lens (\StopAutomationExecution' {automationExecutionId} -> automationExecutionId) (\s@StopAutomationExecution' {} a -> s {automationExecutionId = a} :: StopAutomationExecution)

instance Core.AWSRequest StopAutomationExecution where
  type
    AWSResponse StopAutomationExecution =
      StopAutomationExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAutomationExecutionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopAutomationExecution where
  hashWithSalt _salt StopAutomationExecution' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` automationExecutionId

instance Prelude.NFData StopAutomationExecution where
  rnf StopAutomationExecution' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf automationExecutionId

instance Data.ToHeaders StopAutomationExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.StopAutomationExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopAutomationExecution where
  toJSON StopAutomationExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            Prelude.Just
              ( "AutomationExecutionId"
                  Data..= automationExecutionId
              )
          ]
      )

instance Data.ToPath StopAutomationExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StopAutomationExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopAutomationExecutionResponse' smart constructor.
data StopAutomationExecutionResponse = StopAutomationExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopAutomationExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopAutomationExecutionResponse_httpStatus' - The response's http status code.
newStopAutomationExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopAutomationExecutionResponse
newStopAutomationExecutionResponse pHttpStatus_ =
  StopAutomationExecutionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopAutomationExecutionResponse_httpStatus :: Lens.Lens' StopAutomationExecutionResponse Prelude.Int
stopAutomationExecutionResponse_httpStatus = Lens.lens (\StopAutomationExecutionResponse' {httpStatus} -> httpStatus) (\s@StopAutomationExecutionResponse' {} a -> s {httpStatus = a} :: StopAutomationExecutionResponse)

instance
  Prelude.NFData
    StopAutomationExecutionResponse
  where
  rnf StopAutomationExecutionResponse' {..} =
    Prelude.rnf httpStatus
