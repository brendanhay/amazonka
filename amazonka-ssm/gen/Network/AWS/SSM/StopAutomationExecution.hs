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
-- Module      : Network.AWS.SSM.StopAutomationExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop an Automation that is currently running.
module Network.AWS.SSM.StopAutomationExecution
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newStopAutomationExecution' smart constructor.
data StopAutomationExecution = StopAutomationExecution'
  { -- | The stop request type. Valid types include the following: Cancel and
    -- Complete. The default type is Cancel.
    type' :: Core.Maybe StopType,
    -- | The execution ID of the Automation to stop.
    automationExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StopAutomationExecution
newStopAutomationExecution pAutomationExecutionId_ =
  StopAutomationExecution'
    { type' = Core.Nothing,
      automationExecutionId = pAutomationExecutionId_
    }

-- | The stop request type. Valid types include the following: Cancel and
-- Complete. The default type is Cancel.
stopAutomationExecution_type :: Lens.Lens' StopAutomationExecution (Core.Maybe StopType)
stopAutomationExecution_type = Lens.lens (\StopAutomationExecution' {type'} -> type') (\s@StopAutomationExecution' {} a -> s {type' = a} :: StopAutomationExecution)

-- | The execution ID of the Automation to stop.
stopAutomationExecution_automationExecutionId :: Lens.Lens' StopAutomationExecution Core.Text
stopAutomationExecution_automationExecutionId = Lens.lens (\StopAutomationExecution' {automationExecutionId} -> automationExecutionId) (\s@StopAutomationExecution' {} a -> s {automationExecutionId = a} :: StopAutomationExecution)

instance Core.AWSRequest StopAutomationExecution where
  type
    AWSResponse StopAutomationExecution =
      StopAutomationExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAutomationExecutionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopAutomationExecution

instance Core.NFData StopAutomationExecution

instance Core.ToHeaders StopAutomationExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.StopAutomationExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopAutomationExecution where
  toJSON StopAutomationExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Type" Core..=) Core.<$> type',
            Core.Just
              ( "AutomationExecutionId"
                  Core..= automationExecutionId
              )
          ]
      )

instance Core.ToPath StopAutomationExecution where
  toPath = Core.const "/"

instance Core.ToQuery StopAutomationExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopAutomationExecutionResponse' smart constructor.
data StopAutomationExecutionResponse = StopAutomationExecutionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StopAutomationExecutionResponse
newStopAutomationExecutionResponse pHttpStatus_ =
  StopAutomationExecutionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopAutomationExecutionResponse_httpStatus :: Lens.Lens' StopAutomationExecutionResponse Core.Int
stopAutomationExecutionResponse_httpStatus = Lens.lens (\StopAutomationExecutionResponse' {httpStatus} -> httpStatus) (\s@StopAutomationExecutionResponse' {} a -> s {httpStatus = a} :: StopAutomationExecutionResponse)

instance Core.NFData StopAutomationExecutionResponse
