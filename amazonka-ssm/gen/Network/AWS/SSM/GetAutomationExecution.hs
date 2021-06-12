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
-- Module      : Network.AWS.SSM.GetAutomationExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detailed information about a particular Automation execution.
module Network.AWS.SSM.GetAutomationExecution
  ( -- * Creating a Request
    GetAutomationExecution (..),
    newGetAutomationExecution,

    -- * Request Lenses
    getAutomationExecution_automationExecutionId,

    -- * Destructuring the Response
    GetAutomationExecutionResponse (..),
    newGetAutomationExecutionResponse,

    -- * Response Lenses
    getAutomationExecutionResponse_automationExecution,
    getAutomationExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetAutomationExecution' smart constructor.
data GetAutomationExecution = GetAutomationExecution'
  { -- | The unique identifier for an existing automation execution to examine.
    -- The execution ID is returned by StartAutomationExecution when the
    -- execution of an Automation document is initiated.
    automationExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAutomationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automationExecutionId', 'getAutomationExecution_automationExecutionId' - The unique identifier for an existing automation execution to examine.
-- The execution ID is returned by StartAutomationExecution when the
-- execution of an Automation document is initiated.
newGetAutomationExecution ::
  -- | 'automationExecutionId'
  Core.Text ->
  GetAutomationExecution
newGetAutomationExecution pAutomationExecutionId_ =
  GetAutomationExecution'
    { automationExecutionId =
        pAutomationExecutionId_
    }

-- | The unique identifier for an existing automation execution to examine.
-- The execution ID is returned by StartAutomationExecution when the
-- execution of an Automation document is initiated.
getAutomationExecution_automationExecutionId :: Lens.Lens' GetAutomationExecution Core.Text
getAutomationExecution_automationExecutionId = Lens.lens (\GetAutomationExecution' {automationExecutionId} -> automationExecutionId) (\s@GetAutomationExecution' {} a -> s {automationExecutionId = a} :: GetAutomationExecution)

instance Core.AWSRequest GetAutomationExecution where
  type
    AWSResponse GetAutomationExecution =
      GetAutomationExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutomationExecutionResponse'
            Core.<$> (x Core..?> "AutomationExecution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAutomationExecution

instance Core.NFData GetAutomationExecution

instance Core.ToHeaders GetAutomationExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetAutomationExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAutomationExecution where
  toJSON GetAutomationExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "AutomationExecutionId"
                  Core..= automationExecutionId
              )
          ]
      )

instance Core.ToPath GetAutomationExecution where
  toPath = Core.const "/"

instance Core.ToQuery GetAutomationExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAutomationExecutionResponse' smart constructor.
data GetAutomationExecutionResponse = GetAutomationExecutionResponse'
  { -- | Detailed information about the current state of an automation execution.
    automationExecution :: Core.Maybe AutomationExecution,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAutomationExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automationExecution', 'getAutomationExecutionResponse_automationExecution' - Detailed information about the current state of an automation execution.
--
-- 'httpStatus', 'getAutomationExecutionResponse_httpStatus' - The response's http status code.
newGetAutomationExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAutomationExecutionResponse
newGetAutomationExecutionResponse pHttpStatus_ =
  GetAutomationExecutionResponse'
    { automationExecution =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the current state of an automation execution.
getAutomationExecutionResponse_automationExecution :: Lens.Lens' GetAutomationExecutionResponse (Core.Maybe AutomationExecution)
getAutomationExecutionResponse_automationExecution = Lens.lens (\GetAutomationExecutionResponse' {automationExecution} -> automationExecution) (\s@GetAutomationExecutionResponse' {} a -> s {automationExecution = a} :: GetAutomationExecutionResponse)

-- | The response's http status code.
getAutomationExecutionResponse_httpStatus :: Lens.Lens' GetAutomationExecutionResponse Core.Int
getAutomationExecutionResponse_httpStatus = Lens.lens (\GetAutomationExecutionResponse' {httpStatus} -> httpStatus) (\s@GetAutomationExecutionResponse' {} a -> s {httpStatus = a} :: GetAutomationExecutionResponse)

instance Core.NFData GetAutomationExecutionResponse
