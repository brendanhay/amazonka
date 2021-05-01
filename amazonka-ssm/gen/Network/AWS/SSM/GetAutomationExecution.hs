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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetAutomationExecution' smart constructor.
data GetAutomationExecution = GetAutomationExecution'
  { -- | The unique identifier for an existing automation execution to examine.
    -- The execution ID is returned by StartAutomationExecution when the
    -- execution of an Automation document is initiated.
    automationExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetAutomationExecution
newGetAutomationExecution pAutomationExecutionId_ =
  GetAutomationExecution'
    { automationExecutionId =
        pAutomationExecutionId_
    }

-- | The unique identifier for an existing automation execution to examine.
-- The execution ID is returned by StartAutomationExecution when the
-- execution of an Automation document is initiated.
getAutomationExecution_automationExecutionId :: Lens.Lens' GetAutomationExecution Prelude.Text
getAutomationExecution_automationExecutionId = Lens.lens (\GetAutomationExecution' {automationExecutionId} -> automationExecutionId) (\s@GetAutomationExecution' {} a -> s {automationExecutionId = a} :: GetAutomationExecution)

instance Prelude.AWSRequest GetAutomationExecution where
  type
    Rs GetAutomationExecution =
      GetAutomationExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutomationExecutionResponse'
            Prelude.<$> (x Prelude..?> "AutomationExecution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAutomationExecution

instance Prelude.NFData GetAutomationExecution

instance Prelude.ToHeaders GetAutomationExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.GetAutomationExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetAutomationExecution where
  toJSON GetAutomationExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AutomationExecutionId"
                  Prelude..= automationExecutionId
              )
          ]
      )

instance Prelude.ToPath GetAutomationExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAutomationExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAutomationExecutionResponse' smart constructor.
data GetAutomationExecutionResponse = GetAutomationExecutionResponse'
  { -- | Detailed information about the current state of an automation execution.
    automationExecution :: Prelude.Maybe AutomationExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetAutomationExecutionResponse
newGetAutomationExecutionResponse pHttpStatus_ =
  GetAutomationExecutionResponse'
    { automationExecution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the current state of an automation execution.
getAutomationExecutionResponse_automationExecution :: Lens.Lens' GetAutomationExecutionResponse (Prelude.Maybe AutomationExecution)
getAutomationExecutionResponse_automationExecution = Lens.lens (\GetAutomationExecutionResponse' {automationExecution} -> automationExecution) (\s@GetAutomationExecutionResponse' {} a -> s {automationExecution = a} :: GetAutomationExecutionResponse)

-- | The response's http status code.
getAutomationExecutionResponse_httpStatus :: Lens.Lens' GetAutomationExecutionResponse Prelude.Int
getAutomationExecutionResponse_httpStatus = Lens.lens (\GetAutomationExecutionResponse' {httpStatus} -> httpStatus) (\s@GetAutomationExecutionResponse' {} a -> s {httpStatus = a} :: GetAutomationExecutionResponse)

instance
  Prelude.NFData
    GetAutomationExecutionResponse
