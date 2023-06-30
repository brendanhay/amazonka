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
-- Module      : Amazonka.SSM.SendAutomationSignal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to an Automation execution to change the current behavior
-- or status of the execution.
module Amazonka.SSM.SendAutomationSignal
  ( -- * Creating a Request
    SendAutomationSignal (..),
    newSendAutomationSignal,

    -- * Request Lenses
    sendAutomationSignal_payload,
    sendAutomationSignal_automationExecutionId,
    sendAutomationSignal_signalType,

    -- * Destructuring the Response
    SendAutomationSignalResponse (..),
    newSendAutomationSignalResponse,

    -- * Response Lenses
    sendAutomationSignalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newSendAutomationSignal' smart constructor.
data SendAutomationSignal = SendAutomationSignal'
  { -- | The data sent with the signal. The data schema depends on the type of
    -- signal used in the request.
    --
    -- For @Approve@ and @Reject@ signal types, the payload is an optional
    -- comment that you can send with the signal type. For example:
    --
    -- @Comment=\"Looks good\"@
    --
    -- For @StartStep@ and @Resume@ signal types, you must send the name of the
    -- Automation step to start or resume as the payload. For example:
    --
    -- @StepName=\"step1\"@
    --
    -- For the @StopStep@ signal type, you must send the step execution ID as
    -- the payload. For example:
    --
    -- @StepExecutionId=\"97fff367-fc5a-4299-aed8-0123456789ab\"@
    payload :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The unique identifier for an existing Automation execution that you want
    -- to send the signal to.
    automationExecutionId :: Prelude.Text,
    -- | The type of signal to send to an Automation execution.
    signalType :: SignalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendAutomationSignal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'sendAutomationSignal_payload' - The data sent with the signal. The data schema depends on the type of
-- signal used in the request.
--
-- For @Approve@ and @Reject@ signal types, the payload is an optional
-- comment that you can send with the signal type. For example:
--
-- @Comment=\"Looks good\"@
--
-- For @StartStep@ and @Resume@ signal types, you must send the name of the
-- Automation step to start or resume as the payload. For example:
--
-- @StepName=\"step1\"@
--
-- For the @StopStep@ signal type, you must send the step execution ID as
-- the payload. For example:
--
-- @StepExecutionId=\"97fff367-fc5a-4299-aed8-0123456789ab\"@
--
-- 'automationExecutionId', 'sendAutomationSignal_automationExecutionId' - The unique identifier for an existing Automation execution that you want
-- to send the signal to.
--
-- 'signalType', 'sendAutomationSignal_signalType' - The type of signal to send to an Automation execution.
newSendAutomationSignal ::
  -- | 'automationExecutionId'
  Prelude.Text ->
  -- | 'signalType'
  SignalType ->
  SendAutomationSignal
newSendAutomationSignal
  pAutomationExecutionId_
  pSignalType_ =
    SendAutomationSignal'
      { payload = Prelude.Nothing,
        automationExecutionId = pAutomationExecutionId_,
        signalType = pSignalType_
      }

-- | The data sent with the signal. The data schema depends on the type of
-- signal used in the request.
--
-- For @Approve@ and @Reject@ signal types, the payload is an optional
-- comment that you can send with the signal type. For example:
--
-- @Comment=\"Looks good\"@
--
-- For @StartStep@ and @Resume@ signal types, you must send the name of the
-- Automation step to start or resume as the payload. For example:
--
-- @StepName=\"step1\"@
--
-- For the @StopStep@ signal type, you must send the step execution ID as
-- the payload. For example:
--
-- @StepExecutionId=\"97fff367-fc5a-4299-aed8-0123456789ab\"@
sendAutomationSignal_payload :: Lens.Lens' SendAutomationSignal (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
sendAutomationSignal_payload = Lens.lens (\SendAutomationSignal' {payload} -> payload) (\s@SendAutomationSignal' {} a -> s {payload = a} :: SendAutomationSignal) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for an existing Automation execution that you want
-- to send the signal to.
sendAutomationSignal_automationExecutionId :: Lens.Lens' SendAutomationSignal Prelude.Text
sendAutomationSignal_automationExecutionId = Lens.lens (\SendAutomationSignal' {automationExecutionId} -> automationExecutionId) (\s@SendAutomationSignal' {} a -> s {automationExecutionId = a} :: SendAutomationSignal)

-- | The type of signal to send to an Automation execution.
sendAutomationSignal_signalType :: Lens.Lens' SendAutomationSignal SignalType
sendAutomationSignal_signalType = Lens.lens (\SendAutomationSignal' {signalType} -> signalType) (\s@SendAutomationSignal' {} a -> s {signalType = a} :: SendAutomationSignal)

instance Core.AWSRequest SendAutomationSignal where
  type
    AWSResponse SendAutomationSignal =
      SendAutomationSignalResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendAutomationSignalResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendAutomationSignal where
  hashWithSalt _salt SendAutomationSignal' {..} =
    _salt
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` automationExecutionId
      `Prelude.hashWithSalt` signalType

instance Prelude.NFData SendAutomationSignal where
  rnf SendAutomationSignal' {..} =
    Prelude.rnf payload
      `Prelude.seq` Prelude.rnf automationExecutionId
      `Prelude.seq` Prelude.rnf signalType

instance Data.ToHeaders SendAutomationSignal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.SendAutomationSignal" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendAutomationSignal where
  toJSON SendAutomationSignal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Payload" Data..=) Prelude.<$> payload,
            Prelude.Just
              ( "AutomationExecutionId"
                  Data..= automationExecutionId
              ),
            Prelude.Just ("SignalType" Data..= signalType)
          ]
      )

instance Data.ToPath SendAutomationSignal where
  toPath = Prelude.const "/"

instance Data.ToQuery SendAutomationSignal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendAutomationSignalResponse' smart constructor.
data SendAutomationSignalResponse = SendAutomationSignalResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendAutomationSignalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendAutomationSignalResponse_httpStatus' - The response's http status code.
newSendAutomationSignalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendAutomationSignalResponse
newSendAutomationSignalResponse pHttpStatus_ =
  SendAutomationSignalResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
sendAutomationSignalResponse_httpStatus :: Lens.Lens' SendAutomationSignalResponse Prelude.Int
sendAutomationSignalResponse_httpStatus = Lens.lens (\SendAutomationSignalResponse' {httpStatus} -> httpStatus) (\s@SendAutomationSignalResponse' {} a -> s {httpStatus = a} :: SendAutomationSignalResponse)

instance Prelude.NFData SendAutomationSignalResponse where
  rnf SendAutomationSignalResponse' {..} =
    Prelude.rnf httpStatus
