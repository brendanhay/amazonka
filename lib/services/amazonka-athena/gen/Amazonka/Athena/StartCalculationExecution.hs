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
-- Module      : Amazonka.Athena.StartCalculationExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits calculations for execution within a session. You can supply the
-- code to run as an inline code block within the request or as an Amazon
-- S3 URL.
module Amazonka.Athena.StartCalculationExecution
  ( -- * Creating a Request
    StartCalculationExecution (..),
    newStartCalculationExecution,

    -- * Request Lenses
    startCalculationExecution_calculationConfiguration,
    startCalculationExecution_clientRequestToken,
    startCalculationExecution_codeBlock,
    startCalculationExecution_description,
    startCalculationExecution_sessionId,

    -- * Destructuring the Response
    StartCalculationExecutionResponse (..),
    newStartCalculationExecutionResponse,

    -- * Response Lenses
    startCalculationExecutionResponse_calculationExecutionId,
    startCalculationExecutionResponse_state,
    startCalculationExecutionResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartCalculationExecution' smart constructor.
data StartCalculationExecution = StartCalculationExecution'
  { -- | Contains configuration information for the calculation.
    calculationConfiguration :: Prelude.Maybe CalculationConfiguration,
    -- | A unique case-sensitive string used to ensure the request to create the
    -- calculation is idempotent (executes only once). If another
    -- @StartCalculationExecutionRequest@ is received, the same response is
    -- returned and another calculation is not created. If a parameter has
    -- changed, an error is returned.
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for users. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A string that contains the code of the calculation.
    codeBlock :: Prelude.Maybe Prelude.Text,
    -- | A description of the calculation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The session ID.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCalculationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationConfiguration', 'startCalculationExecution_calculationConfiguration' - Contains configuration information for the calculation.
--
-- 'clientRequestToken', 'startCalculationExecution_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- calculation is idempotent (executes only once). If another
-- @StartCalculationExecutionRequest@ is received, the same response is
-- returned and another calculation is not created. If a parameter has
-- changed, an error is returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'codeBlock', 'startCalculationExecution_codeBlock' - A string that contains the code of the calculation.
--
-- 'description', 'startCalculationExecution_description' - A description of the calculation.
--
-- 'sessionId', 'startCalculationExecution_sessionId' - The session ID.
newStartCalculationExecution ::
  -- | 'sessionId'
  Prelude.Text ->
  StartCalculationExecution
newStartCalculationExecution pSessionId_ =
  StartCalculationExecution'
    { calculationConfiguration =
        Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      codeBlock = Prelude.Nothing,
      description = Prelude.Nothing,
      sessionId = pSessionId_
    }

-- | Contains configuration information for the calculation.
startCalculationExecution_calculationConfiguration :: Lens.Lens' StartCalculationExecution (Prelude.Maybe CalculationConfiguration)
startCalculationExecution_calculationConfiguration = Lens.lens (\StartCalculationExecution' {calculationConfiguration} -> calculationConfiguration) (\s@StartCalculationExecution' {} a -> s {calculationConfiguration = a} :: StartCalculationExecution)

-- | A unique case-sensitive string used to ensure the request to create the
-- calculation is idempotent (executes only once). If another
-- @StartCalculationExecutionRequest@ is received, the same response is
-- returned and another calculation is not created. If a parameter has
-- changed, an error is returned.
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for users. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
startCalculationExecution_clientRequestToken :: Lens.Lens' StartCalculationExecution (Prelude.Maybe Prelude.Text)
startCalculationExecution_clientRequestToken = Lens.lens (\StartCalculationExecution' {clientRequestToken} -> clientRequestToken) (\s@StartCalculationExecution' {} a -> s {clientRequestToken = a} :: StartCalculationExecution)

-- | A string that contains the code of the calculation.
startCalculationExecution_codeBlock :: Lens.Lens' StartCalculationExecution (Prelude.Maybe Prelude.Text)
startCalculationExecution_codeBlock = Lens.lens (\StartCalculationExecution' {codeBlock} -> codeBlock) (\s@StartCalculationExecution' {} a -> s {codeBlock = a} :: StartCalculationExecution)

-- | A description of the calculation.
startCalculationExecution_description :: Lens.Lens' StartCalculationExecution (Prelude.Maybe Prelude.Text)
startCalculationExecution_description = Lens.lens (\StartCalculationExecution' {description} -> description) (\s@StartCalculationExecution' {} a -> s {description = a} :: StartCalculationExecution)

-- | The session ID.
startCalculationExecution_sessionId :: Lens.Lens' StartCalculationExecution Prelude.Text
startCalculationExecution_sessionId = Lens.lens (\StartCalculationExecution' {sessionId} -> sessionId) (\s@StartCalculationExecution' {} a -> s {sessionId = a} :: StartCalculationExecution)

instance Core.AWSRequest StartCalculationExecution where
  type
    AWSResponse StartCalculationExecution =
      StartCalculationExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartCalculationExecutionResponse'
            Prelude.<$> (x Data..?> "CalculationExecutionId")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCalculationExecution where
  hashWithSalt _salt StartCalculationExecution' {..} =
    _salt
      `Prelude.hashWithSalt` calculationConfiguration
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` codeBlock
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sessionId

instance Prelude.NFData StartCalculationExecution where
  rnf StartCalculationExecution' {..} =
    Prelude.rnf calculationConfiguration
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf codeBlock
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sessionId

instance Data.ToHeaders StartCalculationExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.StartCalculationExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartCalculationExecution where
  toJSON StartCalculationExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CalculationConfiguration" Data..=)
              Prelude.<$> calculationConfiguration,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("CodeBlock" Data..=) Prelude.<$> codeBlock,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("SessionId" Data..= sessionId)
          ]
      )

instance Data.ToPath StartCalculationExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartCalculationExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCalculationExecutionResponse' smart constructor.
data StartCalculationExecutionResponse = StartCalculationExecutionResponse'
  { -- | The calculation execution UUID.
    calculationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | @CREATING@ - The calculation is in the process of being created.
    --
    -- @CREATED@ - The calculation has been created and is ready to run.
    --
    -- @QUEUED@ - The calculation has been queued for processing.
    --
    -- @RUNNING@ - The calculation is running.
    --
    -- @CANCELING@ - A request to cancel the calculation has been received and
    -- the system is working to stop it.
    --
    -- @CANCELED@ - The calculation is no longer running as the result of a
    -- cancel request.
    --
    -- @COMPLETED@ - The calculation has completed without error.
    --
    -- @FAILED@ - The calculation failed and is no longer running.
    state :: Prelude.Maybe CalculationExecutionState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCalculationExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationExecutionId', 'startCalculationExecutionResponse_calculationExecutionId' - The calculation execution UUID.
--
-- 'state', 'startCalculationExecutionResponse_state' - @CREATING@ - The calculation is in the process of being created.
--
-- @CREATED@ - The calculation has been created and is ready to run.
--
-- @QUEUED@ - The calculation has been queued for processing.
--
-- @RUNNING@ - The calculation is running.
--
-- @CANCELING@ - A request to cancel the calculation has been received and
-- the system is working to stop it.
--
-- @CANCELED@ - The calculation is no longer running as the result of a
-- cancel request.
--
-- @COMPLETED@ - The calculation has completed without error.
--
-- @FAILED@ - The calculation failed and is no longer running.
--
-- 'httpStatus', 'startCalculationExecutionResponse_httpStatus' - The response's http status code.
newStartCalculationExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCalculationExecutionResponse
newStartCalculationExecutionResponse pHttpStatus_ =
  StartCalculationExecutionResponse'
    { calculationExecutionId =
        Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The calculation execution UUID.
startCalculationExecutionResponse_calculationExecutionId :: Lens.Lens' StartCalculationExecutionResponse (Prelude.Maybe Prelude.Text)
startCalculationExecutionResponse_calculationExecutionId = Lens.lens (\StartCalculationExecutionResponse' {calculationExecutionId} -> calculationExecutionId) (\s@StartCalculationExecutionResponse' {} a -> s {calculationExecutionId = a} :: StartCalculationExecutionResponse)

-- | @CREATING@ - The calculation is in the process of being created.
--
-- @CREATED@ - The calculation has been created and is ready to run.
--
-- @QUEUED@ - The calculation has been queued for processing.
--
-- @RUNNING@ - The calculation is running.
--
-- @CANCELING@ - A request to cancel the calculation has been received and
-- the system is working to stop it.
--
-- @CANCELED@ - The calculation is no longer running as the result of a
-- cancel request.
--
-- @COMPLETED@ - The calculation has completed without error.
--
-- @FAILED@ - The calculation failed and is no longer running.
startCalculationExecutionResponse_state :: Lens.Lens' StartCalculationExecutionResponse (Prelude.Maybe CalculationExecutionState)
startCalculationExecutionResponse_state = Lens.lens (\StartCalculationExecutionResponse' {state} -> state) (\s@StartCalculationExecutionResponse' {} a -> s {state = a} :: StartCalculationExecutionResponse)

-- | The response's http status code.
startCalculationExecutionResponse_httpStatus :: Lens.Lens' StartCalculationExecutionResponse Prelude.Int
startCalculationExecutionResponse_httpStatus = Lens.lens (\StartCalculationExecutionResponse' {httpStatus} -> httpStatus) (\s@StartCalculationExecutionResponse' {} a -> s {httpStatus = a} :: StartCalculationExecutionResponse)

instance
  Prelude.NFData
    StartCalculationExecutionResponse
  where
  rnf StartCalculationExecutionResponse' {..} =
    Prelude.rnf calculationExecutionId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
