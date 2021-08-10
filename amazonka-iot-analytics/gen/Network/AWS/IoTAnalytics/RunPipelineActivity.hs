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
-- Module      : Network.AWS.IoTAnalytics.RunPipelineActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulates the results of running a pipeline activity on a message
-- payload.
module Network.AWS.IoTAnalytics.RunPipelineActivity
  ( -- * Creating a Request
    RunPipelineActivity (..),
    newRunPipelineActivity,

    -- * Request Lenses
    runPipelineActivity_pipelineActivity,
    runPipelineActivity_payloads,

    -- * Destructuring the Response
    RunPipelineActivityResponse (..),
    newRunPipelineActivityResponse,

    -- * Response Lenses
    runPipelineActivityResponse_logResult,
    runPipelineActivityResponse_payloads,
    runPipelineActivityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRunPipelineActivity' smart constructor.
data RunPipelineActivity = RunPipelineActivity'
  { -- | The pipeline activity that is run. This must not be a channel activity
    -- or a datastore activity because these activities are used in a pipeline
    -- only to load the original message and to store the (possibly)
    -- transformed message. If a lambda activity is specified, only
    -- short-running Lambda functions (those with a timeout of less than 30
    -- seconds or less) can be used.
    pipelineActivity :: PipelineActivity,
    -- | The sample message payloads on which the pipeline activity is run.
    payloads :: Prelude.NonEmpty Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunPipelineActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineActivity', 'runPipelineActivity_pipelineActivity' - The pipeline activity that is run. This must not be a channel activity
-- or a datastore activity because these activities are used in a pipeline
-- only to load the original message and to store the (possibly)
-- transformed message. If a lambda activity is specified, only
-- short-running Lambda functions (those with a timeout of less than 30
-- seconds or less) can be used.
--
-- 'payloads', 'runPipelineActivity_payloads' - The sample message payloads on which the pipeline activity is run.
newRunPipelineActivity ::
  -- | 'pipelineActivity'
  PipelineActivity ->
  -- | 'payloads'
  Prelude.NonEmpty Prelude.ByteString ->
  RunPipelineActivity
newRunPipelineActivity pPipelineActivity_ pPayloads_ =
  RunPipelineActivity'
    { pipelineActivity =
        pPipelineActivity_,
      payloads = Lens._Coerce Lens.# pPayloads_
    }

-- | The pipeline activity that is run. This must not be a channel activity
-- or a datastore activity because these activities are used in a pipeline
-- only to load the original message and to store the (possibly)
-- transformed message. If a lambda activity is specified, only
-- short-running Lambda functions (those with a timeout of less than 30
-- seconds or less) can be used.
runPipelineActivity_pipelineActivity :: Lens.Lens' RunPipelineActivity PipelineActivity
runPipelineActivity_pipelineActivity = Lens.lens (\RunPipelineActivity' {pipelineActivity} -> pipelineActivity) (\s@RunPipelineActivity' {} a -> s {pipelineActivity = a} :: RunPipelineActivity)

-- | The sample message payloads on which the pipeline activity is run.
runPipelineActivity_payloads :: Lens.Lens' RunPipelineActivity (Prelude.NonEmpty Prelude.ByteString)
runPipelineActivity_payloads = Lens.lens (\RunPipelineActivity' {payloads} -> payloads) (\s@RunPipelineActivity' {} a -> s {payloads = a} :: RunPipelineActivity) Prelude.. Lens._Coerce

instance Core.AWSRequest RunPipelineActivity where
  type
    AWSResponse RunPipelineActivity =
      RunPipelineActivityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RunPipelineActivityResponse'
            Prelude.<$> (x Core..?> "logResult")
            Prelude.<*> (x Core..?> "payloads")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RunPipelineActivity

instance Prelude.NFData RunPipelineActivity

instance Core.ToHeaders RunPipelineActivity where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON RunPipelineActivity where
  toJSON RunPipelineActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("pipelineActivity" Core..= pipelineActivity),
            Prelude.Just ("payloads" Core..= payloads)
          ]
      )

instance Core.ToPath RunPipelineActivity where
  toPath = Prelude.const "/pipelineactivities/run"

instance Core.ToQuery RunPipelineActivity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRunPipelineActivityResponse' smart constructor.
data RunPipelineActivityResponse = RunPipelineActivityResponse'
  { -- | In case the pipeline activity fails, the log message that is generated.
    logResult :: Prelude.Maybe Prelude.Text,
    -- | The enriched or transformed sample message payloads as base64-encoded
    -- strings. (The results of running the pipeline activity on each input
    -- sample message payload, encoded in base64.)
    payloads :: Prelude.Maybe (Prelude.NonEmpty Core.Base64),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunPipelineActivityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logResult', 'runPipelineActivityResponse_logResult' - In case the pipeline activity fails, the log message that is generated.
--
-- 'payloads', 'runPipelineActivityResponse_payloads' - The enriched or transformed sample message payloads as base64-encoded
-- strings. (The results of running the pipeline activity on each input
-- sample message payload, encoded in base64.)
--
-- 'httpStatus', 'runPipelineActivityResponse_httpStatus' - The response's http status code.
newRunPipelineActivityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RunPipelineActivityResponse
newRunPipelineActivityResponse pHttpStatus_ =
  RunPipelineActivityResponse'
    { logResult =
        Prelude.Nothing,
      payloads = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | In case the pipeline activity fails, the log message that is generated.
runPipelineActivityResponse_logResult :: Lens.Lens' RunPipelineActivityResponse (Prelude.Maybe Prelude.Text)
runPipelineActivityResponse_logResult = Lens.lens (\RunPipelineActivityResponse' {logResult} -> logResult) (\s@RunPipelineActivityResponse' {} a -> s {logResult = a} :: RunPipelineActivityResponse)

-- | The enriched or transformed sample message payloads as base64-encoded
-- strings. (The results of running the pipeline activity on each input
-- sample message payload, encoded in base64.)
runPipelineActivityResponse_payloads :: Lens.Lens' RunPipelineActivityResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.ByteString))
runPipelineActivityResponse_payloads = Lens.lens (\RunPipelineActivityResponse' {payloads} -> payloads) (\s@RunPipelineActivityResponse' {} a -> s {payloads = a} :: RunPipelineActivityResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
runPipelineActivityResponse_httpStatus :: Lens.Lens' RunPipelineActivityResponse Prelude.Int
runPipelineActivityResponse_httpStatus = Lens.lens (\RunPipelineActivityResponse' {httpStatus} -> httpStatus) (\s@RunPipelineActivityResponse' {} a -> s {httpStatus = a} :: RunPipelineActivityResponse)

instance Prelude.NFData RunPipelineActivityResponse
