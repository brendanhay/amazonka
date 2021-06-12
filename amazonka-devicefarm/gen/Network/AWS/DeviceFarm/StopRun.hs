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
-- Module      : Network.AWS.DeviceFarm.StopRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current test run. AWS Device Farm
-- immediately stops the run on devices where tests have not started. You
-- are not billed for these devices. On devices where tests have started
-- executing, setup suite and teardown suite tests run to completion on
-- those devices. You are billed for setup, teardown, and any tests that
-- were in progress or already completed.
module Network.AWS.DeviceFarm.StopRun
  ( -- * Creating a Request
    StopRun (..),
    newStopRun,

    -- * Request Lenses
    stopRun_arn,

    -- * Destructuring the Response
    StopRunResponse (..),
    newStopRunResponse,

    -- * Response Lenses
    stopRunResponse_run,
    stopRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to stop a specific run.
--
-- /See:/ 'newStopRun' smart constructor.
data StopRun = StopRun'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm run to
    -- stop.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopRun_arn' - Represents the Amazon Resource Name (ARN) of the Device Farm run to
-- stop.
newStopRun ::
  -- | 'arn'
  Core.Text ->
  StopRun
newStopRun pArn_ = StopRun' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm run to
-- stop.
stopRun_arn :: Lens.Lens' StopRun Core.Text
stopRun_arn = Lens.lens (\StopRun' {arn} -> arn) (\s@StopRun' {} a -> s {arn = a} :: StopRun)

instance Core.AWSRequest StopRun where
  type AWSResponse StopRun = StopRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopRunResponse'
            Core.<$> (x Core..?> "run")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopRun

instance Core.NFData StopRun

instance Core.ToHeaders StopRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.StopRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopRun where
  toJSON StopRun' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath StopRun where
  toPath = Core.const "/"

instance Core.ToQuery StopRun where
  toQuery = Core.const Core.mempty

-- | Represents the results of your stop run attempt.
--
-- /See:/ 'newStopRunResponse' smart constructor.
data StopRunResponse = StopRunResponse'
  { -- | The run that was stopped.
    run :: Core.Maybe Run,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'run', 'stopRunResponse_run' - The run that was stopped.
--
-- 'httpStatus', 'stopRunResponse_httpStatus' - The response's http status code.
newStopRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopRunResponse
newStopRunResponse pHttpStatus_ =
  StopRunResponse'
    { run = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The run that was stopped.
stopRunResponse_run :: Lens.Lens' StopRunResponse (Core.Maybe Run)
stopRunResponse_run = Lens.lens (\StopRunResponse' {run} -> run) (\s@StopRunResponse' {} a -> s {run = a} :: StopRunResponse)

-- | The response's http status code.
stopRunResponse_httpStatus :: Lens.Lens' StopRunResponse Core.Int
stopRunResponse_httpStatus = Lens.lens (\StopRunResponse' {httpStatus} -> httpStatus) (\s@StopRunResponse' {} a -> s {httpStatus = a} :: StopRunResponse)

instance Core.NFData StopRunResponse
