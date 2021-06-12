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
-- Module      : Network.AWS.DeviceFarm.GetRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a run.
module Network.AWS.DeviceFarm.GetRun
  ( -- * Creating a Request
    GetRun (..),
    newGetRun,

    -- * Request Lenses
    getRun_arn,

    -- * Destructuring the Response
    GetRunResponse (..),
    newGetRunResponse,

    -- * Response Lenses
    getRunResponse_run,
    getRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get run operation.
--
-- /See:/ 'newGetRun' smart constructor.
data GetRun = GetRun'
  { -- | The run\'s ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRun_arn' - The run\'s ARN.
newGetRun ::
  -- | 'arn'
  Core.Text ->
  GetRun
newGetRun pArn_ = GetRun' {arn = pArn_}

-- | The run\'s ARN.
getRun_arn :: Lens.Lens' GetRun Core.Text
getRun_arn = Lens.lens (\GetRun' {arn} -> arn) (\s@GetRun' {} a -> s {arn = a} :: GetRun)

instance Core.AWSRequest GetRun where
  type AWSResponse GetRun = GetRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRunResponse'
            Core.<$> (x Core..?> "run")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRun

instance Core.NFData GetRun

instance Core.ToHeaders GetRun where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.GetRun" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRun where
  toJSON GetRun' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetRun where
  toPath = Core.const "/"

instance Core.ToQuery GetRun where
  toQuery = Core.const Core.mempty

-- | Represents the result of a get run request.
--
-- /See:/ 'newGetRunResponse' smart constructor.
data GetRunResponse = GetRunResponse'
  { -- | The run to get results from.
    run :: Core.Maybe Run,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'run', 'getRunResponse_run' - The run to get results from.
--
-- 'httpStatus', 'getRunResponse_httpStatus' - The response's http status code.
newGetRunResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRunResponse
newGetRunResponse pHttpStatus_ =
  GetRunResponse'
    { run = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The run to get results from.
getRunResponse_run :: Lens.Lens' GetRunResponse (Core.Maybe Run)
getRunResponse_run = Lens.lens (\GetRunResponse' {run} -> run) (\s@GetRunResponse' {} a -> s {run = a} :: GetRunResponse)

-- | The response's http status code.
getRunResponse_httpStatus :: Lens.Lens' GetRunResponse Core.Int
getRunResponse_httpStatus = Lens.lens (\GetRunResponse' {httpStatus} -> httpStatus) (\s@GetRunResponse' {} a -> s {httpStatus = a} :: GetRunResponse)

instance Core.NFData GetRunResponse
