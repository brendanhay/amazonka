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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get run operation.
--
-- /See:/ 'newGetRun' smart constructor.
data GetRun = GetRun'
  { -- | The run\'s ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetRun
newGetRun pArn_ = GetRun' {arn = pArn_}

-- | The run\'s ARN.
getRun_arn :: Lens.Lens' GetRun Prelude.Text
getRun_arn = Lens.lens (\GetRun' {arn} -> arn) (\s@GetRun' {} a -> s {arn = a} :: GetRun)

instance Core.AWSRequest GetRun where
  type AWSResponse GetRun = GetRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRunResponse'
            Prelude.<$> (x Core..?> "run")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRun

instance Prelude.NFData GetRun

instance Core.ToHeaders GetRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.GetRun" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRun where
  toJSON GetRun' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )

instance Core.ToPath GetRun where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRun where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a get run request.
--
-- /See:/ 'newGetRunResponse' smart constructor.
data GetRunResponse = GetRunResponse'
  { -- | The run to get results from.
    run :: Prelude.Maybe Run,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRunResponse
newGetRunResponse pHttpStatus_ =
  GetRunResponse'
    { run = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The run to get results from.
getRunResponse_run :: Lens.Lens' GetRunResponse (Prelude.Maybe Run)
getRunResponse_run = Lens.lens (\GetRunResponse' {run} -> run) (\s@GetRunResponse' {} a -> s {run = a} :: GetRunResponse)

-- | The response's http status code.
getRunResponse_httpStatus :: Lens.Lens' GetRunResponse Prelude.Int
getRunResponse_httpStatus = Lens.lens (\GetRunResponse' {httpStatus} -> httpStatus) (\s@GetRunResponse' {} a -> s {httpStatus = a} :: GetRunResponse)

instance Prelude.NFData GetRunResponse
