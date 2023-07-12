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
-- Module      : Amazonka.DeviceFarm.StopJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a stop request for the current job. AWS Device Farm
-- immediately stops the job on the device where tests have not started.
-- You are not billed for this device. On the device where tests have
-- started, setup suite and teardown suite tests run to completion on the
-- device. You are billed for setup, teardown, and any tests that were in
-- progress or already completed.
module Amazonka.DeviceFarm.StopJob
  ( -- * Creating a Request
    StopJob (..),
    newStopJob,

    -- * Request Lenses
    stopJob_arn,

    -- * Destructuring the Response
    StopJobResponse (..),
    newStopJobResponse,

    -- * Response Lenses
    stopJobResponse_job,
    stopJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopJob' smart constructor.
data StopJob = StopJob'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm job to
    -- stop.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopJob_arn' - Represents the Amazon Resource Name (ARN) of the Device Farm job to
-- stop.
newStopJob ::
  -- | 'arn'
  Prelude.Text ->
  StopJob
newStopJob pArn_ = StopJob' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm job to
-- stop.
stopJob_arn :: Lens.Lens' StopJob Prelude.Text
stopJob_arn = Lens.lens (\StopJob' {arn} -> arn) (\s@StopJob' {} a -> s {arn = a} :: StopJob)

instance Core.AWSRequest StopJob where
  type AWSResponse StopJob = StopJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopJobResponse'
            Prelude.<$> (x Data..?> "job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopJob where
  hashWithSalt _salt StopJob' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData StopJob where
  rnf StopJob' {..} = Prelude.rnf arn

instance Data.ToHeaders StopJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.StopJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopJob where
  toJSON StopJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath StopJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopJobResponse' smart constructor.
data StopJobResponse = StopJobResponse'
  { -- | The job that was stopped.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'stopJobResponse_job' - The job that was stopped.
--
-- 'httpStatus', 'stopJobResponse_httpStatus' - The response's http status code.
newStopJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopJobResponse
newStopJobResponse pHttpStatus_ =
  StopJobResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job that was stopped.
stopJobResponse_job :: Lens.Lens' StopJobResponse (Prelude.Maybe Job)
stopJobResponse_job = Lens.lens (\StopJobResponse' {job} -> job) (\s@StopJobResponse' {} a -> s {job = a} :: StopJobResponse)

-- | The response's http status code.
stopJobResponse_httpStatus :: Lens.Lens' StopJobResponse Prelude.Int
stopJobResponse_httpStatus = Lens.lens (\StopJobResponse' {httpStatus} -> httpStatus) (\s@StopJobResponse' {} a -> s {httpStatus = a} :: StopJobResponse)

instance Prelude.NFData StopJobResponse where
  rnf StopJobResponse' {..} =
    Prelude.rnf job
      `Prelude.seq` Prelude.rnf httpStatus
