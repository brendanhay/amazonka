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
-- Module      : Amazonka.Braket.CancelJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an Amazon Braket job.
module Amazonka.Braket.CancelJob
  ( -- * Creating a Request
    CancelJob (..),
    newCancelJob,

    -- * Request Lenses
    cancelJob_jobArn,

    -- * Destructuring the Response
    CancelJobResponse (..),
    newCancelJobResponse,

    -- * Response Lenses
    cancelJobResponse_httpStatus,
    cancelJobResponse_cancellationStatus,
    cancelJobResponse_jobArn,
  )
where

import Amazonka.Braket.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelJob' smart constructor.
data CancelJob = CancelJob'
  { -- | The ARN of the Amazon Braket job to cancel.
    jobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobArn', 'cancelJob_jobArn' - The ARN of the Amazon Braket job to cancel.
newCancelJob ::
  -- | 'jobArn'
  Prelude.Text ->
  CancelJob
newCancelJob pJobArn_ = CancelJob' {jobArn = pJobArn_}

-- | The ARN of the Amazon Braket job to cancel.
cancelJob_jobArn :: Lens.Lens' CancelJob Prelude.Text
cancelJob_jobArn = Lens.lens (\CancelJob' {jobArn} -> jobArn) (\s@CancelJob' {} a -> s {jobArn = a} :: CancelJob)

instance Core.AWSRequest CancelJob where
  type AWSResponse CancelJob = CancelJobResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "cancellationStatus")
            Prelude.<*> (x Core..:> "jobArn")
      )

instance Prelude.Hashable CancelJob where
  hashWithSalt _salt CancelJob' {..} =
    _salt `Prelude.hashWithSalt` jobArn

instance Prelude.NFData CancelJob where
  rnf CancelJob' {..} = Prelude.rnf jobArn

instance Core.ToHeaders CancelJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelJob where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CancelJob where
  toPath CancelJob' {..} =
    Prelude.mconcat
      ["/job/", Core.toBS jobArn, "/cancel"]

instance Core.ToQuery CancelJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the job cancellation request.
    cancellationStatus :: CancellationStatus,
    -- | The ARN of the Amazon Braket job.
    jobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelJobResponse_httpStatus' - The response's http status code.
--
-- 'cancellationStatus', 'cancelJobResponse_cancellationStatus' - The status of the job cancellation request.
--
-- 'jobArn', 'cancelJobResponse_jobArn' - The ARN of the Amazon Braket job.
newCancelJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'cancellationStatus'
  CancellationStatus ->
  -- | 'jobArn'
  Prelude.Text ->
  CancelJobResponse
newCancelJobResponse
  pHttpStatus_
  pCancellationStatus_
  pJobArn_ =
    CancelJobResponse'
      { httpStatus = pHttpStatus_,
        cancellationStatus = pCancellationStatus_,
        jobArn = pJobArn_
      }

-- | The response's http status code.
cancelJobResponse_httpStatus :: Lens.Lens' CancelJobResponse Prelude.Int
cancelJobResponse_httpStatus = Lens.lens (\CancelJobResponse' {httpStatus} -> httpStatus) (\s@CancelJobResponse' {} a -> s {httpStatus = a} :: CancelJobResponse)

-- | The status of the job cancellation request.
cancelJobResponse_cancellationStatus :: Lens.Lens' CancelJobResponse CancellationStatus
cancelJobResponse_cancellationStatus = Lens.lens (\CancelJobResponse' {cancellationStatus} -> cancellationStatus) (\s@CancelJobResponse' {} a -> s {cancellationStatus = a} :: CancelJobResponse)

-- | The ARN of the Amazon Braket job.
cancelJobResponse_jobArn :: Lens.Lens' CancelJobResponse Prelude.Text
cancelJobResponse_jobArn = Lens.lens (\CancelJobResponse' {jobArn} -> jobArn) (\s@CancelJobResponse' {} a -> s {jobArn = a} :: CancelJobResponse)

instance Prelude.NFData CancelJobResponse where
  rnf CancelJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf cancellationStatus
      `Prelude.seq` Prelude.rnf jobArn
