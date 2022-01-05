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
-- Module      : Amazonka.Transcribe.GetCallAnalyticsJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a call analytics job. To see the status of the
-- job, check the @CallAnalyticsJobStatus@ field. If the status is
-- @COMPLETED@, the job is finished and you can find the results at the
-- location specified in the @TranscriptFileUri@ field. If you enable
-- personally identifiable information (PII) redaction, the redacted
-- transcript appears in the @RedactedTranscriptFileUri@ field.
module Amazonka.Transcribe.GetCallAnalyticsJob
  ( -- * Creating a Request
    GetCallAnalyticsJob (..),
    newGetCallAnalyticsJob,

    -- * Request Lenses
    getCallAnalyticsJob_callAnalyticsJobName,

    -- * Destructuring the Response
    GetCallAnalyticsJobResponse (..),
    newGetCallAnalyticsJobResponse,

    -- * Response Lenses
    getCallAnalyticsJobResponse_callAnalyticsJob,
    getCallAnalyticsJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newGetCallAnalyticsJob' smart constructor.
data GetCallAnalyticsJob = GetCallAnalyticsJob'
  { -- | The name of the analytics job you want information about. This value is
    -- case sensitive.
    callAnalyticsJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCallAnalyticsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAnalyticsJobName', 'getCallAnalyticsJob_callAnalyticsJobName' - The name of the analytics job you want information about. This value is
-- case sensitive.
newGetCallAnalyticsJob ::
  -- | 'callAnalyticsJobName'
  Prelude.Text ->
  GetCallAnalyticsJob
newGetCallAnalyticsJob pCallAnalyticsJobName_ =
  GetCallAnalyticsJob'
    { callAnalyticsJobName =
        pCallAnalyticsJobName_
    }

-- | The name of the analytics job you want information about. This value is
-- case sensitive.
getCallAnalyticsJob_callAnalyticsJobName :: Lens.Lens' GetCallAnalyticsJob Prelude.Text
getCallAnalyticsJob_callAnalyticsJobName = Lens.lens (\GetCallAnalyticsJob' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@GetCallAnalyticsJob' {} a -> s {callAnalyticsJobName = a} :: GetCallAnalyticsJob)

instance Core.AWSRequest GetCallAnalyticsJob where
  type
    AWSResponse GetCallAnalyticsJob =
      GetCallAnalyticsJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCallAnalyticsJobResponse'
            Prelude.<$> (x Core..?> "CallAnalyticsJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCallAnalyticsJob where
  hashWithSalt _salt GetCallAnalyticsJob' {..} =
    _salt `Prelude.hashWithSalt` callAnalyticsJobName

instance Prelude.NFData GetCallAnalyticsJob where
  rnf GetCallAnalyticsJob' {..} =
    Prelude.rnf callAnalyticsJobName

instance Core.ToHeaders GetCallAnalyticsJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.GetCallAnalyticsJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCallAnalyticsJob where
  toJSON GetCallAnalyticsJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CallAnalyticsJobName"
                  Core..= callAnalyticsJobName
              )
          ]
      )

instance Core.ToPath GetCallAnalyticsJob where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCallAnalyticsJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCallAnalyticsJobResponse' smart constructor.
data GetCallAnalyticsJobResponse = GetCallAnalyticsJobResponse'
  { -- | An object that contains the results of your call analytics job.
    callAnalyticsJob :: Prelude.Maybe CallAnalyticsJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCallAnalyticsJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAnalyticsJob', 'getCallAnalyticsJobResponse_callAnalyticsJob' - An object that contains the results of your call analytics job.
--
-- 'httpStatus', 'getCallAnalyticsJobResponse_httpStatus' - The response's http status code.
newGetCallAnalyticsJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCallAnalyticsJobResponse
newGetCallAnalyticsJobResponse pHttpStatus_ =
  GetCallAnalyticsJobResponse'
    { callAnalyticsJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the results of your call analytics job.
getCallAnalyticsJobResponse_callAnalyticsJob :: Lens.Lens' GetCallAnalyticsJobResponse (Prelude.Maybe CallAnalyticsJob)
getCallAnalyticsJobResponse_callAnalyticsJob = Lens.lens (\GetCallAnalyticsJobResponse' {callAnalyticsJob} -> callAnalyticsJob) (\s@GetCallAnalyticsJobResponse' {} a -> s {callAnalyticsJob = a} :: GetCallAnalyticsJobResponse)

-- | The response's http status code.
getCallAnalyticsJobResponse_httpStatus :: Lens.Lens' GetCallAnalyticsJobResponse Prelude.Int
getCallAnalyticsJobResponse_httpStatus = Lens.lens (\GetCallAnalyticsJobResponse' {httpStatus} -> httpStatus) (\s@GetCallAnalyticsJobResponse' {} a -> s {httpStatus = a} :: GetCallAnalyticsJobResponse)

instance Prelude.NFData GetCallAnalyticsJobResponse where
  rnf GetCallAnalyticsJobResponse' {..} =
    Prelude.rnf callAnalyticsJob
      `Prelude.seq` Prelude.rnf httpStatus
