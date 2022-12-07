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
-- Module      : Amazonka.Transcribe.GetTranscriptionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the specified transcription job.
--
-- To view the status of the specified transcription job, check the
-- @TranscriptionJobStatus@ field. If the status is @COMPLETED@, the job is
-- finished and you can find the results at the location specified in
-- @TranscriptFileUri@. If the status is @FAILED@, @FailureReason@ provides
-- details on why your transcription job failed.
--
-- If you enabled content redaction, the redacted transcript can be found
-- at the location specified in @RedactedTranscriptFileUri@.
--
-- To get a list of your transcription jobs, use the operation.
module Amazonka.Transcribe.GetTranscriptionJob
  ( -- * Creating a Request
    GetTranscriptionJob (..),
    newGetTranscriptionJob,

    -- * Request Lenses
    getTranscriptionJob_transcriptionJobName,

    -- * Destructuring the Response
    GetTranscriptionJobResponse (..),
    newGetTranscriptionJobResponse,

    -- * Response Lenses
    getTranscriptionJobResponse_transcriptionJob,
    getTranscriptionJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newGetTranscriptionJob' smart constructor.
data GetTranscriptionJob = GetTranscriptionJob'
  { -- | The name of the transcription job you want information about. Job names
    -- are case sensitive.
    transcriptionJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJobName', 'getTranscriptionJob_transcriptionJobName' - The name of the transcription job you want information about. Job names
-- are case sensitive.
newGetTranscriptionJob ::
  -- | 'transcriptionJobName'
  Prelude.Text ->
  GetTranscriptionJob
newGetTranscriptionJob pTranscriptionJobName_ =
  GetTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the transcription job you want information about. Job names
-- are case sensitive.
getTranscriptionJob_transcriptionJobName :: Lens.Lens' GetTranscriptionJob Prelude.Text
getTranscriptionJob_transcriptionJobName = Lens.lens (\GetTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@GetTranscriptionJob' {} a -> s {transcriptionJobName = a} :: GetTranscriptionJob)

instance Core.AWSRequest GetTranscriptionJob where
  type
    AWSResponse GetTranscriptionJob =
      GetTranscriptionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTranscriptionJobResponse'
            Prelude.<$> (x Data..?> "TranscriptionJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTranscriptionJob where
  hashWithSalt _salt GetTranscriptionJob' {..} =
    _salt `Prelude.hashWithSalt` transcriptionJobName

instance Prelude.NFData GetTranscriptionJob where
  rnf GetTranscriptionJob' {..} =
    Prelude.rnf transcriptionJobName

instance Data.ToHeaders GetTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.GetTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTranscriptionJob where
  toJSON GetTranscriptionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TranscriptionJobName"
                  Data..= transcriptionJobName
              )
          ]
      )

instance Data.ToPath GetTranscriptionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTranscriptionJobResponse' smart constructor.
data GetTranscriptionJobResponse = GetTranscriptionJobResponse'
  { -- | Provides detailed information about the specified transcription job,
    -- including job status and, if applicable, failure reason.
    transcriptionJob :: Prelude.Maybe TranscriptionJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJob', 'getTranscriptionJobResponse_transcriptionJob' - Provides detailed information about the specified transcription job,
-- including job status and, if applicable, failure reason.
--
-- 'httpStatus', 'getTranscriptionJobResponse_httpStatus' - The response's http status code.
newGetTranscriptionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTranscriptionJobResponse
newGetTranscriptionJobResponse pHttpStatus_ =
  GetTranscriptionJobResponse'
    { transcriptionJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides detailed information about the specified transcription job,
-- including job status and, if applicable, failure reason.
getTranscriptionJobResponse_transcriptionJob :: Lens.Lens' GetTranscriptionJobResponse (Prelude.Maybe TranscriptionJob)
getTranscriptionJobResponse_transcriptionJob = Lens.lens (\GetTranscriptionJobResponse' {transcriptionJob} -> transcriptionJob) (\s@GetTranscriptionJobResponse' {} a -> s {transcriptionJob = a} :: GetTranscriptionJobResponse)

-- | The response's http status code.
getTranscriptionJobResponse_httpStatus :: Lens.Lens' GetTranscriptionJobResponse Prelude.Int
getTranscriptionJobResponse_httpStatus = Lens.lens (\GetTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@GetTranscriptionJobResponse' {} a -> s {httpStatus = a} :: GetTranscriptionJobResponse)

instance Prelude.NFData GetTranscriptionJobResponse where
  rnf GetTranscriptionJobResponse' {..} =
    Prelude.rnf transcriptionJob
      `Prelude.seq` Prelude.rnf httpStatus
