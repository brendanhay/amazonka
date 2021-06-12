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
-- Module      : Network.AWS.Transcribe.GetTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transcription job. To see the status of the
-- job, check the @TranscriptionJobStatus@ field. If the status is
-- @COMPLETED@, the job is finished and you can find the results at the
-- location specified in the @TranscriptFileUri@ field. If you enable
-- content redaction, the redacted transcript appears in
-- @RedactedTranscriptFileUri@.
module Network.AWS.Transcribe.GetTranscriptionJob
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newGetTranscriptionJob' smart constructor.
data GetTranscriptionJob = GetTranscriptionJob'
  { -- | The name of the job.
    transcriptionJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJobName', 'getTranscriptionJob_transcriptionJobName' - The name of the job.
newGetTranscriptionJob ::
  -- | 'transcriptionJobName'
  Core.Text ->
  GetTranscriptionJob
newGetTranscriptionJob pTranscriptionJobName_ =
  GetTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the job.
getTranscriptionJob_transcriptionJobName :: Lens.Lens' GetTranscriptionJob Core.Text
getTranscriptionJob_transcriptionJobName = Lens.lens (\GetTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@GetTranscriptionJob' {} a -> s {transcriptionJobName = a} :: GetTranscriptionJob)

instance Core.AWSRequest GetTranscriptionJob where
  type
    AWSResponse GetTranscriptionJob =
      GetTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTranscriptionJobResponse'
            Core.<$> (x Core..?> "TranscriptionJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTranscriptionJob

instance Core.NFData GetTranscriptionJob

instance Core.ToHeaders GetTranscriptionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.GetTranscriptionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTranscriptionJob where
  toJSON GetTranscriptionJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "TranscriptionJobName"
                  Core..= transcriptionJobName
              )
          ]
      )

instance Core.ToPath GetTranscriptionJob where
  toPath = Core.const "/"

instance Core.ToQuery GetTranscriptionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTranscriptionJobResponse' smart constructor.
data GetTranscriptionJobResponse = GetTranscriptionJobResponse'
  { -- | An object that contains the results of the transcription job.
    transcriptionJob :: Core.Maybe TranscriptionJob,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJob', 'getTranscriptionJobResponse_transcriptionJob' - An object that contains the results of the transcription job.
--
-- 'httpStatus', 'getTranscriptionJobResponse_httpStatus' - The response's http status code.
newGetTranscriptionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTranscriptionJobResponse
newGetTranscriptionJobResponse pHttpStatus_ =
  GetTranscriptionJobResponse'
    { transcriptionJob =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the results of the transcription job.
getTranscriptionJobResponse_transcriptionJob :: Lens.Lens' GetTranscriptionJobResponse (Core.Maybe TranscriptionJob)
getTranscriptionJobResponse_transcriptionJob = Lens.lens (\GetTranscriptionJobResponse' {transcriptionJob} -> transcriptionJob) (\s@GetTranscriptionJobResponse' {} a -> s {transcriptionJob = a} :: GetTranscriptionJobResponse)

-- | The response's http status code.
getTranscriptionJobResponse_httpStatus :: Lens.Lens' GetTranscriptionJobResponse Core.Int
getTranscriptionJobResponse_httpStatus = Lens.lens (\GetTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@GetTranscriptionJobResponse' {} a -> s {httpStatus = a} :: GetTranscriptionJobResponse)

instance Core.NFData GetTranscriptionJobResponse
