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
-- Module      : Network.AWS.Transcribe.GetMedicalTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transcription job from Amazon Transcribe
-- Medical. To see the status of the job, check the
-- @TranscriptionJobStatus@ field. If the status is @COMPLETED@, the job is
-- finished. You find the results of the completed job in the
-- @TranscriptFileUri@ field.
module Network.AWS.Transcribe.GetMedicalTranscriptionJob
  ( -- * Creating a Request
    GetMedicalTranscriptionJob (..),
    newGetMedicalTranscriptionJob,

    -- * Request Lenses
    getMedicalTranscriptionJob_medicalTranscriptionJobName,

    -- * Destructuring the Response
    GetMedicalTranscriptionJobResponse (..),
    newGetMedicalTranscriptionJobResponse,

    -- * Response Lenses
    getMedicalTranscriptionJobResponse_medicalTranscriptionJob,
    getMedicalTranscriptionJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newGetMedicalTranscriptionJob' smart constructor.
data GetMedicalTranscriptionJob = GetMedicalTranscriptionJob'
  { -- | The name of the medical transcription job.
    medicalTranscriptionJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMedicalTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medicalTranscriptionJobName', 'getMedicalTranscriptionJob_medicalTranscriptionJobName' - The name of the medical transcription job.
newGetMedicalTranscriptionJob ::
  -- | 'medicalTranscriptionJobName'
  Core.Text ->
  GetMedicalTranscriptionJob
newGetMedicalTranscriptionJob
  pMedicalTranscriptionJobName_ =
    GetMedicalTranscriptionJob'
      { medicalTranscriptionJobName =
          pMedicalTranscriptionJobName_
      }

-- | The name of the medical transcription job.
getMedicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' GetMedicalTranscriptionJob Core.Text
getMedicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\GetMedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@GetMedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: GetMedicalTranscriptionJob)

instance Core.AWSRequest GetMedicalTranscriptionJob where
  type
    AWSResponse GetMedicalTranscriptionJob =
      GetMedicalTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMedicalTranscriptionJobResponse'
            Core.<$> (x Core..?> "MedicalTranscriptionJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetMedicalTranscriptionJob

instance Core.NFData GetMedicalTranscriptionJob

instance Core.ToHeaders GetMedicalTranscriptionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.GetMedicalTranscriptionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMedicalTranscriptionJob where
  toJSON GetMedicalTranscriptionJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "MedicalTranscriptionJobName"
                  Core..= medicalTranscriptionJobName
              )
          ]
      )

instance Core.ToPath GetMedicalTranscriptionJob where
  toPath = Core.const "/"

instance Core.ToQuery GetMedicalTranscriptionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMedicalTranscriptionJobResponse' smart constructor.
data GetMedicalTranscriptionJobResponse = GetMedicalTranscriptionJobResponse'
  { -- | An object that contains the results of the medical transcription job.
    medicalTranscriptionJob :: Core.Maybe MedicalTranscriptionJob,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMedicalTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medicalTranscriptionJob', 'getMedicalTranscriptionJobResponse_medicalTranscriptionJob' - An object that contains the results of the medical transcription job.
--
-- 'httpStatus', 'getMedicalTranscriptionJobResponse_httpStatus' - The response's http status code.
newGetMedicalTranscriptionJobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetMedicalTranscriptionJobResponse
newGetMedicalTranscriptionJobResponse pHttpStatus_ =
  GetMedicalTranscriptionJobResponse'
    { medicalTranscriptionJob =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the results of the medical transcription job.
getMedicalTranscriptionJobResponse_medicalTranscriptionJob :: Lens.Lens' GetMedicalTranscriptionJobResponse (Core.Maybe MedicalTranscriptionJob)
getMedicalTranscriptionJobResponse_medicalTranscriptionJob = Lens.lens (\GetMedicalTranscriptionJobResponse' {medicalTranscriptionJob} -> medicalTranscriptionJob) (\s@GetMedicalTranscriptionJobResponse' {} a -> s {medicalTranscriptionJob = a} :: GetMedicalTranscriptionJobResponse)

-- | The response's http status code.
getMedicalTranscriptionJobResponse_httpStatus :: Lens.Lens' GetMedicalTranscriptionJobResponse Core.Int
getMedicalTranscriptionJobResponse_httpStatus = Lens.lens (\GetMedicalTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@GetMedicalTranscriptionJobResponse' {} a -> s {httpStatus = a} :: GetMedicalTranscriptionJobResponse)

instance
  Core.NFData
    GetMedicalTranscriptionJobResponse
