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
-- Module      : Amazonka.Transcribe.GetMedicalTranscriptionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the specified medical transcription job.
--
-- To view the status of the specified medical transcription job, check the
-- @TranscriptionJobStatus@ field. If the status is @COMPLETED@, the job is
-- finished. You can find the results at the location specified in
-- @TranscriptFileUri@. If the status is @FAILED@, @FailureReason@ provides
-- details on why your transcription job failed.
--
-- To get a list of your medical transcription jobs, use the operation.
module Amazonka.Transcribe.GetMedicalTranscriptionJob
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newGetMedicalTranscriptionJob' smart constructor.
data GetMedicalTranscriptionJob = GetMedicalTranscriptionJob'
  { -- | The name of the medical transcription job you want information about.
    -- Job names are case sensitive.
    medicalTranscriptionJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMedicalTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medicalTranscriptionJobName', 'getMedicalTranscriptionJob_medicalTranscriptionJobName' - The name of the medical transcription job you want information about.
-- Job names are case sensitive.
newGetMedicalTranscriptionJob ::
  -- | 'medicalTranscriptionJobName'
  Prelude.Text ->
  GetMedicalTranscriptionJob
newGetMedicalTranscriptionJob
  pMedicalTranscriptionJobName_ =
    GetMedicalTranscriptionJob'
      { medicalTranscriptionJobName =
          pMedicalTranscriptionJobName_
      }

-- | The name of the medical transcription job you want information about.
-- Job names are case sensitive.
getMedicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' GetMedicalTranscriptionJob Prelude.Text
getMedicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\GetMedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@GetMedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: GetMedicalTranscriptionJob)

instance Core.AWSRequest GetMedicalTranscriptionJob where
  type
    AWSResponse GetMedicalTranscriptionJob =
      GetMedicalTranscriptionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMedicalTranscriptionJobResponse'
            Prelude.<$> (x Data..?> "MedicalTranscriptionJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMedicalTranscriptionJob where
  hashWithSalt _salt GetMedicalTranscriptionJob' {..} =
    _salt
      `Prelude.hashWithSalt` medicalTranscriptionJobName

instance Prelude.NFData GetMedicalTranscriptionJob where
  rnf GetMedicalTranscriptionJob' {..} =
    Prelude.rnf medicalTranscriptionJobName

instance Data.ToHeaders GetMedicalTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.GetMedicalTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMedicalTranscriptionJob where
  toJSON GetMedicalTranscriptionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MedicalTranscriptionJobName"
                  Data..= medicalTranscriptionJobName
              )
          ]
      )

instance Data.ToPath GetMedicalTranscriptionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMedicalTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMedicalTranscriptionJobResponse' smart constructor.
data GetMedicalTranscriptionJobResponse = GetMedicalTranscriptionJobResponse'
  { -- | Provides detailed information about the specified medical transcription
    -- job, including job status and, if applicable, failure reason.
    medicalTranscriptionJob :: Prelude.Maybe MedicalTranscriptionJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMedicalTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medicalTranscriptionJob', 'getMedicalTranscriptionJobResponse_medicalTranscriptionJob' - Provides detailed information about the specified medical transcription
-- job, including job status and, if applicable, failure reason.
--
-- 'httpStatus', 'getMedicalTranscriptionJobResponse_httpStatus' - The response's http status code.
newGetMedicalTranscriptionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMedicalTranscriptionJobResponse
newGetMedicalTranscriptionJobResponse pHttpStatus_ =
  GetMedicalTranscriptionJobResponse'
    { medicalTranscriptionJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides detailed information about the specified medical transcription
-- job, including job status and, if applicable, failure reason.
getMedicalTranscriptionJobResponse_medicalTranscriptionJob :: Lens.Lens' GetMedicalTranscriptionJobResponse (Prelude.Maybe MedicalTranscriptionJob)
getMedicalTranscriptionJobResponse_medicalTranscriptionJob = Lens.lens (\GetMedicalTranscriptionJobResponse' {medicalTranscriptionJob} -> medicalTranscriptionJob) (\s@GetMedicalTranscriptionJobResponse' {} a -> s {medicalTranscriptionJob = a} :: GetMedicalTranscriptionJobResponse)

-- | The response's http status code.
getMedicalTranscriptionJobResponse_httpStatus :: Lens.Lens' GetMedicalTranscriptionJobResponse Prelude.Int
getMedicalTranscriptionJobResponse_httpStatus = Lens.lens (\GetMedicalTranscriptionJobResponse' {httpStatus} -> httpStatus) (\s@GetMedicalTranscriptionJobResponse' {} a -> s {httpStatus = a} :: GetMedicalTranscriptionJobResponse)

instance
  Prelude.NFData
    GetMedicalTranscriptionJobResponse
  where
  rnf GetMedicalTranscriptionJobResponse' {..} =
    Prelude.rnf medicalTranscriptionJob
      `Prelude.seq` Prelude.rnf httpStatus
