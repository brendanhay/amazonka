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
-- Module      : Network.AWS.Transcribe.DeleteMedicalTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transcription job generated by Amazon Transcribe Medical and
-- any related information.
module Network.AWS.Transcribe.DeleteMedicalTranscriptionJob
  ( -- * Creating a Request
    DeleteMedicalTranscriptionJob (..),
    newDeleteMedicalTranscriptionJob,

    -- * Request Lenses
    deleteMedicalTranscriptionJob_medicalTranscriptionJobName,

    -- * Destructuring the Response
    DeleteMedicalTranscriptionJobResponse (..),
    newDeleteMedicalTranscriptionJobResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteMedicalTranscriptionJob' smart constructor.
data DeleteMedicalTranscriptionJob = DeleteMedicalTranscriptionJob'
  { -- | The name you provide to the @DeleteMedicalTranscriptionJob@ object to
    -- delete a transcription job.
    medicalTranscriptionJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMedicalTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'medicalTranscriptionJobName', 'deleteMedicalTranscriptionJob_medicalTranscriptionJobName' - The name you provide to the @DeleteMedicalTranscriptionJob@ object to
-- delete a transcription job.
newDeleteMedicalTranscriptionJob ::
  -- | 'medicalTranscriptionJobName'
  Prelude.Text ->
  DeleteMedicalTranscriptionJob
newDeleteMedicalTranscriptionJob
  pMedicalTranscriptionJobName_ =
    DeleteMedicalTranscriptionJob'
      { medicalTranscriptionJobName =
          pMedicalTranscriptionJobName_
      }

-- | The name you provide to the @DeleteMedicalTranscriptionJob@ object to
-- delete a transcription job.
deleteMedicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' DeleteMedicalTranscriptionJob Prelude.Text
deleteMedicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\DeleteMedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@DeleteMedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: DeleteMedicalTranscriptionJob)

instance
  Core.AWSRequest
    DeleteMedicalTranscriptionJob
  where
  type
    AWSResponse DeleteMedicalTranscriptionJob =
      DeleteMedicalTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteMedicalTranscriptionJobResponse'

instance
  Prelude.Hashable
    DeleteMedicalTranscriptionJob

instance Prelude.NFData DeleteMedicalTranscriptionJob

instance Core.ToHeaders DeleteMedicalTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteMedicalTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteMedicalTranscriptionJob where
  toJSON DeleteMedicalTranscriptionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MedicalTranscriptionJobName"
                  Core..= medicalTranscriptionJobName
              )
          ]
      )

instance Core.ToPath DeleteMedicalTranscriptionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteMedicalTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMedicalTranscriptionJobResponse' smart constructor.
data DeleteMedicalTranscriptionJobResponse = DeleteMedicalTranscriptionJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMedicalTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMedicalTranscriptionJobResponse ::
  DeleteMedicalTranscriptionJobResponse
newDeleteMedicalTranscriptionJobResponse =
  DeleteMedicalTranscriptionJobResponse'

instance
  Prelude.NFData
    DeleteMedicalTranscriptionJobResponse
