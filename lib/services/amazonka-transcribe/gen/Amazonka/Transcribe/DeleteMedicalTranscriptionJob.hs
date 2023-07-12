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
-- Module      : Amazonka.Transcribe.DeleteMedicalTranscriptionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a medical transcription job. To use this operation, specify the
-- name of the job you want to delete using @MedicalTranscriptionJobName@.
-- Job names are case sensitive.
module Amazonka.Transcribe.DeleteMedicalTranscriptionJob
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDeleteMedicalTranscriptionJob' smart constructor.
data DeleteMedicalTranscriptionJob = DeleteMedicalTranscriptionJob'
  { -- | The name of the medical transcription job you want to delete. Job names
    -- are case sensitive.
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
-- 'medicalTranscriptionJobName', 'deleteMedicalTranscriptionJob_medicalTranscriptionJobName' - The name of the medical transcription job you want to delete. Job names
-- are case sensitive.
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

-- | The name of the medical transcription job you want to delete. Job names
-- are case sensitive.
deleteMedicalTranscriptionJob_medicalTranscriptionJobName :: Lens.Lens' DeleteMedicalTranscriptionJob Prelude.Text
deleteMedicalTranscriptionJob_medicalTranscriptionJobName = Lens.lens (\DeleteMedicalTranscriptionJob' {medicalTranscriptionJobName} -> medicalTranscriptionJobName) (\s@DeleteMedicalTranscriptionJob' {} a -> s {medicalTranscriptionJobName = a} :: DeleteMedicalTranscriptionJob)

instance
  Core.AWSRequest
    DeleteMedicalTranscriptionJob
  where
  type
    AWSResponse DeleteMedicalTranscriptionJob =
      DeleteMedicalTranscriptionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteMedicalTranscriptionJobResponse'

instance
  Prelude.Hashable
    DeleteMedicalTranscriptionJob
  where
  hashWithSalt _salt DeleteMedicalTranscriptionJob' {..} =
    _salt
      `Prelude.hashWithSalt` medicalTranscriptionJobName

instance Prelude.NFData DeleteMedicalTranscriptionJob where
  rnf DeleteMedicalTranscriptionJob' {..} =
    Prelude.rnf medicalTranscriptionJobName

instance Data.ToHeaders DeleteMedicalTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.DeleteMedicalTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMedicalTranscriptionJob where
  toJSON DeleteMedicalTranscriptionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "MedicalTranscriptionJobName"
                  Data..= medicalTranscriptionJobName
              )
          ]
      )

instance Data.ToPath DeleteMedicalTranscriptionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMedicalTranscriptionJob where
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
  where
  rnf _ = ()
