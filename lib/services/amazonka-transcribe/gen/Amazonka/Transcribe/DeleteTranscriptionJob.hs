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
-- Module      : Amazonka.Transcribe.DeleteTranscriptionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transcription job. To use this operation, specify the name of
-- the job you want to delete using @TranscriptionJobName@. Job names are
-- case sensitive.
module Amazonka.Transcribe.DeleteTranscriptionJob
  ( -- * Creating a Request
    DeleteTranscriptionJob (..),
    newDeleteTranscriptionJob,

    -- * Request Lenses
    deleteTranscriptionJob_transcriptionJobName,

    -- * Destructuring the Response
    DeleteTranscriptionJobResponse (..),
    newDeleteTranscriptionJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDeleteTranscriptionJob' smart constructor.
data DeleteTranscriptionJob = DeleteTranscriptionJob'
  { -- | The name of the transcription job you want to delete. Job names are case
    -- sensitive.
    transcriptionJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJobName', 'deleteTranscriptionJob_transcriptionJobName' - The name of the transcription job you want to delete. Job names are case
-- sensitive.
newDeleteTranscriptionJob ::
  -- | 'transcriptionJobName'
  Prelude.Text ->
  DeleteTranscriptionJob
newDeleteTranscriptionJob pTranscriptionJobName_ =
  DeleteTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the transcription job you want to delete. Job names are case
-- sensitive.
deleteTranscriptionJob_transcriptionJobName :: Lens.Lens' DeleteTranscriptionJob Prelude.Text
deleteTranscriptionJob_transcriptionJobName = Lens.lens (\DeleteTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@DeleteTranscriptionJob' {} a -> s {transcriptionJobName = a} :: DeleteTranscriptionJob)

instance Core.AWSRequest DeleteTranscriptionJob where
  type
    AWSResponse DeleteTranscriptionJob =
      DeleteTranscriptionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteTranscriptionJobResponse'

instance Prelude.Hashable DeleteTranscriptionJob where
  hashWithSalt _salt DeleteTranscriptionJob' {..} =
    _salt `Prelude.hashWithSalt` transcriptionJobName

instance Prelude.NFData DeleteTranscriptionJob where
  rnf DeleteTranscriptionJob' {..} =
    Prelude.rnf transcriptionJobName

instance Core.ToHeaders DeleteTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteTranscriptionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteTranscriptionJob where
  toJSON DeleteTranscriptionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TranscriptionJobName"
                  Core..= transcriptionJobName
              )
          ]
      )

instance Core.ToPath DeleteTranscriptionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTranscriptionJobResponse' smart constructor.
data DeleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTranscriptionJobResponse ::
  DeleteTranscriptionJobResponse
newDeleteTranscriptionJobResponse =
  DeleteTranscriptionJobResponse'

instance
  Prelude.NFData
    DeleteTranscriptionJobResponse
  where
  rnf _ = ()
