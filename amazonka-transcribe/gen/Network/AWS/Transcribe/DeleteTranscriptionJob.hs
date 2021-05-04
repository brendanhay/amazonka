{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Transcribe.DeleteTranscriptionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously submitted transcription job along with any other
-- generated results such as the transcription, models, and so on.
module Network.AWS.Transcribe.DeleteTranscriptionJob
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteTranscriptionJob' smart constructor.
data DeleteTranscriptionJob = DeleteTranscriptionJob'
  { -- | The name of the transcription job to be deleted.
    transcriptionJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTranscriptionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transcriptionJobName', 'deleteTranscriptionJob_transcriptionJobName' - The name of the transcription job to be deleted.
newDeleteTranscriptionJob ::
  -- | 'transcriptionJobName'
  Prelude.Text ->
  DeleteTranscriptionJob
newDeleteTranscriptionJob pTranscriptionJobName_ =
  DeleteTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the transcription job to be deleted.
deleteTranscriptionJob_transcriptionJobName :: Lens.Lens' DeleteTranscriptionJob Prelude.Text
deleteTranscriptionJob_transcriptionJobName = Lens.lens (\DeleteTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@DeleteTranscriptionJob' {} a -> s {transcriptionJobName = a} :: DeleteTranscriptionJob)

instance Prelude.AWSRequest DeleteTranscriptionJob where
  type
    Rs DeleteTranscriptionJob =
      DeleteTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteTranscriptionJobResponse'

instance Prelude.Hashable DeleteTranscriptionJob

instance Prelude.NFData DeleteTranscriptionJob

instance Prelude.ToHeaders DeleteTranscriptionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Transcribe.DeleteTranscriptionJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteTranscriptionJob where
  toJSON DeleteTranscriptionJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TranscriptionJobName"
                  Prelude..= transcriptionJobName
              )
          ]
      )

instance Prelude.ToPath DeleteTranscriptionJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTranscriptionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTranscriptionJobResponse' smart constructor.
data DeleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
