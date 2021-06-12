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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteTranscriptionJob' smart constructor.
data DeleteTranscriptionJob = DeleteTranscriptionJob'
  { -- | The name of the transcription job to be deleted.
    transcriptionJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteTranscriptionJob
newDeleteTranscriptionJob pTranscriptionJobName_ =
  DeleteTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the transcription job to be deleted.
deleteTranscriptionJob_transcriptionJobName :: Lens.Lens' DeleteTranscriptionJob Core.Text
deleteTranscriptionJob_transcriptionJobName = Lens.lens (\DeleteTranscriptionJob' {transcriptionJobName} -> transcriptionJobName) (\s@DeleteTranscriptionJob' {} a -> s {transcriptionJobName = a} :: DeleteTranscriptionJob)

instance Core.AWSRequest DeleteTranscriptionJob where
  type
    AWSResponse DeleteTranscriptionJob =
      DeleteTranscriptionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteTranscriptionJobResponse'

instance Core.Hashable DeleteTranscriptionJob

instance Core.NFData DeleteTranscriptionJob

instance Core.ToHeaders DeleteTranscriptionJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteTranscriptionJob" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTranscriptionJob where
  toJSON DeleteTranscriptionJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "TranscriptionJobName"
                  Core..= transcriptionJobName
              )
          ]
      )

instance Core.ToPath DeleteTranscriptionJob where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTranscriptionJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteTranscriptionJobResponse' smart constructor.
data DeleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTranscriptionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTranscriptionJobResponse ::
  DeleteTranscriptionJobResponse
newDeleteTranscriptionJobResponse =
  DeleteTranscriptionJobResponse'

instance Core.NFData DeleteTranscriptionJobResponse
