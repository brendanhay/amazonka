{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.DeleteTranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously submitted transcription job along with any other generated results such as the transcription, models, and so on.
module Network.AWS.Transcribe.DeleteTranscriptionJob
  ( -- * Creating a request
    DeleteTranscriptionJob (..),
    mkDeleteTranscriptionJob,

    -- ** Request lenses
    dtjTranscriptionJobName,

    -- * Destructuring the response
    DeleteTranscriptionJobResponse (..),
    mkDeleteTranscriptionJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkDeleteTranscriptionJob' smart constructor.
newtype DeleteTranscriptionJob = DeleteTranscriptionJob'
  { transcriptionJobName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTranscriptionJob' with the minimum fields required to make a request.
--
-- * 'transcriptionJobName' - The name of the transcription job to be deleted.
mkDeleteTranscriptionJob ::
  -- | 'transcriptionJobName'
  Lude.Text ->
  DeleteTranscriptionJob
mkDeleteTranscriptionJob pTranscriptionJobName_ =
  DeleteTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the transcription job to be deleted.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjTranscriptionJobName :: Lens.Lens' DeleteTranscriptionJob Lude.Text
dtjTranscriptionJobName = Lens.lens (transcriptionJobName :: DeleteTranscriptionJob -> Lude.Text) (\s a -> s {transcriptionJobName = a} :: DeleteTranscriptionJob)
{-# DEPRECATED dtjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

instance Lude.AWSRequest DeleteTranscriptionJob where
  type Rs DeleteTranscriptionJob = DeleteTranscriptionJobResponse
  request = Req.postJSON transcribeService
  response = Res.receiveNull DeleteTranscriptionJobResponse'

instance Lude.ToHeaders DeleteTranscriptionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.DeleteTranscriptionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTranscriptionJob where
  toJSON DeleteTranscriptionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TranscriptionJobName" Lude..= transcriptionJobName)]
      )

instance Lude.ToPath DeleteTranscriptionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTranscriptionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTranscriptionJobResponse' smart constructor.
data DeleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTranscriptionJobResponse' with the minimum fields required to make a request.
mkDeleteTranscriptionJobResponse ::
  DeleteTranscriptionJobResponse
mkDeleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'
