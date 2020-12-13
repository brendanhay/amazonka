{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.GetTranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transcription job. To see the status of the job, check the @TranscriptionJobStatus@ field. If the status is @COMPLETED@ , the job is finished and you can find the results at the location specified in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ .
module Network.AWS.Transcribe.GetTranscriptionJob
  ( -- * Creating a request
    GetTranscriptionJob (..),
    mkGetTranscriptionJob,

    -- ** Request lenses
    gtjTranscriptionJobName,

    -- * Destructuring the response
    GetTranscriptionJobResponse (..),
    mkGetTranscriptionJobResponse,

    -- ** Response lenses
    gtjrsTranscriptionJob,
    gtjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkGetTranscriptionJob' smart constructor.
newtype GetTranscriptionJob = GetTranscriptionJob'
  { -- | The name of the job.
    transcriptionJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTranscriptionJob' with the minimum fields required to make a request.
--
-- * 'transcriptionJobName' - The name of the job.
mkGetTranscriptionJob ::
  -- | 'transcriptionJobName'
  Lude.Text ->
  GetTranscriptionJob
mkGetTranscriptionJob pTranscriptionJobName_ =
  GetTranscriptionJob'
    { transcriptionJobName =
        pTranscriptionJobName_
    }

-- | The name of the job.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjTranscriptionJobName :: Lens.Lens' GetTranscriptionJob Lude.Text
gtjTranscriptionJobName = Lens.lens (transcriptionJobName :: GetTranscriptionJob -> Lude.Text) (\s a -> s {transcriptionJobName = a} :: GetTranscriptionJob)
{-# DEPRECATED gtjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

instance Lude.AWSRequest GetTranscriptionJob where
  type Rs GetTranscriptionJob = GetTranscriptionJobResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTranscriptionJobResponse'
            Lude.<$> (x Lude..?> "TranscriptionJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTranscriptionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.GetTranscriptionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTranscriptionJob where
  toJSON GetTranscriptionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TranscriptionJobName" Lude..= transcriptionJobName)]
      )

instance Lude.ToPath GetTranscriptionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTranscriptionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTranscriptionJobResponse' smart constructor.
data GetTranscriptionJobResponse = GetTranscriptionJobResponse'
  { -- | An object that contains the results of the transcription job.
    transcriptionJob :: Lude.Maybe TranscriptionJob,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- * 'transcriptionJob' - An object that contains the results of the transcription job.
-- * 'responseStatus' - The response status code.
mkGetTranscriptionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTranscriptionJobResponse
mkGetTranscriptionJobResponse pResponseStatus_ =
  GetTranscriptionJobResponse'
    { transcriptionJob = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the results of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjrsTranscriptionJob :: Lens.Lens' GetTranscriptionJobResponse (Lude.Maybe TranscriptionJob)
gtjrsTranscriptionJob = Lens.lens (transcriptionJob :: GetTranscriptionJobResponse -> Lude.Maybe TranscriptionJob) (\s a -> s {transcriptionJob = a} :: GetTranscriptionJobResponse)
{-# DEPRECATED gtjrsTranscriptionJob "Use generic-lens or generic-optics with 'transcriptionJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjrsResponseStatus :: Lens.Lens' GetTranscriptionJobResponse Lude.Int
gtjrsResponseStatus = Lens.lens (responseStatus :: GetTranscriptionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTranscriptionJobResponse)
{-# DEPRECATED gtjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
