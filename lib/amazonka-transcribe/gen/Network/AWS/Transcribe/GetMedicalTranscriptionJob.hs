{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.GetMedicalTranscriptionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transcription job from Amazon Transcribe Medical. To see the status of the job, check the @TranscriptionJobStatus@ field. If the status is @COMPLETED@ , the job is finished. You find the results of the completed job in the @TranscriptFileUri@ field.
module Network.AWS.Transcribe.GetMedicalTranscriptionJob
  ( -- * Creating a request
    GetMedicalTranscriptionJob (..),
    mkGetMedicalTranscriptionJob,

    -- ** Request lenses
    gmtjMedicalTranscriptionJobName,

    -- * Destructuring the response
    GetMedicalTranscriptionJobResponse (..),
    mkGetMedicalTranscriptionJobResponse,

    -- ** Response lenses
    gmtjrsMedicalTranscriptionJob,
    gmtjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Transcribe.Types

-- | /See:/ 'mkGetMedicalTranscriptionJob' smart constructor.
newtype GetMedicalTranscriptionJob = GetMedicalTranscriptionJob'
  { -- | The name of the medical transcription job.
    medicalTranscriptionJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMedicalTranscriptionJob' with the minimum fields required to make a request.
--
-- * 'medicalTranscriptionJobName' - The name of the medical transcription job.
mkGetMedicalTranscriptionJob ::
  -- | 'medicalTranscriptionJobName'
  Lude.Text ->
  GetMedicalTranscriptionJob
mkGetMedicalTranscriptionJob pMedicalTranscriptionJobName_ =
  GetMedicalTranscriptionJob'
    { medicalTranscriptionJobName =
        pMedicalTranscriptionJobName_
    }

-- | The name of the medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtjMedicalTranscriptionJobName :: Lens.Lens' GetMedicalTranscriptionJob Lude.Text
gmtjMedicalTranscriptionJobName = Lens.lens (medicalTranscriptionJobName :: GetMedicalTranscriptionJob -> Lude.Text) (\s a -> s {medicalTranscriptionJobName = a} :: GetMedicalTranscriptionJob)
{-# DEPRECATED gmtjMedicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead." #-}

instance Lude.AWSRequest GetMedicalTranscriptionJob where
  type
    Rs GetMedicalTranscriptionJob =
      GetMedicalTranscriptionJobResponse
  request = Req.postJSON transcribeService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMedicalTranscriptionJobResponse'
            Lude.<$> (x Lude..?> "MedicalTranscriptionJob")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMedicalTranscriptionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Transcribe.GetMedicalTranscriptionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMedicalTranscriptionJob where
  toJSON GetMedicalTranscriptionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "MedicalTranscriptionJobName"
                  Lude..= medicalTranscriptionJobName
              )
          ]
      )

instance Lude.ToPath GetMedicalTranscriptionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMedicalTranscriptionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMedicalTranscriptionJobResponse' smart constructor.
data GetMedicalTranscriptionJobResponse = GetMedicalTranscriptionJobResponse'
  { -- | An object that contains the results of the medical transcription job.
    medicalTranscriptionJob :: Lude.Maybe MedicalTranscriptionJob,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMedicalTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- * 'medicalTranscriptionJob' - An object that contains the results of the medical transcription job.
-- * 'responseStatus' - The response status code.
mkGetMedicalTranscriptionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMedicalTranscriptionJobResponse
mkGetMedicalTranscriptionJobResponse pResponseStatus_ =
  GetMedicalTranscriptionJobResponse'
    { medicalTranscriptionJob =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the results of the medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtjrsMedicalTranscriptionJob :: Lens.Lens' GetMedicalTranscriptionJobResponse (Lude.Maybe MedicalTranscriptionJob)
gmtjrsMedicalTranscriptionJob = Lens.lens (medicalTranscriptionJob :: GetMedicalTranscriptionJobResponse -> Lude.Maybe MedicalTranscriptionJob) (\s a -> s {medicalTranscriptionJob = a} :: GetMedicalTranscriptionJobResponse)
{-# DEPRECATED gmtjrsMedicalTranscriptionJob "Use generic-lens or generic-optics with 'medicalTranscriptionJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtjrsResponseStatus :: Lens.Lens' GetMedicalTranscriptionJobResponse Lude.Int
gmtjrsResponseStatus = Lens.lens (responseStatus :: GetMedicalTranscriptionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMedicalTranscriptionJobResponse)
{-# DEPRECATED gmtjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
