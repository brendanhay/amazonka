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
    gmtjrrsMedicalTranscriptionJob,
    gmtjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkGetMedicalTranscriptionJob' smart constructor.
newtype GetMedicalTranscriptionJob = GetMedicalTranscriptionJob'
  { -- | The name of the medical transcription job.
    medicalTranscriptionJobName :: Types.MedicalTranscriptionJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMedicalTranscriptionJob' value with any optional fields omitted.
mkGetMedicalTranscriptionJob ::
  -- | 'medicalTranscriptionJobName'
  Types.MedicalTranscriptionJobName ->
  GetMedicalTranscriptionJob
mkGetMedicalTranscriptionJob medicalTranscriptionJobName =
  GetMedicalTranscriptionJob' {medicalTranscriptionJobName}

-- | The name of the medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtjMedicalTranscriptionJobName :: Lens.Lens' GetMedicalTranscriptionJob Types.MedicalTranscriptionJobName
gmtjMedicalTranscriptionJobName = Lens.field @"medicalTranscriptionJobName"
{-# DEPRECATED gmtjMedicalTranscriptionJobName "Use generic-lens or generic-optics with 'medicalTranscriptionJobName' instead." #-}

instance Core.FromJSON GetMedicalTranscriptionJob where
  toJSON GetMedicalTranscriptionJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "MedicalTranscriptionJobName"
                  Core..= medicalTranscriptionJobName
              )
          ]
      )

instance Core.AWSRequest GetMedicalTranscriptionJob where
  type
    Rs GetMedicalTranscriptionJob =
      GetMedicalTranscriptionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.GetMedicalTranscriptionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMedicalTranscriptionJobResponse'
            Core.<$> (x Core..:? "MedicalTranscriptionJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMedicalTranscriptionJobResponse' smart constructor.
data GetMedicalTranscriptionJobResponse = GetMedicalTranscriptionJobResponse'
  { -- | An object that contains the results of the medical transcription job.
    medicalTranscriptionJob :: Core.Maybe Types.MedicalTranscriptionJob,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMedicalTranscriptionJobResponse' value with any optional fields omitted.
mkGetMedicalTranscriptionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMedicalTranscriptionJobResponse
mkGetMedicalTranscriptionJobResponse responseStatus =
  GetMedicalTranscriptionJobResponse'
    { medicalTranscriptionJob =
        Core.Nothing,
      responseStatus
    }

-- | An object that contains the results of the medical transcription job.
--
-- /Note:/ Consider using 'medicalTranscriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtjrrsMedicalTranscriptionJob :: Lens.Lens' GetMedicalTranscriptionJobResponse (Core.Maybe Types.MedicalTranscriptionJob)
gmtjrrsMedicalTranscriptionJob = Lens.field @"medicalTranscriptionJob"
{-# DEPRECATED gmtjrrsMedicalTranscriptionJob "Use generic-lens or generic-optics with 'medicalTranscriptionJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtjrrsResponseStatus :: Lens.Lens' GetMedicalTranscriptionJobResponse Core.Int
gmtjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmtjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
