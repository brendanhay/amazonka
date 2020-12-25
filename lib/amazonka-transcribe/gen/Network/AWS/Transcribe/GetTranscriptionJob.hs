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
    gtjrrsTranscriptionJob,
    gtjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkGetTranscriptionJob' smart constructor.
newtype GetTranscriptionJob = GetTranscriptionJob'
  { -- | The name of the job.
    transcriptionJobName :: Types.TranscriptionJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTranscriptionJob' value with any optional fields omitted.
mkGetTranscriptionJob ::
  -- | 'transcriptionJobName'
  Types.TranscriptionJobName ->
  GetTranscriptionJob
mkGetTranscriptionJob transcriptionJobName =
  GetTranscriptionJob' {transcriptionJobName}

-- | The name of the job.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjTranscriptionJobName :: Lens.Lens' GetTranscriptionJob Types.TranscriptionJobName
gtjTranscriptionJobName = Lens.field @"transcriptionJobName"
{-# DEPRECATED gtjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

instance Core.FromJSON GetTranscriptionJob where
  toJSON GetTranscriptionJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TranscriptionJobName" Core..= transcriptionJobName)]
      )

instance Core.AWSRequest GetTranscriptionJob where
  type Rs GetTranscriptionJob = GetTranscriptionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.GetTranscriptionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTranscriptionJobResponse'
            Core.<$> (x Core..:? "TranscriptionJob")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTranscriptionJobResponse' smart constructor.
data GetTranscriptionJobResponse = GetTranscriptionJobResponse'
  { -- | An object that contains the results of the transcription job.
    transcriptionJob :: Core.Maybe Types.TranscriptionJob,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTranscriptionJobResponse' value with any optional fields omitted.
mkGetTranscriptionJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTranscriptionJobResponse
mkGetTranscriptionJobResponse responseStatus =
  GetTranscriptionJobResponse'
    { transcriptionJob = Core.Nothing,
      responseStatus
    }

-- | An object that contains the results of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjrrsTranscriptionJob :: Lens.Lens' GetTranscriptionJobResponse (Core.Maybe Types.TranscriptionJob)
gtjrrsTranscriptionJob = Lens.field @"transcriptionJob"
{-# DEPRECATED gtjrrsTranscriptionJob "Use generic-lens or generic-optics with 'transcriptionJob' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjrrsResponseStatus :: Lens.Lens' GetTranscriptionJobResponse Core.Int
gtjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
