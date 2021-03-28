{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTranscriptionJob (..)
    , mkGetTranscriptionJob
    -- ** Request lenses
    , gtjTranscriptionJobName

    -- * Destructuring the response
    , GetTranscriptionJobResponse (..)
    , mkGetTranscriptionJobResponse
    -- ** Response lenses
    , gtjrrsTranscriptionJob
    , gtjrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkGetTranscriptionJob' smart constructor.
newtype GetTranscriptionJob = GetTranscriptionJob'
  { transcriptionJobName :: Types.TranscriptionJobName
    -- ^ The name of the job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTranscriptionJob' value with any optional fields omitted.
mkGetTranscriptionJob
    :: Types.TranscriptionJobName -- ^ 'transcriptionJobName'
    -> GetTranscriptionJob
mkGetTranscriptionJob transcriptionJobName
  = GetTranscriptionJob'{transcriptionJobName}

-- | The name of the job.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjTranscriptionJobName :: Lens.Lens' GetTranscriptionJob Types.TranscriptionJobName
gtjTranscriptionJobName = Lens.field @"transcriptionJobName"
{-# INLINEABLE gtjTranscriptionJobName #-}
{-# DEPRECATED transcriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead"  #-}

instance Core.ToQuery GetTranscriptionJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTranscriptionJob where
        toHeaders GetTranscriptionJob{..}
          = Core.pure ("X-Amz-Target", "Transcribe.GetTranscriptionJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetTranscriptionJob where
        toJSON GetTranscriptionJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TranscriptionJobName" Core..= transcriptionJobName)])

instance Core.AWSRequest GetTranscriptionJob where
        type Rs GetTranscriptionJob = GetTranscriptionJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTranscriptionJobResponse' Core.<$>
                   (x Core..:? "TranscriptionJob") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetTranscriptionJobResponse' smart constructor.
data GetTranscriptionJobResponse = GetTranscriptionJobResponse'
  { transcriptionJob :: Core.Maybe Types.TranscriptionJob
    -- ^ An object that contains the results of the transcription job.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTranscriptionJobResponse' value with any optional fields omitted.
mkGetTranscriptionJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTranscriptionJobResponse
mkGetTranscriptionJobResponse responseStatus
  = GetTranscriptionJobResponse'{transcriptionJob = Core.Nothing,
                                 responseStatus}

-- | An object that contains the results of the transcription job.
--
-- /Note:/ Consider using 'transcriptionJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjrrsTranscriptionJob :: Lens.Lens' GetTranscriptionJobResponse (Core.Maybe Types.TranscriptionJob)
gtjrrsTranscriptionJob = Lens.field @"transcriptionJob"
{-# INLINEABLE gtjrrsTranscriptionJob #-}
{-# DEPRECATED transcriptionJob "Use generic-lens or generic-optics with 'transcriptionJob' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtjrrsResponseStatus :: Lens.Lens' GetTranscriptionJobResponse Core.Int
gtjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
