{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkDeleteTranscriptionJob' smart constructor.
newtype DeleteTranscriptionJob = DeleteTranscriptionJob'
  { -- | The name of the transcription job to be deleted.
    transcriptionJobName :: Types.TranscriptionJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTranscriptionJob' value with any optional fields omitted.
mkDeleteTranscriptionJob ::
  -- | 'transcriptionJobName'
  Types.TranscriptionJobName ->
  DeleteTranscriptionJob
mkDeleteTranscriptionJob transcriptionJobName =
  DeleteTranscriptionJob' {transcriptionJobName}

-- | The name of the transcription job to be deleted.
--
-- /Note:/ Consider using 'transcriptionJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtjTranscriptionJobName :: Lens.Lens' DeleteTranscriptionJob Types.TranscriptionJobName
dtjTranscriptionJobName = Lens.field @"transcriptionJobName"
{-# DEPRECATED dtjTranscriptionJobName "Use generic-lens or generic-optics with 'transcriptionJobName' instead." #-}

instance Core.FromJSON DeleteTranscriptionJob where
  toJSON DeleteTranscriptionJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TranscriptionJobName" Core..= transcriptionJobName)]
      )

instance Core.AWSRequest DeleteTranscriptionJob where
  type Rs DeleteTranscriptionJob = DeleteTranscriptionJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Transcribe.DeleteTranscriptionJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteTranscriptionJobResponse'

-- | /See:/ 'mkDeleteTranscriptionJobResponse' smart constructor.
data DeleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTranscriptionJobResponse' value with any optional fields omitted.
mkDeleteTranscriptionJobResponse ::
  DeleteTranscriptionJobResponse
mkDeleteTranscriptionJobResponse = DeleteTranscriptionJobResponse'
