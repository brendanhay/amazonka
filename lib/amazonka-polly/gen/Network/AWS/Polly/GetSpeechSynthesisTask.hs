{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.GetSpeechSynthesisTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specific SpeechSynthesisTask object based on its TaskID. This object contains information about the given speech synthesis task, including the status of the task, and a link to the S3 bucket containing the output of the task.
module Network.AWS.Polly.GetSpeechSynthesisTask
    (
    -- * Creating a request
      GetSpeechSynthesisTask (..)
    , mkGetSpeechSynthesisTask
    -- ** Request lenses
    , gsstTaskId

    -- * Destructuring the response
    , GetSpeechSynthesisTaskResponse (..)
    , mkGetSpeechSynthesisTaskResponse
    -- ** Response lenses
    , gsstrrsSynthesisTask
    , gsstrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSpeechSynthesisTask' smart constructor.
newtype GetSpeechSynthesisTask = GetSpeechSynthesisTask'
  { taskId :: Types.TaskId
    -- ^ The Amazon Polly generated identifier for a speech synthesis task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSpeechSynthesisTask' value with any optional fields omitted.
mkGetSpeechSynthesisTask
    :: Types.TaskId -- ^ 'taskId'
    -> GetSpeechSynthesisTask
mkGetSpeechSynthesisTask taskId = GetSpeechSynthesisTask'{taskId}

-- | The Amazon Polly generated identifier for a speech synthesis task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsstTaskId :: Lens.Lens' GetSpeechSynthesisTask Types.TaskId
gsstTaskId = Lens.field @"taskId"
{-# INLINEABLE gsstTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

instance Core.ToQuery GetSpeechSynthesisTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSpeechSynthesisTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetSpeechSynthesisTask where
        type Rs GetSpeechSynthesisTask = GetSpeechSynthesisTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/v1/synthesisTasks/" Core.<> Core.toText taskId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSpeechSynthesisTaskResponse' Core.<$>
                   (x Core..:? "SynthesisTask") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSpeechSynthesisTaskResponse' smart constructor.
data GetSpeechSynthesisTaskResponse = GetSpeechSynthesisTaskResponse'
  { synthesisTask :: Core.Maybe Types.SynthesisTask
    -- ^ SynthesisTask object that provides information from the requested task, including output format, creation time, task status, and so on.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSpeechSynthesisTaskResponse' value with any optional fields omitted.
mkGetSpeechSynthesisTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSpeechSynthesisTaskResponse
mkGetSpeechSynthesisTaskResponse responseStatus
  = GetSpeechSynthesisTaskResponse'{synthesisTask = Core.Nothing,
                                    responseStatus}

-- | SynthesisTask object that provides information from the requested task, including output format, creation time, task status, and so on.
--
-- /Note:/ Consider using 'synthesisTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsstrrsSynthesisTask :: Lens.Lens' GetSpeechSynthesisTaskResponse (Core.Maybe Types.SynthesisTask)
gsstrrsSynthesisTask = Lens.field @"synthesisTask"
{-# INLINEABLE gsstrrsSynthesisTask #-}
{-# DEPRECATED synthesisTask "Use generic-lens or generic-optics with 'synthesisTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsstrrsResponseStatus :: Lens.Lens' GetSpeechSynthesisTaskResponse Core.Int
gsstrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsstrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
