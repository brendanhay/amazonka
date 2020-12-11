{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetSpeechSynthesisTask (..),
    mkGetSpeechSynthesisTask,

    -- ** Request lenses
    gsstTaskId,

    -- * Destructuring the response
    GetSpeechSynthesisTaskResponse (..),
    mkGetSpeechSynthesisTaskResponse,

    -- ** Response lenses
    gsstrsSynthesisTask,
    gsstrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSpeechSynthesisTask' smart constructor.
newtype GetSpeechSynthesisTask = GetSpeechSynthesisTask'
  { taskId ::
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

-- | Creates a value of 'GetSpeechSynthesisTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The Amazon Polly generated identifier for a speech synthesis task.
mkGetSpeechSynthesisTask ::
  -- | 'taskId'
  Lude.Text ->
  GetSpeechSynthesisTask
mkGetSpeechSynthesisTask pTaskId_ =
  GetSpeechSynthesisTask' {taskId = pTaskId_}

-- | The Amazon Polly generated identifier for a speech synthesis task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsstTaskId :: Lens.Lens' GetSpeechSynthesisTask Lude.Text
gsstTaskId = Lens.lens (taskId :: GetSpeechSynthesisTask -> Lude.Text) (\s a -> s {taskId = a} :: GetSpeechSynthesisTask)
{-# DEPRECATED gsstTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest GetSpeechSynthesisTask where
  type Rs GetSpeechSynthesisTask = GetSpeechSynthesisTaskResponse
  request = Req.get pollyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSpeechSynthesisTaskResponse'
            Lude.<$> (x Lude..?> "SynthesisTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSpeechSynthesisTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetSpeechSynthesisTask where
  toPath GetSpeechSynthesisTask' {..} =
    Lude.mconcat ["/v1/synthesisTasks/", Lude.toBS taskId]

instance Lude.ToQuery GetSpeechSynthesisTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSpeechSynthesisTaskResponse' smart constructor.
data GetSpeechSynthesisTaskResponse = GetSpeechSynthesisTaskResponse'
  { synthesisTask ::
      Lude.Maybe SynthesisTask,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSpeechSynthesisTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'synthesisTask' - SynthesisTask object that provides information from the requested task, including output format, creation time, task status, and so on.
mkGetSpeechSynthesisTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSpeechSynthesisTaskResponse
mkGetSpeechSynthesisTaskResponse pResponseStatus_ =
  GetSpeechSynthesisTaskResponse'
    { synthesisTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | SynthesisTask object that provides information from the requested task, including output format, creation time, task status, and so on.
--
-- /Note:/ Consider using 'synthesisTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsstrsSynthesisTask :: Lens.Lens' GetSpeechSynthesisTaskResponse (Lude.Maybe SynthesisTask)
gsstrsSynthesisTask = Lens.lens (synthesisTask :: GetSpeechSynthesisTaskResponse -> Lude.Maybe SynthesisTask) (\s a -> s {synthesisTask = a} :: GetSpeechSynthesisTaskResponse)
{-# DEPRECATED gsstrsSynthesisTask "Use generic-lens or generic-optics with 'synthesisTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsstrsResponseStatus :: Lens.Lens' GetSpeechSynthesisTaskResponse Lude.Int
gsstrsResponseStatus = Lens.lens (responseStatus :: GetSpeechSynthesisTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSpeechSynthesisTaskResponse)
{-# DEPRECATED gsstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
