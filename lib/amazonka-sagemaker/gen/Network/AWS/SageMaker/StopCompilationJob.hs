{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopCompilationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a model compilation job.
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal. This gracefully shuts the job down. If the job hasn't stopped, it sends the SIGKILL signal.
-- When it receives a @StopCompilationJob@ request, Amazon SageMaker changes the 'CompilationJobSummary$CompilationJobStatus' of the job to @Stopping@ . After Amazon SageMaker stops the job, it sets the 'CompilationJobSummary$CompilationJobStatus' to @Stopped@ .
module Network.AWS.SageMaker.StopCompilationJob
  ( -- * Creating a request
    StopCompilationJob (..),
    mkStopCompilationJob,

    -- ** Request lenses
    scjCompilationJobName,

    -- * Destructuring the response
    StopCompilationJobResponse (..),
    mkStopCompilationJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkStopCompilationJob' smart constructor.
newtype StopCompilationJob = StopCompilationJob'
  { -- | The name of the model compilation job to stop.
    compilationJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopCompilationJob' with the minimum fields required to make a request.
--
-- * 'compilationJobName' - The name of the model compilation job to stop.
mkStopCompilationJob ::
  -- | 'compilationJobName'
  Lude.Text ->
  StopCompilationJob
mkStopCompilationJob pCompilationJobName_ =
  StopCompilationJob' {compilationJobName = pCompilationJobName_}

-- | The name of the model compilation job to stop.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scjCompilationJobName :: Lens.Lens' StopCompilationJob Lude.Text
scjCompilationJobName = Lens.lens (compilationJobName :: StopCompilationJob -> Lude.Text) (\s a -> s {compilationJobName = a} :: StopCompilationJob)
{-# DEPRECATED scjCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

instance Lude.AWSRequest StopCompilationJob where
  type Rs StopCompilationJob = StopCompilationJobResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull StopCompilationJobResponse'

instance Lude.ToHeaders StopCompilationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.StopCompilationJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopCompilationJob where
  toJSON StopCompilationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CompilationJobName" Lude..= compilationJobName)]
      )

instance Lude.ToPath StopCompilationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StopCompilationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopCompilationJobResponse' smart constructor.
data StopCompilationJobResponse = StopCompilationJobResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopCompilationJobResponse' with the minimum fields required to make a request.
mkStopCompilationJobResponse ::
  StopCompilationJobResponse
mkStopCompilationJobResponse = StopCompilationJobResponse'
