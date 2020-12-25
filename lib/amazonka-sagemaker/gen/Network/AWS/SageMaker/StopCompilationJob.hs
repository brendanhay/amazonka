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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopCompilationJob' smart constructor.
newtype StopCompilationJob = StopCompilationJob'
  { -- | The name of the model compilation job to stop.
    compilationJobName :: Types.EntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopCompilationJob' value with any optional fields omitted.
mkStopCompilationJob ::
  -- | 'compilationJobName'
  Types.EntityName ->
  StopCompilationJob
mkStopCompilationJob compilationJobName =
  StopCompilationJob' {compilationJobName}

-- | The name of the model compilation job to stop.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scjCompilationJobName :: Lens.Lens' StopCompilationJob Types.EntityName
scjCompilationJobName = Lens.field @"compilationJobName"
{-# DEPRECATED scjCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

instance Core.FromJSON StopCompilationJob where
  toJSON StopCompilationJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CompilationJobName" Core..= compilationJobName)]
      )

instance Core.AWSRequest StopCompilationJob where
  type Rs StopCompilationJob = StopCompilationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.StopCompilationJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StopCompilationJobResponse'

-- | /See:/ 'mkStopCompilationJobResponse' smart constructor.
data StopCompilationJobResponse = StopCompilationJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopCompilationJobResponse' value with any optional fields omitted.
mkStopCompilationJobResponse ::
  StopCompilationJobResponse
mkStopCompilationJobResponse = StopCompilationJobResponse'
