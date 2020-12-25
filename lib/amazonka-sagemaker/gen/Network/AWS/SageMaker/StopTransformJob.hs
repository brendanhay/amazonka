{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopTransformJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a transform job.
--
-- When Amazon SageMaker receives a @StopTransformJob@ request, the status of the job changes to @Stopping@ . After Amazon SageMaker stops the job, the status is set to @Stopped@ . When you stop a transform job before it is completed, Amazon SageMaker doesn't store the job's output in Amazon S3.
module Network.AWS.SageMaker.StopTransformJob
  ( -- * Creating a request
    StopTransformJob (..),
    mkStopTransformJob,

    -- ** Request lenses
    stjTransformJobName,

    -- * Destructuring the response
    StopTransformJobResponse (..),
    mkStopTransformJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopTransformJob' smart constructor.
newtype StopTransformJob = StopTransformJob'
  { -- | The name of the transform job to stop.
    transformJobName :: Types.TransformJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTransformJob' value with any optional fields omitted.
mkStopTransformJob ::
  -- | 'transformJobName'
  Types.TransformJobName ->
  StopTransformJob
mkStopTransformJob transformJobName =
  StopTransformJob' {transformJobName}

-- | The name of the transform job to stop.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjTransformJobName :: Lens.Lens' StopTransformJob Types.TransformJobName
stjTransformJobName = Lens.field @"transformJobName"
{-# DEPRECATED stjTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

instance Core.FromJSON StopTransformJob where
  toJSON StopTransformJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TransformJobName" Core..= transformJobName)]
      )

instance Core.AWSRequest StopTransformJob where
  type Rs StopTransformJob = StopTransformJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.StopTransformJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StopTransformJobResponse'

-- | /See:/ 'mkStopTransformJobResponse' smart constructor.
data StopTransformJobResponse = StopTransformJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopTransformJobResponse' value with any optional fields omitted.
mkStopTransformJobResponse ::
  StopTransformJobResponse
mkStopTransformJobResponse = StopTransformJobResponse'
