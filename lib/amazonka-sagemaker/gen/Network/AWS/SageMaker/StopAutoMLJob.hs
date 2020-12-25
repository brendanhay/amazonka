{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A method for forcing the termination of a running job.
module Network.AWS.SageMaker.StopAutoMLJob
  ( -- * Creating a request
    StopAutoMLJob (..),
    mkStopAutoMLJob,

    -- ** Request lenses
    samljAutoMLJobName,

    -- * Destructuring the response
    StopAutoMLJobResponse (..),
    mkStopAutoMLJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopAutoMLJob' smart constructor.
newtype StopAutoMLJob = StopAutoMLJob'
  { -- | The name of the object you are requesting.
    autoMLJobName :: Types.AutoMLJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAutoMLJob' value with any optional fields omitted.
mkStopAutoMLJob ::
  -- | 'autoMLJobName'
  Types.AutoMLJobName ->
  StopAutoMLJob
mkStopAutoMLJob autoMLJobName = StopAutoMLJob' {autoMLJobName}

-- | The name of the object you are requesting.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samljAutoMLJobName :: Lens.Lens' StopAutoMLJob Types.AutoMLJobName
samljAutoMLJobName = Lens.field @"autoMLJobName"
{-# DEPRECATED samljAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

instance Core.FromJSON StopAutoMLJob where
  toJSON StopAutoMLJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AutoMLJobName" Core..= autoMLJobName)]
      )

instance Core.AWSRequest StopAutoMLJob where
  type Rs StopAutoMLJob = StopAutoMLJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.StopAutoMLJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull StopAutoMLJobResponse'

-- | /See:/ 'mkStopAutoMLJobResponse' smart constructor.
data StopAutoMLJobResponse = StopAutoMLJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopAutoMLJobResponse' value with any optional fields omitted.
mkStopAutoMLJobResponse ::
  StopAutoMLJobResponse
mkStopAutoMLJobResponse = StopAutoMLJobResponse'
