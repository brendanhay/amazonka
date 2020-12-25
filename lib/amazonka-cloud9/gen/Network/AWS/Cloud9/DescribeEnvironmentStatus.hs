{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DescribeEnvironmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets status information for an AWS Cloud9 development environment.
module Network.AWS.Cloud9.DescribeEnvironmentStatus
  ( -- * Creating a request
    DescribeEnvironmentStatus (..),
    mkDescribeEnvironmentStatus,

    -- ** Request lenses
    desEnvironmentId,

    -- * Destructuring the response
    DescribeEnvironmentStatusResponse (..),
    mkDescribeEnvironmentStatusResponse,

    -- ** Response lenses
    desrrsMessage,
    desrrsStatus,
    desrrsResponseStatus,
  )
where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEnvironmentStatus' smart constructor.
newtype DescribeEnvironmentStatus = DescribeEnvironmentStatus'
  { -- | The ID of the environment to get status information about.
    environmentId :: Types.EnvironmentId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentStatus' value with any optional fields omitted.
mkDescribeEnvironmentStatus ::
  -- | 'environmentId'
  Types.EnvironmentId ->
  DescribeEnvironmentStatus
mkDescribeEnvironmentStatus environmentId =
  DescribeEnvironmentStatus' {environmentId}

-- | The ID of the environment to get status information about.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desEnvironmentId :: Lens.Lens' DescribeEnvironmentStatus Types.EnvironmentId
desEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED desEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Core.FromJSON DescribeEnvironmentStatus where
  toJSON DescribeEnvironmentStatus {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("environmentId" Core..= environmentId)]
      )

instance Core.AWSRequest DescribeEnvironmentStatus where
  type
    Rs DescribeEnvironmentStatus =
      DescribeEnvironmentStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCloud9WorkspaceManagementService.DescribeEnvironmentStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEnvironmentStatusResponse'
            Core.<$> (x Core..:? "message")
            Core.<*> (x Core..:? "status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEnvironmentStatusResponse' smart constructor.
data DescribeEnvironmentStatusResponse = DescribeEnvironmentStatusResponse'
  { -- | Any informational message about the status of the environment.
    message :: Core.Maybe Types.Message,
    -- | The status of the environment. Available values include:
    --
    --
    --     * @connecting@ : The environment is connecting.
    --
    --
    --     * @creating@ : The environment is being created.
    --
    --
    --     * @deleting@ : The environment is being deleted.
    --
    --
    --     * @error@ : The environment is in an error state.
    --
    --
    --     * @ready@ : The environment is ready.
    --
    --
    --     * @stopped@ : The environment is stopped.
    --
    --
    --     * @stopping@ : The environment is stopping.
    status :: Core.Maybe Types.EnvironmentStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentStatusResponse' value with any optional fields omitted.
mkDescribeEnvironmentStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEnvironmentStatusResponse
mkDescribeEnvironmentStatusResponse responseStatus =
  DescribeEnvironmentStatusResponse'
    { message = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Any informational message about the status of the environment.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsMessage :: Lens.Lens' DescribeEnvironmentStatusResponse (Core.Maybe Types.Message)
desrrsMessage = Lens.field @"message"
{-# DEPRECATED desrrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status of the environment. Available values include:
--
--
--     * @connecting@ : The environment is connecting.
--
--
--     * @creating@ : The environment is being created.
--
--
--     * @deleting@ : The environment is being deleted.
--
--
--     * @error@ : The environment is in an error state.
--
--
--     * @ready@ : The environment is ready.
--
--
--     * @stopped@ : The environment is stopped.
--
--
--     * @stopping@ : The environment is stopping.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsStatus :: Lens.Lens' DescribeEnvironmentStatusResponse (Core.Maybe Types.EnvironmentStatus)
desrrsStatus = Lens.field @"status"
{-# DEPRECATED desrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrrsResponseStatus :: Lens.Lens' DescribeEnvironmentStatusResponse Core.Int
desrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED desrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
