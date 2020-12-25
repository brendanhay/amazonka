{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.StopAccessLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops access logging on the specified container. When you stop access logging on a container, MediaStore stops sending access logs to Amazon CloudWatch Logs. These access logs are not saved and are not retrievable.
module Network.AWS.MediaStore.StopAccessLogging
  ( -- * Creating a request
    StopAccessLogging (..),
    mkStopAccessLogging,

    -- ** Request lenses
    salContainerName,

    -- * Destructuring the response
    StopAccessLoggingResponse (..),
    mkStopAccessLoggingResponse,

    -- ** Response lenses
    salrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopAccessLogging' smart constructor.
newtype StopAccessLogging = StopAccessLogging'
  { -- | The name of the container that you want to stop access logging on.
    containerName :: Types.ContainerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAccessLogging' value with any optional fields omitted.
mkStopAccessLogging ::
  -- | 'containerName'
  Types.ContainerName ->
  StopAccessLogging
mkStopAccessLogging containerName =
  StopAccessLogging' {containerName}

-- | The name of the container that you want to stop access logging on.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salContainerName :: Lens.Lens' StopAccessLogging Types.ContainerName
salContainerName = Lens.field @"containerName"
{-# DEPRECATED salContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

instance Core.FromJSON StopAccessLogging where
  toJSON StopAccessLogging {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.AWSRequest StopAccessLogging where
  type Rs StopAccessLogging = StopAccessLoggingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "MediaStore_20170901.StopAccessLogging")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopAccessLoggingResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopAccessLoggingResponse' smart constructor.
newtype StopAccessLoggingResponse = StopAccessLoggingResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopAccessLoggingResponse' value with any optional fields omitted.
mkStopAccessLoggingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopAccessLoggingResponse
mkStopAccessLoggingResponse responseStatus =
  StopAccessLoggingResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salrrsResponseStatus :: Lens.Lens' StopAccessLoggingResponse Core.Int
salrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED salrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
