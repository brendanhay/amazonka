{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts processing a stream processor. You create a stream processor by calling 'CreateStreamProcessor' . To tell @StartStreamProcessor@ which stream processor to start, use the value of the @Name@ field specified in the call to @CreateStreamProcessor@ .
module Network.AWS.Rekognition.StartStreamProcessor
  ( -- * Creating a request
    StartStreamProcessor (..),
    mkStartStreamProcessor,

    -- ** Request lenses
    sName,

    -- * Destructuring the response
    StartStreamProcessorResponse (..),
    mkStartStreamProcessorResponse,

    -- ** Response lenses
    ssprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartStreamProcessor' smart constructor.
newtype StartStreamProcessor = StartStreamProcessor'
  { -- | The name of the stream processor to start processing.
    name :: Types.StreamProcessorName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartStreamProcessor' value with any optional fields omitted.
mkStartStreamProcessor ::
  -- | 'name'
  Types.StreamProcessorName ->
  StartStreamProcessor
mkStartStreamProcessor name = StartStreamProcessor' {name}

-- | The name of the stream processor to start processing.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StartStreamProcessor Types.StreamProcessorName
sName = Lens.field @"name"
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON StartStreamProcessor where
  toJSON StartStreamProcessor {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StartStreamProcessor where
  type Rs StartStreamProcessor = StartStreamProcessorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.StartStreamProcessor")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartStreamProcessorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartStreamProcessorResponse' smart constructor.
newtype StartStreamProcessorResponse = StartStreamProcessorResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartStreamProcessorResponse' value with any optional fields omitted.
mkStartStreamProcessorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartStreamProcessorResponse
mkStartStreamProcessorResponse responseStatus =
  StartStreamProcessorResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssprfrsResponseStatus :: Lens.Lens' StartStreamProcessorResponse Core.Int
ssprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ssprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
