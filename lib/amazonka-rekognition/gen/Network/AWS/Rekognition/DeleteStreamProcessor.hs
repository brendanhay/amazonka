{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteStreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stream processor identified by @Name@ . You assign the value for @Name@ when you create the stream processor with 'CreateStreamProcessor' . You might not be able to use the same name for a stream processor for a few seconds after calling @DeleteStreamProcessor@ .
module Network.AWS.Rekognition.DeleteStreamProcessor
  ( -- * Creating a request
    DeleteStreamProcessor (..),
    mkDeleteStreamProcessor,

    -- ** Request lenses
    dName,

    -- * Destructuring the response
    DeleteStreamProcessorResponse (..),
    mkDeleteStreamProcessorResponse,

    -- ** Response lenses
    dsprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteStreamProcessor' smart constructor.
newtype DeleteStreamProcessor = DeleteStreamProcessor'
  { -- | The name of the stream processor you want to delete.
    name :: Types.StreamProcessorName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamProcessor' value with any optional fields omitted.
mkDeleteStreamProcessor ::
  -- | 'name'
  Types.StreamProcessorName ->
  DeleteStreamProcessor
mkDeleteStreamProcessor name = DeleteStreamProcessor' {name}

-- | The name of the stream processor you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DeleteStreamProcessor Types.StreamProcessorName
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteStreamProcessor where
  toJSON DeleteStreamProcessor {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteStreamProcessor where
  type Rs DeleteStreamProcessor = DeleteStreamProcessorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "RekognitionService.DeleteStreamProcessor")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStreamProcessorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteStreamProcessorResponse' smart constructor.
newtype DeleteStreamProcessorResponse = DeleteStreamProcessorResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamProcessorResponse' value with any optional fields omitted.
mkDeleteStreamProcessorResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteStreamProcessorResponse
mkDeleteStreamProcessorResponse responseStatus =
  DeleteStreamProcessorResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprfrsResponseStatus :: Lens.Lens' DeleteStreamProcessorResponse Core.Int
dsprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
