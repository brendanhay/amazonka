{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateImageBuilderStreamingURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL to start an image builder streaming session.
module Network.AWS.AppStream.CreateImageBuilderStreamingURL
  ( -- * Creating a request
    CreateImageBuilderStreamingURL (..),
    mkCreateImageBuilderStreamingURL,

    -- ** Request lenses
    cibsurlName,
    cibsurlValidity,

    -- * Destructuring the response
    CreateImageBuilderStreamingURLResponse (..),
    mkCreateImageBuilderStreamingURLResponse,

    -- ** Response lenses
    cibsurlrrsExpires,
    cibsurlrrsStreamingURL,
    cibsurlrrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateImageBuilderStreamingURL' smart constructor.
data CreateImageBuilderStreamingURL = CreateImageBuilderStreamingURL'
  { -- | The name of the image builder.
    name :: Types.String,
    -- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
    validity :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageBuilderStreamingURL' value with any optional fields omitted.
mkCreateImageBuilderStreamingURL ::
  -- | 'name'
  Types.String ->
  CreateImageBuilderStreamingURL
mkCreateImageBuilderStreamingURL name =
  CreateImageBuilderStreamingURL' {name, validity = Core.Nothing}

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlName :: Lens.Lens' CreateImageBuilderStreamingURL Types.String
cibsurlName = Lens.field @"name"
{-# DEPRECATED cibsurlName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlValidity :: Lens.Lens' CreateImageBuilderStreamingURL (Core.Maybe Core.Integer)
cibsurlValidity = Lens.field @"validity"
{-# DEPRECATED cibsurlValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

instance Core.FromJSON CreateImageBuilderStreamingURL where
  toJSON CreateImageBuilderStreamingURL {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Validity" Core..=) Core.<$> validity
          ]
      )

instance Core.AWSRequest CreateImageBuilderStreamingURL where
  type
    Rs CreateImageBuilderStreamingURL =
      CreateImageBuilderStreamingURLResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "PhotonAdminProxyService.CreateImageBuilderStreamingURL"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageBuilderStreamingURLResponse'
            Core.<$> (x Core..:? "Expires")
            Core.<*> (x Core..:? "StreamingURL")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateImageBuilderStreamingURLResponse' smart constructor.
data CreateImageBuilderStreamingURLResponse = CreateImageBuilderStreamingURLResponse'
  { -- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
    expires :: Core.Maybe Core.NominalDiffTime,
    -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Core.Maybe Types.StreamingURL,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateImageBuilderStreamingURLResponse' value with any optional fields omitted.
mkCreateImageBuilderStreamingURLResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateImageBuilderStreamingURLResponse
mkCreateImageBuilderStreamingURLResponse responseStatus =
  CreateImageBuilderStreamingURLResponse'
    { expires = Core.Nothing,
      streamingURL = Core.Nothing,
      responseStatus
    }

-- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlrrsExpires :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Core.Maybe Core.NominalDiffTime)
cibsurlrrsExpires = Lens.field @"expires"
{-# DEPRECATED cibsurlrrsExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The URL to start the AppStream 2.0 streaming session.
--
-- /Note:/ Consider using 'streamingURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlrrsStreamingURL :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Core.Maybe Types.StreamingURL)
cibsurlrrsStreamingURL = Lens.field @"streamingURL"
{-# DEPRECATED cibsurlrrsStreamingURL "Use generic-lens or generic-optics with 'streamingURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsurlrrsResponseStatus :: Lens.Lens' CreateImageBuilderStreamingURLResponse Core.Int
cibsurlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cibsurlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
