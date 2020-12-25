{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an upload.
module Network.AWS.DeviceFarm.GetUpload
  ( -- * Creating a request
    GetUpload (..),
    mkGetUpload,

    -- ** Request lenses
    guArn,

    -- * Destructuring the response
    GetUploadResponse (..),
    mkGetUploadResponse,

    -- ** Response lenses
    gurrsUpload,
    gurrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get upload operation.
--
-- /See:/ 'mkGetUpload' smart constructor.
newtype GetUpload = GetUpload'
  { -- | The upload's ARN.
    arn :: Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetUpload' value with any optional fields omitted.
mkGetUpload ::
  -- | 'arn'
  Types.Arn ->
  GetUpload
mkGetUpload arn = GetUpload' {arn}

-- | The upload's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guArn :: Lens.Lens' GetUpload Types.Arn
guArn = Lens.field @"arn"
{-# DEPRECATED guArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON GetUpload where
  toJSON GetUpload {..} =
    Core.object (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.AWSRequest GetUpload where
  type Rs GetUpload = GetUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.GetUpload")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUploadResponse'
            Core.<$> (x Core..:? "upload") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the result of a get upload request.
--
-- /See:/ 'mkGetUploadResponse' smart constructor.
data GetUploadResponse = GetUploadResponse'
  { -- | An app or a set of one or more tests to upload or that have been uploaded.
    upload :: Core.Maybe Types.Upload,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetUploadResponse' value with any optional fields omitted.
mkGetUploadResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetUploadResponse
mkGetUploadResponse responseStatus =
  GetUploadResponse' {upload = Core.Nothing, responseStatus}

-- | An app or a set of one or more tests to upload or that have been uploaded.
--
-- /Note:/ Consider using 'upload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsUpload :: Lens.Lens' GetUploadResponse (Core.Maybe Types.Upload)
gurrsUpload = Lens.field @"upload"
{-# DEPRECATED gurrsUpload "Use generic-lens or generic-optics with 'upload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gurrsResponseStatus :: Lens.Lens' GetUploadResponse Core.Int
gurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
