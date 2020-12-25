{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInputDeviceThumbnail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the latest thumbnail data for the input device.
module Network.AWS.MediaLive.DescribeInputDeviceThumbnail
  ( -- * Creating a request
    DescribeInputDeviceThumbnail (..),
    mkDescribeInputDeviceThumbnail,

    -- ** Request lenses
    didtInputDeviceId,
    didtAccept,

    -- * Destructuring the response
    DescribeInputDeviceThumbnailResponse (..),
    mkDescribeInputDeviceThumbnailResponse,

    -- ** Response lenses
    didtrrsBody,
    didtrrsContentLength,
    didtrrsContentType,
    didtrrsETag,
    didtrrsLastModified,
    didtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeInputDeviceThumbnailRequest
--
-- /See:/ 'mkDescribeInputDeviceThumbnail' smart constructor.
data DescribeInputDeviceThumbnail = DescribeInputDeviceThumbnail'
  { -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text,
    -- | The HTTP Accept header. Indicates the requested type for the thumbnail.
    accept :: Types.AcceptHeader
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInputDeviceThumbnail' value with any optional fields omitted.
mkDescribeInputDeviceThumbnail ::
  -- | 'inputDeviceId'
  Core.Text ->
  -- | 'accept'
  Types.AcceptHeader ->
  DescribeInputDeviceThumbnail
mkDescribeInputDeviceThumbnail inputDeviceId accept =
  DescribeInputDeviceThumbnail' {inputDeviceId, accept}

-- | The unique ID of this input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtInputDeviceId :: Lens.Lens' DescribeInputDeviceThumbnail Core.Text
didtInputDeviceId = Lens.field @"inputDeviceId"
{-# DEPRECATED didtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

-- | The HTTP Accept header. Indicates the requested type for the thumbnail.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtAccept :: Lens.Lens' DescribeInputDeviceThumbnail Types.AcceptHeader
didtAccept = Lens.field @"accept"
{-# DEPRECATED didtAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

instance Core.AWSRequest DescribeInputDeviceThumbnail where
  type
    Rs DescribeInputDeviceThumbnail =
      DescribeInputDeviceThumbnailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/prod/inputDevices/" Core.<> (Core.toText inputDeviceId)
                Core.<> ("/thumbnailData")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "accept" accept
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveBody
      ( \s h x ->
          DescribeInputDeviceThumbnailResponse'
            Core.<$> (Core.pure x)
            Core.<*> (Core.parseHeaderMaybe "Content-Length" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Type" h)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Last-Modified" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Placeholder documentation for DescribeInputDeviceThumbnailResponse
--
-- /See:/ 'mkDescribeInputDeviceThumbnailResponse' smart constructor.
data DescribeInputDeviceThumbnailResponse = DescribeInputDeviceThumbnailResponse'
  { -- | The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
    body :: Core.RsBody,
    -- | The length of the content.
    contentLength :: Core.Maybe Core.Integer,
    -- | Specifies the media type of the thumbnail.
    contentType :: Core.Maybe Types.ContentType,
    -- | The unique, cacheable version of this thumbnail.
    eTag :: Core.Maybe Core.Text,
    -- | The date and time the thumbnail was last updated at the device.
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'DescribeInputDeviceThumbnailResponse' value with any optional fields omitted.
mkDescribeInputDeviceThumbnailResponse ::
  -- | 'body'
  Core.RsBody ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeInputDeviceThumbnailResponse
mkDescribeInputDeviceThumbnailResponse body responseStatus =
  DescribeInputDeviceThumbnailResponse'
    { body,
      contentLength = Core.Nothing,
      contentType = Core.Nothing,
      eTag = Core.Nothing,
      lastModified = Core.Nothing,
      responseStatus
    }

-- | The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrrsBody :: Lens.Lens' DescribeInputDeviceThumbnailResponse Core.RsBody
didtrrsBody = Lens.field @"body"
{-# DEPRECATED didtrrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The length of the content.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrrsContentLength :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe Core.Integer)
didtrrsContentLength = Lens.field @"contentLength"
{-# DEPRECATED didtrrsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | Specifies the media type of the thumbnail.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrrsContentType :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe Types.ContentType)
didtrrsContentType = Lens.field @"contentType"
{-# DEPRECATED didtrrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The unique, cacheable version of this thumbnail.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrrsETag :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe Core.Text)
didtrrsETag = Lens.field @"eTag"
{-# DEPRECATED didtrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The date and time the thumbnail was last updated at the device.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrrsLastModified :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe Core.NominalDiffTime)
didtrrsLastModified = Lens.field @"lastModified"
{-# DEPRECATED didtrrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrrsResponseStatus :: Lens.Lens' DescribeInputDeviceThumbnailResponse Core.Int
didtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED didtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
