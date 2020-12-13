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
    didtAccept,
    didtInputDeviceId,

    -- * Destructuring the response
    DescribeInputDeviceThumbnailResponse (..),
    mkDescribeInputDeviceThumbnailResponse,

    -- ** Response lenses
    didtrsETag,
    didtrsContentLength,
    didtrsBody,
    didtrsLastModified,
    didtrsContentType,
    didtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeInputDeviceThumbnailRequest
--
-- /See:/ 'mkDescribeInputDeviceThumbnail' smart constructor.
data DescribeInputDeviceThumbnail = DescribeInputDeviceThumbnail'
  { -- | The HTTP Accept header. Indicates the requested type for the thumbnail.
    accept :: AcceptHeader,
    -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputDeviceThumbnail' with the minimum fields required to make a request.
--
-- * 'accept' - The HTTP Accept header. Indicates the requested type for the thumbnail.
-- * 'inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
mkDescribeInputDeviceThumbnail ::
  -- | 'accept'
  AcceptHeader ->
  -- | 'inputDeviceId'
  Lude.Text ->
  DescribeInputDeviceThumbnail
mkDescribeInputDeviceThumbnail pAccept_ pInputDeviceId_ =
  DescribeInputDeviceThumbnail'
    { accept = pAccept_,
      inputDeviceId = pInputDeviceId_
    }

-- | The HTTP Accept header. Indicates the requested type for the thumbnail.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtAccept :: Lens.Lens' DescribeInputDeviceThumbnail AcceptHeader
didtAccept = Lens.lens (accept :: DescribeInputDeviceThumbnail -> AcceptHeader) (\s a -> s {accept = a} :: DescribeInputDeviceThumbnail)
{-# DEPRECATED didtAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

-- | The unique ID of this input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtInputDeviceId :: Lens.Lens' DescribeInputDeviceThumbnail Lude.Text
didtInputDeviceId = Lens.lens (inputDeviceId :: DescribeInputDeviceThumbnail -> Lude.Text) (\s a -> s {inputDeviceId = a} :: DescribeInputDeviceThumbnail)
{-# DEPRECATED didtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

instance Lude.AWSRequest DescribeInputDeviceThumbnail where
  type
    Rs DescribeInputDeviceThumbnail =
      DescribeInputDeviceThumbnailResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveBody
      ( \s h x ->
          DescribeInputDeviceThumbnailResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Content-Length")
            Lude.<*> (Lude.pure x)
            Lude.<*> (h Lude..#? "Last-Modified")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInputDeviceThumbnail where
  toHeaders DescribeInputDeviceThumbnail' {..} =
    Lude.mconcat
      [ "accept" Lude.=# accept,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DescribeInputDeviceThumbnail where
  toPath DescribeInputDeviceThumbnail' {..} =
    Lude.mconcat
      ["/prod/inputDevices/", Lude.toBS inputDeviceId, "/thumbnailData"]

instance Lude.ToQuery DescribeInputDeviceThumbnail where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeInputDeviceThumbnailResponse
--
-- /See:/ 'mkDescribeInputDeviceThumbnailResponse' smart constructor.
data DescribeInputDeviceThumbnailResponse = DescribeInputDeviceThumbnailResponse'
  { -- | The unique, cacheable version of this thumbnail.
    eTag :: Lude.Maybe Lude.Text,
    -- | The length of the content.
    contentLength :: Lude.Maybe Lude.Integer,
    -- | The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
    body :: Lude.RsBody,
    -- | The date and time the thumbnail was last updated at the device.
    lastModified :: Lude.Maybe Lude.Timestamp,
    -- | Specifies the media type of the thumbnail.
    contentType :: Lude.Maybe ContentType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'DescribeInputDeviceThumbnailResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The unique, cacheable version of this thumbnail.
-- * 'contentLength' - The length of the content.
-- * 'body' - The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
-- * 'lastModified' - The date and time the thumbnail was last updated at the device.
-- * 'contentType' - Specifies the media type of the thumbnail.
-- * 'responseStatus' - The response status code.
mkDescribeInputDeviceThumbnailResponse ::
  -- | 'body'
  Lude.RsBody ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInputDeviceThumbnailResponse
mkDescribeInputDeviceThumbnailResponse pBody_ pResponseStatus_ =
  DescribeInputDeviceThumbnailResponse'
    { eTag = Lude.Nothing,
      contentLength = Lude.Nothing,
      body = pBody_,
      lastModified = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique, cacheable version of this thumbnail.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrsETag :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Lude.Maybe Lude.Text)
didtrsETag = Lens.lens (eTag :: DescribeInputDeviceThumbnailResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: DescribeInputDeviceThumbnailResponse)
{-# DEPRECATED didtrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The length of the content.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrsContentLength :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Lude.Maybe Lude.Integer)
didtrsContentLength = Lens.lens (contentLength :: DescribeInputDeviceThumbnailResponse -> Lude.Maybe Lude.Integer) (\s a -> s {contentLength = a} :: DescribeInputDeviceThumbnailResponse)
{-# DEPRECATED didtrsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrsBody :: Lens.Lens' DescribeInputDeviceThumbnailResponse Lude.RsBody
didtrsBody = Lens.lens (body :: DescribeInputDeviceThumbnailResponse -> Lude.RsBody) (\s a -> s {body = a} :: DescribeInputDeviceThumbnailResponse)
{-# DEPRECATED didtrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The date and time the thumbnail was last updated at the device.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrsLastModified :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Lude.Maybe Lude.Timestamp)
didtrsLastModified = Lens.lens (lastModified :: DescribeInputDeviceThumbnailResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: DescribeInputDeviceThumbnailResponse)
{-# DEPRECATED didtrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Specifies the media type of the thumbnail.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrsContentType :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Lude.Maybe ContentType)
didtrsContentType = Lens.lens (contentType :: DescribeInputDeviceThumbnailResponse -> Lude.Maybe ContentType) (\s a -> s {contentType = a} :: DescribeInputDeviceThumbnailResponse)
{-# DEPRECATED didtrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrsResponseStatus :: Lens.Lens' DescribeInputDeviceThumbnailResponse Lude.Int
didtrsResponseStatus = Lens.lens (responseStatus :: DescribeInputDeviceThumbnailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInputDeviceThumbnailResponse)
{-# DEPRECATED didtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
