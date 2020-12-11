{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    didtrsETag,
    didtrsContentLength,
    didtrsLastModified,
    didtrsContentType,
    didtrsResponseStatus,
    didtrsBody,
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
  { inputDeviceId ::
      Lude.Text,
    accept :: AcceptHeader
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInputDeviceThumbnail' with the minimum fields required to make a request.
--
-- * 'accept' - The HTTP Accept header. Indicates the requested type for the thumbnail.
-- * 'inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
mkDescribeInputDeviceThumbnail ::
  -- | 'inputDeviceId'
  Lude.Text ->
  -- | 'accept'
  AcceptHeader ->
  DescribeInputDeviceThumbnail
mkDescribeInputDeviceThumbnail pInputDeviceId_ pAccept_ =
  DescribeInputDeviceThumbnail'
    { inputDeviceId = pInputDeviceId_,
      accept = pAccept_
    }

-- | The unique ID of this input device. For example, hd-123456789abcdef.
--
-- /Note:/ Consider using 'inputDeviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtInputDeviceId :: Lens.Lens' DescribeInputDeviceThumbnail Lude.Text
didtInputDeviceId = Lens.lens (inputDeviceId :: DescribeInputDeviceThumbnail -> Lude.Text) (\s a -> s {inputDeviceId = a} :: DescribeInputDeviceThumbnail)
{-# DEPRECATED didtInputDeviceId "Use generic-lens or generic-optics with 'inputDeviceId' instead." #-}

-- | The HTTP Accept header. Indicates the requested type for the thumbnail.
--
-- /Note:/ Consider using 'accept' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtAccept :: Lens.Lens' DescribeInputDeviceThumbnail AcceptHeader
didtAccept = Lens.lens (accept :: DescribeInputDeviceThumbnail -> AcceptHeader) (\s a -> s {accept = a} :: DescribeInputDeviceThumbnail)
{-# DEPRECATED didtAccept "Use generic-lens or generic-optics with 'accept' instead." #-}

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
            Lude.<*> (h Lude..#? "Last-Modified")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (Lude.pure x)
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
  { eTag ::
      Lude.Maybe
        Lude.Text,
    contentLength ::
      Lude.Maybe
        Lude.Integer,
    lastModified ::
      Lude.Maybe
        Lude.Timestamp,
    contentType ::
      Lude.Maybe
        ContentType,
    responseStatus ::
      Lude.Int,
    body ::
      Lude.RsBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'DescribeInputDeviceThumbnailResponse' with the minimum fields required to make a request.
--
-- * 'body' - The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
-- * 'contentLength' - The length of the content.
-- * 'contentType' - Specifies the media type of the thumbnail.
-- * 'eTag' - The unique, cacheable version of this thumbnail.
-- * 'lastModified' - The date and time the thumbnail was last updated at the device.
-- * 'responseStatus' - The response status code.
mkDescribeInputDeviceThumbnailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'body'
  Lude.RsBody ->
  DescribeInputDeviceThumbnailResponse
mkDescribeInputDeviceThumbnailResponse pResponseStatus_ pBody_ =
  DescribeInputDeviceThumbnailResponse'
    { eTag = Lude.Nothing,
      contentLength = Lude.Nothing,
      lastModified = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_,
      body = pBody_
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

-- | The binary data for the thumbnail that the Link device has most recently sent to MediaLive.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
didtrsBody :: Lens.Lens' DescribeInputDeviceThumbnailResponse Lude.RsBody
didtrsBody = Lens.lens (body :: DescribeInputDeviceThumbnailResponse -> Lude.RsBody) (\s a -> s {body = a} :: DescribeInputDeviceThumbnailResponse)
{-# DEPRECATED didtrsBody "Use generic-lens or generic-optics with 'body' instead." #-}
