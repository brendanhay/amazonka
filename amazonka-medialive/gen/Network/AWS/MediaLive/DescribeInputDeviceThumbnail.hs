{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeInputDeviceThumbnail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the latest thumbnail data for the input device.
module Network.AWS.MediaLive.DescribeInputDeviceThumbnail
  ( -- * Creating a Request
    DescribeInputDeviceThumbnail (..),
    newDescribeInputDeviceThumbnail,

    -- * Request Lenses
    describeInputDeviceThumbnail_inputDeviceId,
    describeInputDeviceThumbnail_accept,

    -- * Destructuring the Response
    DescribeInputDeviceThumbnailResponse (..),
    newDescribeInputDeviceThumbnailResponse,

    -- * Response Lenses
    describeInputDeviceThumbnailResponse_eTag,
    describeInputDeviceThumbnailResponse_contentType,
    describeInputDeviceThumbnailResponse_contentLength,
    describeInputDeviceThumbnailResponse_lastModified,
    describeInputDeviceThumbnailResponse_httpStatus,
    describeInputDeviceThumbnailResponse_body,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeInputDeviceThumbnailRequest
--
-- /See:/ 'newDescribeInputDeviceThumbnail' smart constructor.
data DescribeInputDeviceThumbnail = DescribeInputDeviceThumbnail'
  { -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Core.Text,
    -- | The HTTP Accept header. Indicates the requested type for the thumbnail.
    accept :: AcceptHeader
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInputDeviceThumbnail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDeviceId', 'describeInputDeviceThumbnail_inputDeviceId' - The unique ID of this input device. For example, hd-123456789abcdef.
--
-- 'accept', 'describeInputDeviceThumbnail_accept' - The HTTP Accept header. Indicates the requested type for the thumbnail.
newDescribeInputDeviceThumbnail ::
  -- | 'inputDeviceId'
  Core.Text ->
  -- | 'accept'
  AcceptHeader ->
  DescribeInputDeviceThumbnail
newDescribeInputDeviceThumbnail
  pInputDeviceId_
  pAccept_ =
    DescribeInputDeviceThumbnail'
      { inputDeviceId =
          pInputDeviceId_,
        accept = pAccept_
      }

-- | The unique ID of this input device. For example, hd-123456789abcdef.
describeInputDeviceThumbnail_inputDeviceId :: Lens.Lens' DescribeInputDeviceThumbnail Core.Text
describeInputDeviceThumbnail_inputDeviceId = Lens.lens (\DescribeInputDeviceThumbnail' {inputDeviceId} -> inputDeviceId) (\s@DescribeInputDeviceThumbnail' {} a -> s {inputDeviceId = a} :: DescribeInputDeviceThumbnail)

-- | The HTTP Accept header. Indicates the requested type for the thumbnail.
describeInputDeviceThumbnail_accept :: Lens.Lens' DescribeInputDeviceThumbnail AcceptHeader
describeInputDeviceThumbnail_accept = Lens.lens (\DescribeInputDeviceThumbnail' {accept} -> accept) (\s@DescribeInputDeviceThumbnail' {} a -> s {accept = a} :: DescribeInputDeviceThumbnail)

instance Core.AWSRequest DescribeInputDeviceThumbnail where
  type
    AWSResponse DescribeInputDeviceThumbnail =
      DescribeInputDeviceThumbnailResponse
  request = Request.get defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          DescribeInputDeviceThumbnailResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (h Core..#? "Content-Type")
            Core.<*> (h Core..#? "Content-Length")
            Core.<*> (h Core..#? "Last-Modified")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.pure x)
      )

instance Core.Hashable DescribeInputDeviceThumbnail

instance Core.NFData DescribeInputDeviceThumbnail

instance Core.ToHeaders DescribeInputDeviceThumbnail where
  toHeaders DescribeInputDeviceThumbnail' {..} =
    Core.mconcat
      [ "accept" Core.=# accept,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath DescribeInputDeviceThumbnail where
  toPath DescribeInputDeviceThumbnail' {..} =
    Core.mconcat
      [ "/prod/inputDevices/",
        Core.toBS inputDeviceId,
        "/thumbnailData"
      ]

instance Core.ToQuery DescribeInputDeviceThumbnail where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DescribeInputDeviceThumbnailResponse
--
-- /See:/ 'newDescribeInputDeviceThumbnailResponse' smart constructor.
data DescribeInputDeviceThumbnailResponse = DescribeInputDeviceThumbnailResponse'
  { -- | The unique, cacheable version of this thumbnail.
    eTag :: Core.Maybe Core.Text,
    -- | Specifies the media type of the thumbnail.
    contentType :: Core.Maybe ContentType,
    -- | The length of the content.
    contentLength :: Core.Maybe Core.Integer,
    -- | The date and time the thumbnail was last updated at the device.
    lastModified :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The binary data for the thumbnail that the Link device has most recently
    -- sent to MediaLive.
    body :: Core.ResponseBody
  }
  deriving (Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInputDeviceThumbnailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'describeInputDeviceThumbnailResponse_eTag' - The unique, cacheable version of this thumbnail.
--
-- 'contentType', 'describeInputDeviceThumbnailResponse_contentType' - Specifies the media type of the thumbnail.
--
-- 'contentLength', 'describeInputDeviceThumbnailResponse_contentLength' - The length of the content.
--
-- 'lastModified', 'describeInputDeviceThumbnailResponse_lastModified' - The date and time the thumbnail was last updated at the device.
--
-- 'httpStatus', 'describeInputDeviceThumbnailResponse_httpStatus' - The response's http status code.
--
-- 'body', 'describeInputDeviceThumbnailResponse_body' - The binary data for the thumbnail that the Link device has most recently
-- sent to MediaLive.
newDescribeInputDeviceThumbnailResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'body'
  Core.ResponseBody ->
  DescribeInputDeviceThumbnailResponse
newDescribeInputDeviceThumbnailResponse
  pHttpStatus_
  pBody_ =
    DescribeInputDeviceThumbnailResponse'
      { eTag =
          Core.Nothing,
        contentType = Core.Nothing,
        contentLength = Core.Nothing,
        lastModified = Core.Nothing,
        httpStatus = pHttpStatus_,
        body = pBody_
      }

-- | The unique, cacheable version of this thumbnail.
describeInputDeviceThumbnailResponse_eTag :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe Core.Text)
describeInputDeviceThumbnailResponse_eTag = Lens.lens (\DescribeInputDeviceThumbnailResponse' {eTag} -> eTag) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {eTag = a} :: DescribeInputDeviceThumbnailResponse)

-- | Specifies the media type of the thumbnail.
describeInputDeviceThumbnailResponse_contentType :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe ContentType)
describeInputDeviceThumbnailResponse_contentType = Lens.lens (\DescribeInputDeviceThumbnailResponse' {contentType} -> contentType) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {contentType = a} :: DescribeInputDeviceThumbnailResponse)

-- | The length of the content.
describeInputDeviceThumbnailResponse_contentLength :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe Core.Integer)
describeInputDeviceThumbnailResponse_contentLength = Lens.lens (\DescribeInputDeviceThumbnailResponse' {contentLength} -> contentLength) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {contentLength = a} :: DescribeInputDeviceThumbnailResponse)

-- | The date and time the thumbnail was last updated at the device.
describeInputDeviceThumbnailResponse_lastModified :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Core.Maybe Core.UTCTime)
describeInputDeviceThumbnailResponse_lastModified = Lens.lens (\DescribeInputDeviceThumbnailResponse' {lastModified} -> lastModified) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {lastModified = a} :: DescribeInputDeviceThumbnailResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeInputDeviceThumbnailResponse_httpStatus :: Lens.Lens' DescribeInputDeviceThumbnailResponse Core.Int
describeInputDeviceThumbnailResponse_httpStatus = Lens.lens (\DescribeInputDeviceThumbnailResponse' {httpStatus} -> httpStatus) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {httpStatus = a} :: DescribeInputDeviceThumbnailResponse)

-- | The binary data for the thumbnail that the Link device has most recently
-- sent to MediaLive.
describeInputDeviceThumbnailResponse_body :: Lens.Lens' DescribeInputDeviceThumbnailResponse Core.ResponseBody
describeInputDeviceThumbnailResponse_body = Lens.lens (\DescribeInputDeviceThumbnailResponse' {body} -> body) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {body = a} :: DescribeInputDeviceThumbnailResponse)
