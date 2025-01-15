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
-- Module      : Amazonka.MediaLive.DescribeInputDeviceThumbnail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the latest thumbnail data for the input device.
module Amazonka.MediaLive.DescribeInputDeviceThumbnail
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
    describeInputDeviceThumbnailResponse_contentLength,
    describeInputDeviceThumbnailResponse_contentType,
    describeInputDeviceThumbnailResponse_eTag,
    describeInputDeviceThumbnailResponse_lastModified,
    describeInputDeviceThumbnailResponse_httpStatus,
    describeInputDeviceThumbnailResponse_body,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DescribeInputDeviceThumbnailRequest
--
-- /See:/ 'newDescribeInputDeviceThumbnail' smart constructor.
data DescribeInputDeviceThumbnail = DescribeInputDeviceThumbnail'
  { -- | The unique ID of this input device. For example, hd-123456789abcdef.
    inputDeviceId :: Prelude.Text,
    -- | The HTTP Accept header. Indicates the requested type for the thumbnail.
    accept :: AcceptHeader
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
describeInputDeviceThumbnail_inputDeviceId :: Lens.Lens' DescribeInputDeviceThumbnail Prelude.Text
describeInputDeviceThumbnail_inputDeviceId = Lens.lens (\DescribeInputDeviceThumbnail' {inputDeviceId} -> inputDeviceId) (\s@DescribeInputDeviceThumbnail' {} a -> s {inputDeviceId = a} :: DescribeInputDeviceThumbnail)

-- | The HTTP Accept header. Indicates the requested type for the thumbnail.
describeInputDeviceThumbnail_accept :: Lens.Lens' DescribeInputDeviceThumbnail AcceptHeader
describeInputDeviceThumbnail_accept = Lens.lens (\DescribeInputDeviceThumbnail' {accept} -> accept) (\s@DescribeInputDeviceThumbnail' {} a -> s {accept = a} :: DescribeInputDeviceThumbnail)

instance Core.AWSRequest DescribeInputDeviceThumbnail where
  type
    AWSResponse DescribeInputDeviceThumbnail =
      DescribeInputDeviceThumbnailResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          DescribeInputDeviceThumbnailResponse'
            Prelude.<$> (h Data..#? "Content-Length")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Last-Modified")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance
  Prelude.Hashable
    DescribeInputDeviceThumbnail
  where
  hashWithSalt _salt DescribeInputDeviceThumbnail' {..} =
    _salt
      `Prelude.hashWithSalt` inputDeviceId
      `Prelude.hashWithSalt` accept

instance Prelude.NFData DescribeInputDeviceThumbnail where
  rnf DescribeInputDeviceThumbnail' {..} =
    Prelude.rnf inputDeviceId `Prelude.seq`
      Prelude.rnf accept

instance Data.ToHeaders DescribeInputDeviceThumbnail where
  toHeaders DescribeInputDeviceThumbnail' {..} =
    Prelude.mconcat
      [ "accept" Data.=# accept,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DescribeInputDeviceThumbnail where
  toPath DescribeInputDeviceThumbnail' {..} =
    Prelude.mconcat
      [ "/prod/inputDevices/",
        Data.toBS inputDeviceId,
        "/thumbnailData"
      ]

instance Data.ToQuery DescribeInputDeviceThumbnail where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeInputDeviceThumbnailResponse
--
-- /See:/ 'newDescribeInputDeviceThumbnailResponse' smart constructor.
data DescribeInputDeviceThumbnailResponse = DescribeInputDeviceThumbnailResponse'
  { -- | The length of the content.
    contentLength :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the media type of the thumbnail.
    contentType :: Prelude.Maybe ContentType,
    -- | The unique, cacheable version of this thumbnail.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The date and time the thumbnail was last updated at the device.
    lastModified :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The binary data for the thumbnail that the Link device has most recently
    -- sent to MediaLive.
    body :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInputDeviceThumbnailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentLength', 'describeInputDeviceThumbnailResponse_contentLength' - The length of the content.
--
-- 'contentType', 'describeInputDeviceThumbnailResponse_contentType' - Specifies the media type of the thumbnail.
--
-- 'eTag', 'describeInputDeviceThumbnailResponse_eTag' - The unique, cacheable version of this thumbnail.
--
-- 'lastModified', 'describeInputDeviceThumbnailResponse_lastModified' - The date and time the thumbnail was last updated at the device.
--
-- 'httpStatus', 'describeInputDeviceThumbnailResponse_httpStatus' - The response's http status code.
--
-- 'body', 'describeInputDeviceThumbnailResponse_body' - The binary data for the thumbnail that the Link device has most recently
-- sent to MediaLive.
newDescribeInputDeviceThumbnailResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'body'
  Data.ResponseBody ->
  DescribeInputDeviceThumbnailResponse
newDescribeInputDeviceThumbnailResponse
  pHttpStatus_
  pBody_ =
    DescribeInputDeviceThumbnailResponse'
      { contentLength =
          Prelude.Nothing,
        contentType = Prelude.Nothing,
        eTag = Prelude.Nothing,
        lastModified = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        body = pBody_
      }

-- | The length of the content.
describeInputDeviceThumbnailResponse_contentLength :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Prelude.Maybe Prelude.Integer)
describeInputDeviceThumbnailResponse_contentLength = Lens.lens (\DescribeInputDeviceThumbnailResponse' {contentLength} -> contentLength) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {contentLength = a} :: DescribeInputDeviceThumbnailResponse)

-- | Specifies the media type of the thumbnail.
describeInputDeviceThumbnailResponse_contentType :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Prelude.Maybe ContentType)
describeInputDeviceThumbnailResponse_contentType = Lens.lens (\DescribeInputDeviceThumbnailResponse' {contentType} -> contentType) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {contentType = a} :: DescribeInputDeviceThumbnailResponse)

-- | The unique, cacheable version of this thumbnail.
describeInputDeviceThumbnailResponse_eTag :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Prelude.Maybe Prelude.Text)
describeInputDeviceThumbnailResponse_eTag = Lens.lens (\DescribeInputDeviceThumbnailResponse' {eTag} -> eTag) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {eTag = a} :: DescribeInputDeviceThumbnailResponse)

-- | The date and time the thumbnail was last updated at the device.
describeInputDeviceThumbnailResponse_lastModified :: Lens.Lens' DescribeInputDeviceThumbnailResponse (Prelude.Maybe Prelude.UTCTime)
describeInputDeviceThumbnailResponse_lastModified = Lens.lens (\DescribeInputDeviceThumbnailResponse' {lastModified} -> lastModified) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {lastModified = a} :: DescribeInputDeviceThumbnailResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeInputDeviceThumbnailResponse_httpStatus :: Lens.Lens' DescribeInputDeviceThumbnailResponse Prelude.Int
describeInputDeviceThumbnailResponse_httpStatus = Lens.lens (\DescribeInputDeviceThumbnailResponse' {httpStatus} -> httpStatus) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {httpStatus = a} :: DescribeInputDeviceThumbnailResponse)

-- | The binary data for the thumbnail that the Link device has most recently
-- sent to MediaLive.
describeInputDeviceThumbnailResponse_body :: Lens.Lens' DescribeInputDeviceThumbnailResponse Data.ResponseBody
describeInputDeviceThumbnailResponse_body = Lens.lens (\DescribeInputDeviceThumbnailResponse' {body} -> body) (\s@DescribeInputDeviceThumbnailResponse' {} a -> s {body = a} :: DescribeInputDeviceThumbnailResponse)
