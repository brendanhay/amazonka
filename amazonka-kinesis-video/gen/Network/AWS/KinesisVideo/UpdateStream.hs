{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisVideo.UpdateStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates stream metadata, such as the device name and media type.
--
-- You must provide the stream name or the Amazon Resource Name (ARN) of
-- the stream.
--
-- To make sure that you have the latest version of the stream before
-- updating it, you can specify the stream version. Kinesis Video Streams
-- assigns a version to each stream. When you update a stream, Kinesis
-- Video Streams assigns a new version number. To get the latest stream
-- version, use the @DescribeStream@ API.
--
-- @UpdateStream@ is an asynchronous operation, and takes time to complete.
module Network.AWS.KinesisVideo.UpdateStream
  ( -- * Creating a Request
    UpdateStream (..),
    newUpdateStream,

    -- * Request Lenses
    updateStream_deviceName,
    updateStream_mediaType,
    updateStream_streamARN,
    updateStream_streamName,
    updateStream_currentVersion,

    -- * Destructuring the Response
    UpdateStreamResponse (..),
    newUpdateStreamResponse,

    -- * Response Lenses
    updateStreamResponse_httpStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateStream' smart constructor.
data UpdateStream = UpdateStream'
  { -- | The name of the device that is writing to the stream.
    --
    -- In the current implementation, Kinesis Video Streams does not use this
    -- name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The stream\'s media type. Use @MediaType@ to specify the type of content
    -- that the stream contains to the consumers of the stream. For more
    -- information about media types, see
    -- <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types>.
    -- If you choose to specify the @MediaType@, see
    -- <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements>.
    --
    -- To play video on the console, you must specify the correct video type.
    -- For example, if the video in the stream is H.264, specify @video\/h264@
    -- as the @MediaType@.
    mediaType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the stream whose metadata you want to update.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream whose metadata you want to update.
    --
    -- The stream name is an identifier for the stream, and must be unique for
    -- each account and region.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The version of the stream whose metadata you want to update.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'updateStream_deviceName' - The name of the device that is writing to the stream.
--
-- In the current implementation, Kinesis Video Streams does not use this
-- name.
--
-- 'mediaType', 'updateStream_mediaType' - The stream\'s media type. Use @MediaType@ to specify the type of content
-- that the stream contains to the consumers of the stream. For more
-- information about media types, see
-- <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types>.
-- If you choose to specify the @MediaType@, see
-- <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements>.
--
-- To play video on the console, you must specify the correct video type.
-- For example, if the video in the stream is H.264, specify @video\/h264@
-- as the @MediaType@.
--
-- 'streamARN', 'updateStream_streamARN' - The ARN of the stream whose metadata you want to update.
--
-- 'streamName', 'updateStream_streamName' - The name of the stream whose metadata you want to update.
--
-- The stream name is an identifier for the stream, and must be unique for
-- each account and region.
--
-- 'currentVersion', 'updateStream_currentVersion' - The version of the stream whose metadata you want to update.
newUpdateStream ::
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateStream
newUpdateStream pCurrentVersion_ =
  UpdateStream'
    { deviceName = Prelude.Nothing,
      mediaType = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      currentVersion = pCurrentVersion_
    }

-- | The name of the device that is writing to the stream.
--
-- In the current implementation, Kinesis Video Streams does not use this
-- name.
updateStream_deviceName :: Lens.Lens' UpdateStream (Prelude.Maybe Prelude.Text)
updateStream_deviceName = Lens.lens (\UpdateStream' {deviceName} -> deviceName) (\s@UpdateStream' {} a -> s {deviceName = a} :: UpdateStream)

-- | The stream\'s media type. Use @MediaType@ to specify the type of content
-- that the stream contains to the consumers of the stream. For more
-- information about media types, see
-- <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types>.
-- If you choose to specify the @MediaType@, see
-- <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements>.
--
-- To play video on the console, you must specify the correct video type.
-- For example, if the video in the stream is H.264, specify @video\/h264@
-- as the @MediaType@.
updateStream_mediaType :: Lens.Lens' UpdateStream (Prelude.Maybe Prelude.Text)
updateStream_mediaType = Lens.lens (\UpdateStream' {mediaType} -> mediaType) (\s@UpdateStream' {} a -> s {mediaType = a} :: UpdateStream)

-- | The ARN of the stream whose metadata you want to update.
updateStream_streamARN :: Lens.Lens' UpdateStream (Prelude.Maybe Prelude.Text)
updateStream_streamARN = Lens.lens (\UpdateStream' {streamARN} -> streamARN) (\s@UpdateStream' {} a -> s {streamARN = a} :: UpdateStream)

-- | The name of the stream whose metadata you want to update.
--
-- The stream name is an identifier for the stream, and must be unique for
-- each account and region.
updateStream_streamName :: Lens.Lens' UpdateStream (Prelude.Maybe Prelude.Text)
updateStream_streamName = Lens.lens (\UpdateStream' {streamName} -> streamName) (\s@UpdateStream' {} a -> s {streamName = a} :: UpdateStream)

-- | The version of the stream whose metadata you want to update.
updateStream_currentVersion :: Lens.Lens' UpdateStream Prelude.Text
updateStream_currentVersion = Lens.lens (\UpdateStream' {currentVersion} -> currentVersion) (\s@UpdateStream' {} a -> s {currentVersion = a} :: UpdateStream)

instance Prelude.AWSRequest UpdateStream where
  type Rs UpdateStream = UpdateStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStream

instance Prelude.NFData UpdateStream

instance Prelude.ToHeaders UpdateStream where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateStream where
  toJSON UpdateStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeviceName" Prelude..=) Prelude.<$> deviceName,
            ("MediaType" Prelude..=) Prelude.<$> mediaType,
            ("StreamARN" Prelude..=) Prelude.<$> streamARN,
            ("StreamName" Prelude..=) Prelude.<$> streamName,
            Prelude.Just
              ("CurrentVersion" Prelude..= currentVersion)
          ]
      )

instance Prelude.ToPath UpdateStream where
  toPath = Prelude.const "/updateStream"

instance Prelude.ToQuery UpdateStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStreamResponse' smart constructor.
data UpdateStreamResponse = UpdateStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateStreamResponse_httpStatus' - The response's http status code.
newUpdateStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStreamResponse
newUpdateStreamResponse pHttpStatus_ =
  UpdateStreamResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateStreamResponse_httpStatus :: Lens.Lens' UpdateStreamResponse Prelude.Int
updateStreamResponse_httpStatus = Lens.lens (\UpdateStreamResponse' {httpStatus} -> httpStatus) (\s@UpdateStreamResponse' {} a -> s {httpStatus = a} :: UpdateStreamResponse)

instance Prelude.NFData UpdateStreamResponse
