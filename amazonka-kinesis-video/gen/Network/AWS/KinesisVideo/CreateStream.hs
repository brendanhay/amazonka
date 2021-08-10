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
-- Module      : Network.AWS.KinesisVideo.CreateStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Kinesis video stream.
--
-- When you create a new stream, Kinesis Video Streams assigns it a version
-- number. When you change the stream\'s metadata, Kinesis Video Streams
-- updates the version.
--
-- @CreateStream@ is an asynchronous operation.
--
-- For information about how the service works, see
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/how-it-works.html How it Works>.
--
-- You must have permissions for the @KinesisVideo:CreateStream@ action.
module Network.AWS.KinesisVideo.CreateStream
  ( -- * Creating a Request
    CreateStream (..),
    newCreateStream,

    -- * Request Lenses
    createStream_dataRetentionInHours,
    createStream_kmsKeyId,
    createStream_deviceName,
    createStream_mediaType,
    createStream_tags,
    createStream_streamName,

    -- * Destructuring the Response
    CreateStreamResponse (..),
    newCreateStreamResponse,

    -- * Response Lenses
    createStreamResponse_streamARN,
    createStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisVideo.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateStream' smart constructor.
data CreateStream = CreateStream'
  { -- | The number of hours that you want to retain the data in the stream.
    -- Kinesis Video Streams retains the data in a data store that is
    -- associated with the stream.
    --
    -- The default value is 0, indicating that the stream does not persist
    -- data.
    --
    -- When the @DataRetentionInHours@ value is 0, consumers can still consume
    -- the fragments that remain in the service host buffer, which has a
    -- retention time limit of 5 minutes and a retention memory limit of 200
    -- MB. Fragments are removed from the buffer when either limit is reached.
    dataRetentionInHours :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the AWS Key Management Service (AWS KMS) key that you want
    -- Kinesis Video Streams to use to encrypt stream data.
    --
    -- If no key ID is specified, the default, Kinesis Video-managed key
    -- (@aws\/kinesisvideo@) is used.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey>.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the device that is writing to the stream.
    --
    -- In the current implementation, Kinesis Video Streams does not use this
    -- name.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The media type of the stream. Consumers of the stream can use this
    -- information when processing the stream. For more information about media
    -- types, see
    -- <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types>.
    -- If you choose to specify the @MediaType@, see
    -- <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements>
    -- for guidelines.
    --
    -- Example valid values include \"video\/h264\" and
    -- \"video\/h264,audio\/aac\".
    --
    -- This parameter is optional; the default value is @null@ (or empty in
    -- JSON).
    mediaType :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to associate with the specified stream. Each tag is a
    -- key-value pair (the value is optional).
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the stream that you are creating.
    --
    -- The stream name is an identifier for the stream, and must be unique for
    -- each account and region.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataRetentionInHours', 'createStream_dataRetentionInHours' - The number of hours that you want to retain the data in the stream.
-- Kinesis Video Streams retains the data in a data store that is
-- associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist
-- data.
--
-- When the @DataRetentionInHours@ value is 0, consumers can still consume
-- the fragments that remain in the service host buffer, which has a
-- retention time limit of 5 minutes and a retention memory limit of 200
-- MB. Fragments are removed from the buffer when either limit is reached.
--
-- 'kmsKeyId', 'createStream_kmsKeyId' - The ID of the AWS Key Management Service (AWS KMS) key that you want
-- Kinesis Video Streams to use to encrypt stream data.
--
-- If no key ID is specified, the default, Kinesis Video-managed key
-- (@aws\/kinesisvideo@) is used.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey>.
--
-- 'deviceName', 'createStream_deviceName' - The name of the device that is writing to the stream.
--
-- In the current implementation, Kinesis Video Streams does not use this
-- name.
--
-- 'mediaType', 'createStream_mediaType' - The media type of the stream. Consumers of the stream can use this
-- information when processing the stream. For more information about media
-- types, see
-- <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types>.
-- If you choose to specify the @MediaType@, see
-- <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements>
-- for guidelines.
--
-- Example valid values include \"video\/h264\" and
-- \"video\/h264,audio\/aac\".
--
-- This parameter is optional; the default value is @null@ (or empty in
-- JSON).
--
-- 'tags', 'createStream_tags' - A list of tags to associate with the specified stream. Each tag is a
-- key-value pair (the value is optional).
--
-- 'streamName', 'createStream_streamName' - A name for the stream that you are creating.
--
-- The stream name is an identifier for the stream, and must be unique for
-- each account and region.
newCreateStream ::
  -- | 'streamName'
  Prelude.Text ->
  CreateStream
newCreateStream pStreamName_ =
  CreateStream'
    { dataRetentionInHours =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      mediaType = Prelude.Nothing,
      tags = Prelude.Nothing,
      streamName = pStreamName_
    }

-- | The number of hours that you want to retain the data in the stream.
-- Kinesis Video Streams retains the data in a data store that is
-- associated with the stream.
--
-- The default value is 0, indicating that the stream does not persist
-- data.
--
-- When the @DataRetentionInHours@ value is 0, consumers can still consume
-- the fragments that remain in the service host buffer, which has a
-- retention time limit of 5 minutes and a retention memory limit of 200
-- MB. Fragments are removed from the buffer when either limit is reached.
createStream_dataRetentionInHours :: Lens.Lens' CreateStream (Prelude.Maybe Prelude.Natural)
createStream_dataRetentionInHours = Lens.lens (\CreateStream' {dataRetentionInHours} -> dataRetentionInHours) (\s@CreateStream' {} a -> s {dataRetentionInHours = a} :: CreateStream)

-- | The ID of the AWS Key Management Service (AWS KMS) key that you want
-- Kinesis Video Streams to use to encrypt stream data.
--
-- If no key ID is specified, the default, Kinesis Video-managed key
-- (@aws\/kinesisvideo@) is used.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestParameters DescribeKey>.
createStream_kmsKeyId :: Lens.Lens' CreateStream (Prelude.Maybe Prelude.Text)
createStream_kmsKeyId = Lens.lens (\CreateStream' {kmsKeyId} -> kmsKeyId) (\s@CreateStream' {} a -> s {kmsKeyId = a} :: CreateStream)

-- | The name of the device that is writing to the stream.
--
-- In the current implementation, Kinesis Video Streams does not use this
-- name.
createStream_deviceName :: Lens.Lens' CreateStream (Prelude.Maybe Prelude.Text)
createStream_deviceName = Lens.lens (\CreateStream' {deviceName} -> deviceName) (\s@CreateStream' {} a -> s {deviceName = a} :: CreateStream)

-- | The media type of the stream. Consumers of the stream can use this
-- information when processing the stream. For more information about media
-- types, see
-- <http://www.iana.org/assignments/media-types/media-types.xhtml Media Types>.
-- If you choose to specify the @MediaType@, see
-- <https://tools.ietf.org/html/rfc6838#section-4.2 Naming Requirements>
-- for guidelines.
--
-- Example valid values include \"video\/h264\" and
-- \"video\/h264,audio\/aac\".
--
-- This parameter is optional; the default value is @null@ (or empty in
-- JSON).
createStream_mediaType :: Lens.Lens' CreateStream (Prelude.Maybe Prelude.Text)
createStream_mediaType = Lens.lens (\CreateStream' {mediaType} -> mediaType) (\s@CreateStream' {} a -> s {mediaType = a} :: CreateStream)

-- | A list of tags to associate with the specified stream. Each tag is a
-- key-value pair (the value is optional).
createStream_tags :: Lens.Lens' CreateStream (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStream_tags = Lens.lens (\CreateStream' {tags} -> tags) (\s@CreateStream' {} a -> s {tags = a} :: CreateStream) Prelude.. Lens.mapping Lens._Coerce

-- | A name for the stream that you are creating.
--
-- The stream name is an identifier for the stream, and must be unique for
-- each account and region.
createStream_streamName :: Lens.Lens' CreateStream Prelude.Text
createStream_streamName = Lens.lens (\CreateStream' {streamName} -> streamName) (\s@CreateStream' {} a -> s {streamName = a} :: CreateStream)

instance Core.AWSRequest CreateStream where
  type AWSResponse CreateStream = CreateStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamResponse'
            Prelude.<$> (x Core..?> "StreamARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStream

instance Prelude.NFData CreateStream

instance Core.ToHeaders CreateStream where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateStream where
  toJSON CreateStream' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataRetentionInHours" Core..=)
              Prelude.<$> dataRetentionInHours,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("DeviceName" Core..=) Prelude.<$> deviceName,
            ("MediaType" Core..=) Prelude.<$> mediaType,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("StreamName" Core..= streamName)
          ]
      )

instance Core.ToPath CreateStream where
  toPath = Prelude.const "/createStream"

instance Core.ToQuery CreateStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamResponse' smart constructor.
data CreateStreamResponse = CreateStreamResponse'
  { -- | The Amazon Resource Name (ARN) of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'createStreamResponse_streamARN' - The Amazon Resource Name (ARN) of the stream.
--
-- 'httpStatus', 'createStreamResponse_httpStatus' - The response's http status code.
newCreateStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamResponse
newCreateStreamResponse pHttpStatus_ =
  CreateStreamResponse'
    { streamARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the stream.
createStreamResponse_streamARN :: Lens.Lens' CreateStreamResponse (Prelude.Maybe Prelude.Text)
createStreamResponse_streamARN = Lens.lens (\CreateStreamResponse' {streamARN} -> streamARN) (\s@CreateStreamResponse' {} a -> s {streamARN = a} :: CreateStreamResponse)

-- | The response's http status code.
createStreamResponse_httpStatus :: Lens.Lens' CreateStreamResponse Prelude.Int
createStreamResponse_httpStatus = Lens.lens (\CreateStreamResponse' {httpStatus} -> httpStatus) (\s@CreateStreamResponse' {} a -> s {httpStatus = a} :: CreateStreamResponse)

instance Prelude.NFData CreateStreamResponse
