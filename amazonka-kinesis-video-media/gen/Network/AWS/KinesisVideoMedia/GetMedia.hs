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
-- Module      : Network.AWS.KinesisVideoMedia.GetMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to retrieve media content from a Kinesis video stream. In
-- the request, you identify the stream name or stream Amazon Resource Name
-- (ARN), and the starting chunk. Kinesis Video Streams then returns a
-- stream of chunks in order by fragment number.
--
-- You must first call the @GetDataEndpoint@ API to get an endpoint. Then
-- send the @GetMedia@ requests to this endpoint using the
-- <https://docs.aws.amazon.com/cli/latest/reference/ --endpoint-url parameter>.
--
-- When you put media data (fragments) on a stream, Kinesis Video Streams
-- stores each incoming fragment and related metadata in what is called a
-- \"chunk.\" For more information, see
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia>.
-- The @GetMedia@ API returns a stream of these chunks starting from the
-- chunk that you specify in the request.
--
-- The following limits apply when using the @GetMedia@ API:
--
-- -   A client can call @GetMedia@ up to five times per second per stream.
--
-- -   Kinesis Video Streams sends media data at a rate of up to 25
--     megabytes per second (or 200 megabits per second) during a
--     @GetMedia@ session.
--
-- If an error is thrown after invoking a Kinesis Video Streams media API,
-- in addition to the HTTP status code and the response body, it includes
-- the following pieces of information:
--
-- -   @x-amz-ErrorType@ HTTP header – contains a more specific error type
--     in addition to what the HTTP status code provides.
--
-- -   @x-amz-RequestId@ HTTP header – if you want to report an issue to
--     AWS, the support team can better diagnose the problem if given the
--     Request Id.
--
-- Both the HTTP status code and the ErrorType header can be utilized to
-- make programmatic decisions about whether errors are retry-able and
-- under what conditions, as well as provide information on what actions
-- the client programmer might need to take in order to successfully try
-- again.
--
-- For more information, see the __Errors__ section at the bottom of this
-- topic, as well as
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/CommonErrors.html Common Errors>.
module Network.AWS.KinesisVideoMedia.GetMedia
  ( -- * Creating a Request
    GetMedia (..),
    newGetMedia,

    -- * Request Lenses
    getMedia_streamARN,
    getMedia_streamName,
    getMedia_startSelector,

    -- * Destructuring the Response
    GetMediaResponse (..),
    newGetMediaResponse,

    -- * Response Lenses
    getMediaResponse_contentType,
    getMediaResponse_httpStatus,
    getMediaResponse_payload,
  )
where

import Network.AWS.KinesisVideoMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMedia' smart constructor.
data GetMedia = GetMedia'
  { -- | The ARN of the stream from where you want to get the media content. If
    -- you don\'t specify the @streamARN@, you must specify the @streamName@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The Kinesis video stream name from where you want to get the media
    -- content. If you don\'t specify the @streamName@, you must specify the
    -- @streamARN@.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | Identifies the starting chunk to get from the specified stream.
    startSelector :: StartSelector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMedia' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'getMedia_streamARN' - The ARN of the stream from where you want to get the media content. If
-- you don\'t specify the @streamARN@, you must specify the @streamName@.
--
-- 'streamName', 'getMedia_streamName' - The Kinesis video stream name from where you want to get the media
-- content. If you don\'t specify the @streamName@, you must specify the
-- @streamARN@.
--
-- 'startSelector', 'getMedia_startSelector' - Identifies the starting chunk to get from the specified stream.
newGetMedia ::
  -- | 'startSelector'
  StartSelector ->
  GetMedia
newGetMedia pStartSelector_ =
  GetMedia'
    { streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      startSelector = pStartSelector_
    }

-- | The ARN of the stream from where you want to get the media content. If
-- you don\'t specify the @streamARN@, you must specify the @streamName@.
getMedia_streamARN :: Lens.Lens' GetMedia (Prelude.Maybe Prelude.Text)
getMedia_streamARN = Lens.lens (\GetMedia' {streamARN} -> streamARN) (\s@GetMedia' {} a -> s {streamARN = a} :: GetMedia)

-- | The Kinesis video stream name from where you want to get the media
-- content. If you don\'t specify the @streamName@, you must specify the
-- @streamARN@.
getMedia_streamName :: Lens.Lens' GetMedia (Prelude.Maybe Prelude.Text)
getMedia_streamName = Lens.lens (\GetMedia' {streamName} -> streamName) (\s@GetMedia' {} a -> s {streamName = a} :: GetMedia)

-- | Identifies the starting chunk to get from the specified stream.
getMedia_startSelector :: Lens.Lens' GetMedia StartSelector
getMedia_startSelector = Lens.lens (\GetMedia' {startSelector} -> startSelector) (\s@GetMedia' {} a -> s {startSelector = a} :: GetMedia)

instance Prelude.AWSRequest GetMedia where
  type Rs GetMedia = GetMediaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          GetMediaResponse'
            Prelude.<$> (h Prelude..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetMedia

instance Prelude.NFData GetMedia

instance Prelude.ToHeaders GetMedia where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON GetMedia where
  toJSON GetMedia' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StreamARN" Prelude..=) Prelude.<$> streamARN,
            ("StreamName" Prelude..=) Prelude.<$> streamName,
            Prelude.Just
              ("StartSelector" Prelude..= startSelector)
          ]
      )

instance Prelude.ToPath GetMedia where
  toPath = Prelude.const "/getMedia"

instance Prelude.ToQuery GetMedia where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMediaResponse' smart constructor.
data GetMediaResponse = GetMediaResponse'
  { -- | The content type of the requested media.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The payload Kinesis Video Streams returns is a sequence of chunks from
    -- the specified stream. For information about the chunks, see . The chunks
    -- that Kinesis Video Streams returns in the @GetMedia@ call also include
    -- the following additional Matroska (MKV) tags:
    --
    -- -   AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event
    --     your @GetMedia@ call terminates, you can use this continuation token
    --     in your next request to get the next chunk where the last request
    --     terminated.
    --
    -- -   AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client
    --     applications can use this tag value to determine how far behind the
    --     chunk returned in the response is from the latest chunk on the
    --     stream.
    --
    -- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the
    --     chunk.
    --
    -- -   AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server timestamp of the
    --     fragment.
    --
    -- -   AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer timestamp of the
    --     fragment.
    --
    -- The following tags will be present if an error occurs:
    --
    -- -   AWS_KINESISVIDEO_ERROR_CODE - String description of an error that
    --     caused GetMedia to stop.
    --
    -- -   AWS_KINESISVIDEO_ERROR_ID: Integer code of the error.
    --
    -- The error codes are as follows:
    --
    -- -   3002 - Error writing to the stream
    --
    -- -   4000 - Requested fragment is not found
    --
    -- -   4500 - Access denied for the stream\'s KMS key
    --
    -- -   4501 - Stream\'s KMS key is disabled
    --
    -- -   4502 - Validation error on the stream\'s KMS key
    --
    -- -   4503 - KMS key specified in the stream is unavailable
    --
    -- -   4504 - Invalid usage of the KMS key specified in the stream
    --
    -- -   4505 - Invalid state of the KMS key specified in the stream
    --
    -- -   4506 - Unable to find the KMS key specified in the stream
    --
    -- -   5000 - Internal error
    payload :: Prelude.RsBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getMediaResponse_contentType' - The content type of the requested media.
--
-- 'httpStatus', 'getMediaResponse_httpStatus' - The response's http status code.
--
-- 'payload', 'getMediaResponse_payload' - The payload Kinesis Video Streams returns is a sequence of chunks from
-- the specified stream. For information about the chunks, see . The chunks
-- that Kinesis Video Streams returns in the @GetMedia@ call also include
-- the following additional Matroska (MKV) tags:
--
-- -   AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event
--     your @GetMedia@ call terminates, you can use this continuation token
--     in your next request to get the next chunk where the last request
--     terminated.
--
-- -   AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client
--     applications can use this tag value to determine how far behind the
--     chunk returned in the response is from the latest chunk on the
--     stream.
--
-- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the
--     chunk.
--
-- -   AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server timestamp of the
--     fragment.
--
-- -   AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer timestamp of the
--     fragment.
--
-- The following tags will be present if an error occurs:
--
-- -   AWS_KINESISVIDEO_ERROR_CODE - String description of an error that
--     caused GetMedia to stop.
--
-- -   AWS_KINESISVIDEO_ERROR_ID: Integer code of the error.
--
-- The error codes are as follows:
--
-- -   3002 - Error writing to the stream
--
-- -   4000 - Requested fragment is not found
--
-- -   4500 - Access denied for the stream\'s KMS key
--
-- -   4501 - Stream\'s KMS key is disabled
--
-- -   4502 - Validation error on the stream\'s KMS key
--
-- -   4503 - KMS key specified in the stream is unavailable
--
-- -   4504 - Invalid usage of the KMS key specified in the stream
--
-- -   4505 - Invalid state of the KMS key specified in the stream
--
-- -   4506 - Unable to find the KMS key specified in the stream
--
-- -   5000 - Internal error
newGetMediaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'payload'
  Prelude.RsBody ->
  GetMediaResponse
newGetMediaResponse pHttpStatus_ pPayload_ =
  GetMediaResponse'
    { contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      payload = pPayload_
    }

-- | The content type of the requested media.
getMediaResponse_contentType :: Lens.Lens' GetMediaResponse (Prelude.Maybe Prelude.Text)
getMediaResponse_contentType = Lens.lens (\GetMediaResponse' {contentType} -> contentType) (\s@GetMediaResponse' {} a -> s {contentType = a} :: GetMediaResponse)

-- | The response's http status code.
getMediaResponse_httpStatus :: Lens.Lens' GetMediaResponse Prelude.Int
getMediaResponse_httpStatus = Lens.lens (\GetMediaResponse' {httpStatus} -> httpStatus) (\s@GetMediaResponse' {} a -> s {httpStatus = a} :: GetMediaResponse)

-- | The payload Kinesis Video Streams returns is a sequence of chunks from
-- the specified stream. For information about the chunks, see . The chunks
-- that Kinesis Video Streams returns in the @GetMedia@ call also include
-- the following additional Matroska (MKV) tags:
--
-- -   AWS_KINESISVIDEO_CONTINUATION_TOKEN (UTF-8 string) - In the event
--     your @GetMedia@ call terminates, you can use this continuation token
--     in your next request to get the next chunk where the last request
--     terminated.
--
-- -   AWS_KINESISVIDEO_MILLIS_BEHIND_NOW (UTF-8 string) - Client
--     applications can use this tag value to determine how far behind the
--     chunk returned in the response is from the latest chunk on the
--     stream.
--
-- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the
--     chunk.
--
-- -   AWS_KINESISVIDEO_SERVER_TIMESTAMP - Server timestamp of the
--     fragment.
--
-- -   AWS_KINESISVIDEO_PRODUCER_TIMESTAMP - Producer timestamp of the
--     fragment.
--
-- The following tags will be present if an error occurs:
--
-- -   AWS_KINESISVIDEO_ERROR_CODE - String description of an error that
--     caused GetMedia to stop.
--
-- -   AWS_KINESISVIDEO_ERROR_ID: Integer code of the error.
--
-- The error codes are as follows:
--
-- -   3002 - Error writing to the stream
--
-- -   4000 - Requested fragment is not found
--
-- -   4500 - Access denied for the stream\'s KMS key
--
-- -   4501 - Stream\'s KMS key is disabled
--
-- -   4502 - Validation error on the stream\'s KMS key
--
-- -   4503 - KMS key specified in the stream is unavailable
--
-- -   4504 - Invalid usage of the KMS key specified in the stream
--
-- -   4505 - Invalid state of the KMS key specified in the stream
--
-- -   4506 - Unable to find the KMS key specified in the stream
--
-- -   5000 - Internal error
getMediaResponse_payload :: Lens.Lens' GetMediaResponse Prelude.RsBody
getMediaResponse_payload = Lens.lens (\GetMediaResponse' {payload} -> payload) (\s@GetMediaResponse' {} a -> s {payload = a} :: GetMediaResponse)
