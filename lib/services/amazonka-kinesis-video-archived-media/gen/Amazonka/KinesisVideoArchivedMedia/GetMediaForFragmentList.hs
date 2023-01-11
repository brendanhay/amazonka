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
-- Module      : Amazonka.KinesisVideoArchivedMedia.GetMediaForFragmentList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets media for a list of fragments (specified by fragment number) from
-- the archived data in an Amazon Kinesis video stream.
--
-- You must first call the @GetDataEndpoint@ API to get an endpoint. Then
-- send the @GetMediaForFragmentList@ requests to this endpoint using the
-- <https://docs.aws.amazon.com/cli/latest/reference/ --endpoint-url parameter>.
--
-- For limits, see
-- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/limits.html Kinesis Video Streams Limits>.
--
-- If an error is thrown after invoking a Kinesis Video Streams archived
-- media API, in addition to the HTTP status code and the response body, it
-- includes the following pieces of information:
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
module Amazonka.KinesisVideoArchivedMedia.GetMediaForFragmentList
  ( -- * Creating a Request
    GetMediaForFragmentList (..),
    newGetMediaForFragmentList,

    -- * Request Lenses
    getMediaForFragmentList_streamARN,
    getMediaForFragmentList_streamName,
    getMediaForFragmentList_fragments,

    -- * Destructuring the Response
    GetMediaForFragmentListResponse (..),
    newGetMediaForFragmentListResponse,

    -- * Response Lenses
    getMediaForFragmentListResponse_contentType,
    getMediaForFragmentListResponse_httpStatus,
    getMediaForFragmentListResponse_payload,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoArchivedMedia.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMediaForFragmentList' smart constructor.
data GetMediaForFragmentList = GetMediaForFragmentList'
  { -- | The Amazon Resource Name (ARN) of the stream from which to retrieve
    -- fragment media. Specify either this parameter or the @StreamName@
    -- parameter.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream from which to retrieve fragment media. Specify
    -- either this parameter or the @StreamARN@ parameter.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | A list of the numbers of fragments for which to retrieve media. You
    -- retrieve these values with ListFragments.
    fragments :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaForFragmentList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'getMediaForFragmentList_streamARN' - The Amazon Resource Name (ARN) of the stream from which to retrieve
-- fragment media. Specify either this parameter or the @StreamName@
-- parameter.
--
-- 'streamName', 'getMediaForFragmentList_streamName' - The name of the stream from which to retrieve fragment media. Specify
-- either this parameter or the @StreamARN@ parameter.
--
-- 'fragments', 'getMediaForFragmentList_fragments' - A list of the numbers of fragments for which to retrieve media. You
-- retrieve these values with ListFragments.
newGetMediaForFragmentList ::
  -- | 'fragments'
  Prelude.NonEmpty Prelude.Text ->
  GetMediaForFragmentList
newGetMediaForFragmentList pFragments_ =
  GetMediaForFragmentList'
    { streamARN =
        Prelude.Nothing,
      streamName = Prelude.Nothing,
      fragments = Lens.coerced Lens.# pFragments_
    }

-- | The Amazon Resource Name (ARN) of the stream from which to retrieve
-- fragment media. Specify either this parameter or the @StreamName@
-- parameter.
getMediaForFragmentList_streamARN :: Lens.Lens' GetMediaForFragmentList (Prelude.Maybe Prelude.Text)
getMediaForFragmentList_streamARN = Lens.lens (\GetMediaForFragmentList' {streamARN} -> streamARN) (\s@GetMediaForFragmentList' {} a -> s {streamARN = a} :: GetMediaForFragmentList)

-- | The name of the stream from which to retrieve fragment media. Specify
-- either this parameter or the @StreamARN@ parameter.
getMediaForFragmentList_streamName :: Lens.Lens' GetMediaForFragmentList (Prelude.Maybe Prelude.Text)
getMediaForFragmentList_streamName = Lens.lens (\GetMediaForFragmentList' {streamName} -> streamName) (\s@GetMediaForFragmentList' {} a -> s {streamName = a} :: GetMediaForFragmentList)

-- | A list of the numbers of fragments for which to retrieve media. You
-- retrieve these values with ListFragments.
getMediaForFragmentList_fragments :: Lens.Lens' GetMediaForFragmentList (Prelude.NonEmpty Prelude.Text)
getMediaForFragmentList_fragments = Lens.lens (\GetMediaForFragmentList' {fragments} -> fragments) (\s@GetMediaForFragmentList' {} a -> s {fragments = a} :: GetMediaForFragmentList) Prelude.. Lens.coerced

instance Core.AWSRequest GetMediaForFragmentList where
  type
    AWSResponse GetMediaForFragmentList =
      GetMediaForFragmentListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetMediaForFragmentListResponse'
            Prelude.<$> (h Data..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetMediaForFragmentList where
  hashWithSalt _salt GetMediaForFragmentList' {..} =
    _salt `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` fragments

instance Prelude.NFData GetMediaForFragmentList where
  rnf GetMediaForFragmentList' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf fragments

instance Data.ToHeaders GetMediaForFragmentList where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetMediaForFragmentList where
  toJSON GetMediaForFragmentList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName,
            Prelude.Just ("Fragments" Data..= fragments)
          ]
      )

instance Data.ToPath GetMediaForFragmentList where
  toPath = Prelude.const "/getMediaForFragmentList"

instance Data.ToQuery GetMediaForFragmentList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMediaForFragmentListResponse' smart constructor.
data GetMediaForFragmentListResponse = GetMediaForFragmentListResponse'
  { -- | The content type of the requested media.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The payload that Kinesis Video Streams returns is a sequence of chunks
    -- from the specified stream. For information about the chunks, see
    -- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia>.
    -- The chunks that Kinesis Video Streams returns in the
    -- @GetMediaForFragmentList@ call also include the following additional
    -- Matroska (MKV) tags:
    --
    -- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the
    --     chunk.
    --
    -- -   AWS_KINESISVIDEO_SERVER_SIDE_TIMESTAMP - Server-side timestamp of
    --     the fragment.
    --
    -- -   AWS_KINESISVIDEO_PRODUCER_SIDE_TIMESTAMP - Producer-side timestamp
    --     of the fragment.
    --
    -- The following tags will be included if an exception occurs:
    --
    -- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - The number of the fragment that
    --     threw the exception
    --
    -- -   AWS_KINESISVIDEO_EXCEPTION_ERROR_CODE - The integer code of the
    --     exception
    --
    -- -   AWS_KINESISVIDEO_EXCEPTION_MESSAGE - A text description of the
    --     exception
    payload :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMediaForFragmentListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getMediaForFragmentListResponse_contentType' - The content type of the requested media.
--
-- 'httpStatus', 'getMediaForFragmentListResponse_httpStatus' - The response's http status code.
--
-- 'payload', 'getMediaForFragmentListResponse_payload' - The payload that Kinesis Video Streams returns is a sequence of chunks
-- from the specified stream. For information about the chunks, see
-- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia>.
-- The chunks that Kinesis Video Streams returns in the
-- @GetMediaForFragmentList@ call also include the following additional
-- Matroska (MKV) tags:
--
-- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the
--     chunk.
--
-- -   AWS_KINESISVIDEO_SERVER_SIDE_TIMESTAMP - Server-side timestamp of
--     the fragment.
--
-- -   AWS_KINESISVIDEO_PRODUCER_SIDE_TIMESTAMP - Producer-side timestamp
--     of the fragment.
--
-- The following tags will be included if an exception occurs:
--
-- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - The number of the fragment that
--     threw the exception
--
-- -   AWS_KINESISVIDEO_EXCEPTION_ERROR_CODE - The integer code of the
--     exception
--
-- -   AWS_KINESISVIDEO_EXCEPTION_MESSAGE - A text description of the
--     exception
newGetMediaForFragmentListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'payload'
  Data.ResponseBody ->
  GetMediaForFragmentListResponse
newGetMediaForFragmentListResponse
  pHttpStatus_
  pPayload_ =
    GetMediaForFragmentListResponse'
      { contentType =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        payload = pPayload_
      }

-- | The content type of the requested media.
getMediaForFragmentListResponse_contentType :: Lens.Lens' GetMediaForFragmentListResponse (Prelude.Maybe Prelude.Text)
getMediaForFragmentListResponse_contentType = Lens.lens (\GetMediaForFragmentListResponse' {contentType} -> contentType) (\s@GetMediaForFragmentListResponse' {} a -> s {contentType = a} :: GetMediaForFragmentListResponse)

-- | The response's http status code.
getMediaForFragmentListResponse_httpStatus :: Lens.Lens' GetMediaForFragmentListResponse Prelude.Int
getMediaForFragmentListResponse_httpStatus = Lens.lens (\GetMediaForFragmentListResponse' {httpStatus} -> httpStatus) (\s@GetMediaForFragmentListResponse' {} a -> s {httpStatus = a} :: GetMediaForFragmentListResponse)

-- | The payload that Kinesis Video Streams returns is a sequence of chunks
-- from the specified stream. For information about the chunks, see
-- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_dataplane_PutMedia.html PutMedia>.
-- The chunks that Kinesis Video Streams returns in the
-- @GetMediaForFragmentList@ call also include the following additional
-- Matroska (MKV) tags:
--
-- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - Fragment number returned in the
--     chunk.
--
-- -   AWS_KINESISVIDEO_SERVER_SIDE_TIMESTAMP - Server-side timestamp of
--     the fragment.
--
-- -   AWS_KINESISVIDEO_PRODUCER_SIDE_TIMESTAMP - Producer-side timestamp
--     of the fragment.
--
-- The following tags will be included if an exception occurs:
--
-- -   AWS_KINESISVIDEO_FRAGMENT_NUMBER - The number of the fragment that
--     threw the exception
--
-- -   AWS_KINESISVIDEO_EXCEPTION_ERROR_CODE - The integer code of the
--     exception
--
-- -   AWS_KINESISVIDEO_EXCEPTION_MESSAGE - A text description of the
--     exception
getMediaForFragmentListResponse_payload :: Lens.Lens' GetMediaForFragmentListResponse Data.ResponseBody
getMediaForFragmentListResponse_payload = Lens.lens (\GetMediaForFragmentListResponse' {payload} -> payload) (\s@GetMediaForFragmentListResponse' {} a -> s {payload = a} :: GetMediaForFragmentListResponse)
