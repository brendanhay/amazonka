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
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetClip
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads an MP4 file (clip) containing the archived, on-demand media
-- from the specified video stream over the specified time range.
--
-- Both the StreamName and the StreamARN parameters are optional, but you
-- must specify either the StreamName or the StreamARN when invoking this
-- API operation.
--
-- As a prerequisite to using GetCLip API, you must obtain an endpoint
-- using @GetDataEndpoint@, specifying GET_CLIP for@@ the @APIName@
-- parameter.
--
-- An Amazon Kinesis video stream has the following requirements for
-- providing data through MP4:
--
-- -   The media must contain h.264 or h.265 encoded video and, optionally,
--     AAC or G.711 encoded audio. Specifically, the codec ID of track 1
--     should be @V_MPEG\/ISO\/AVC@ (for h.264) or V_MPEGH\/ISO\/HEVC (for
--     H.265). Optionally, the codec ID of track 2 should be @A_AAC@ (for
--     AAC) or A_MS\/ACM (for G.711).
--
-- -   Data retention must be greater than 0.
--
-- -   The video track of each fragment must contain codec private data in
--     the Advanced Video Coding (AVC) for H.264 format and HEVC for H.265
--     format. For more information, see
--     <https://www.iso.org/standard/55980.html MPEG-4 specification ISO\/IEC 14496-15>.
--     For information about adapting stream data to a given format, see
--     <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags>.
--
-- -   The audio track (if present) of each fragment must contain codec
--     private data in the AAC format
--     (<https://www.iso.org/standard/43345.html AAC specification ISO\/IEC 13818-7>)
--     or the
--     <http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html MS Wave format>.
--
-- You can monitor the amount of outgoing data by monitoring the
-- @GetClip.OutgoingBytes@ Amazon CloudWatch metric. For information about
-- using CloudWatch to monitor Kinesis Video Streams, see
-- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/monitoring.html Monitoring Kinesis Video Streams>.
-- For pricing information, see
-- <https://aws.amazon.com/kinesis/video-streams/pricing/ Amazon Kinesis Video Streams Pricing>
-- and <https://aws.amazon.com/pricing/ AWS Pricing>. Charges for outgoing
-- AWS data apply.
module Network.AWS.KinesisVideoArchivedMedia.GetClip
  ( -- * Creating a Request
    GetClip (..),
    newGetClip,

    -- * Request Lenses
    getClip_streamARN,
    getClip_streamName,
    getClip_clipFragmentSelector,

    -- * Destructuring the Response
    GetClipResponse (..),
    newGetClipResponse,

    -- * Response Lenses
    getClipResponse_contentType,
    getClipResponse_httpStatus,
    getClipResponse_payload,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetClip' smart constructor.
data GetClip = GetClip'
  { -- | The Amazon Resource Name (ARN) of the stream for which to retrieve the
    -- media clip.
    --
    -- You must specify either the StreamName or the StreamARN.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream for which to retrieve the media clip.
    --
    -- You must specify either the StreamName or the StreamARN.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | The time range of the requested clip and the source of the timestamps.
    clipFragmentSelector :: ClipFragmentSelector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetClip' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'getClip_streamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the
-- media clip.
--
-- You must specify either the StreamName or the StreamARN.
--
-- 'streamName', 'getClip_streamName' - The name of the stream for which to retrieve the media clip.
--
-- You must specify either the StreamName or the StreamARN.
--
-- 'clipFragmentSelector', 'getClip_clipFragmentSelector' - The time range of the requested clip and the source of the timestamps.
newGetClip ::
  -- | 'clipFragmentSelector'
  ClipFragmentSelector ->
  GetClip
newGetClip pClipFragmentSelector_ =
  GetClip'
    { streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      clipFragmentSelector = pClipFragmentSelector_
    }

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the
-- media clip.
--
-- You must specify either the StreamName or the StreamARN.
getClip_streamARN :: Lens.Lens' GetClip (Prelude.Maybe Prelude.Text)
getClip_streamARN = Lens.lens (\GetClip' {streamARN} -> streamARN) (\s@GetClip' {} a -> s {streamARN = a} :: GetClip)

-- | The name of the stream for which to retrieve the media clip.
--
-- You must specify either the StreamName or the StreamARN.
getClip_streamName :: Lens.Lens' GetClip (Prelude.Maybe Prelude.Text)
getClip_streamName = Lens.lens (\GetClip' {streamName} -> streamName) (\s@GetClip' {} a -> s {streamName = a} :: GetClip)

-- | The time range of the requested clip and the source of the timestamps.
getClip_clipFragmentSelector :: Lens.Lens' GetClip ClipFragmentSelector
getClip_clipFragmentSelector = Lens.lens (\GetClip' {clipFragmentSelector} -> clipFragmentSelector) (\s@GetClip' {} a -> s {clipFragmentSelector = a} :: GetClip)

instance Prelude.AWSRequest GetClip where
  type Rs GetClip = GetClipResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveBody
      ( \s h x ->
          GetClipResponse'
            Prelude.<$> (h Prelude..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetClip

instance Prelude.NFData GetClip

instance Prelude.ToHeaders GetClip where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON GetClip where
  toJSON GetClip' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StreamARN" Prelude..=) Prelude.<$> streamARN,
            ("StreamName" Prelude..=) Prelude.<$> streamName,
            Prelude.Just
              ( "ClipFragmentSelector"
                  Prelude..= clipFragmentSelector
              )
          ]
      )

instance Prelude.ToPath GetClip where
  toPath = Prelude.const "/getClip"

instance Prelude.ToQuery GetClip where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClipResponse' smart constructor.
data GetClipResponse = GetClipResponse'
  { -- | The content type of the media in the requested clip.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Traditional MP4 file that contains the media clip from the specified
    -- video stream. The output will contain the first 100 MB or the first 200
    -- fragments from the specified start timestamp. For more information, see
    -- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/limits.html Kinesis Video Streams Limits>.
    payload :: Prelude.RsBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'getClipResponse_contentType' - The content type of the media in the requested clip.
--
-- 'httpStatus', 'getClipResponse_httpStatus' - The response's http status code.
--
-- 'payload', 'getClipResponse_payload' - Traditional MP4 file that contains the media clip from the specified
-- video stream. The output will contain the first 100 MB or the first 200
-- fragments from the specified start timestamp. For more information, see
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/limits.html Kinesis Video Streams Limits>.
newGetClipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'payload'
  Prelude.RsBody ->
  GetClipResponse
newGetClipResponse pHttpStatus_ pPayload_ =
  GetClipResponse'
    { contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      payload = pPayload_
    }

-- | The content type of the media in the requested clip.
getClipResponse_contentType :: Lens.Lens' GetClipResponse (Prelude.Maybe Prelude.Text)
getClipResponse_contentType = Lens.lens (\GetClipResponse' {contentType} -> contentType) (\s@GetClipResponse' {} a -> s {contentType = a} :: GetClipResponse)

-- | The response's http status code.
getClipResponse_httpStatus :: Lens.Lens' GetClipResponse Prelude.Int
getClipResponse_httpStatus = Lens.lens (\GetClipResponse' {httpStatus} -> httpStatus) (\s@GetClipResponse' {} a -> s {httpStatus = a} :: GetClipResponse)

-- | Traditional MP4 file that contains the media clip from the specified
-- video stream. The output will contain the first 100 MB or the first 200
-- fragments from the specified start timestamp. For more information, see
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/limits.html Kinesis Video Streams Limits>.
getClipResponse_payload :: Lens.Lens' GetClipResponse Prelude.RsBody
getClipResponse_payload = Lens.lens (\GetClipResponse' {payload} -> payload) (\s@GetClipResponse' {} a -> s {payload = a} :: GetClipResponse)
