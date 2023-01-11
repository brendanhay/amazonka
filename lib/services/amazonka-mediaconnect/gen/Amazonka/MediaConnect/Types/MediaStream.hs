{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConnect.Types.MediaStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.MediaStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.MediaStreamAttributes
import Amazonka.MediaConnect.Types.MediaStreamType
import qualified Amazonka.Prelude as Prelude

-- | A single track or stream of media that contains video, audio, or
-- ancillary data. After you add a media stream to a flow, you can
-- associate it with sources and outputs on that flow, as long as they use
-- the CDI protocol or the ST 2110 JPEG XS protocol. Each source or output
-- can consist of one or many media streams.
--
-- /See:/ 'newMediaStream' smart constructor.
data MediaStream = MediaStream'
  { -- | Attributes that are related to the media stream.
    attributes :: Prelude.Maybe MediaStreamAttributes,
    -- | The sample rate for the stream. This value is measured in Hz.
    clockRate :: Prelude.Maybe Prelude.Int,
    -- | A description that can help you quickly identify what your media stream
    -- is used for.
    description :: Prelude.Maybe Prelude.Text,
    -- | The resolution of the video.
    videoFormat :: Prelude.Maybe Prelude.Text,
    -- | The type of media stream.
    mediaStreamType :: MediaStreamType,
    -- | A unique identifier for the media stream.
    mediaStreamId :: Prelude.Int,
    -- | A name that helps you distinguish one media stream from another.
    mediaStreamName :: Prelude.Text,
    -- | The format type number (sometimes referred to as RTP payload type) of
    -- the media stream. MediaConnect assigns this value to the media stream.
    -- For ST 2110 JPEG XS outputs, you need to provide this value to the
    -- receiver.
    fmt :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'mediaStream_attributes' - Attributes that are related to the media stream.
--
-- 'clockRate', 'mediaStream_clockRate' - The sample rate for the stream. This value is measured in Hz.
--
-- 'description', 'mediaStream_description' - A description that can help you quickly identify what your media stream
-- is used for.
--
-- 'videoFormat', 'mediaStream_videoFormat' - The resolution of the video.
--
-- 'mediaStreamType', 'mediaStream_mediaStreamType' - The type of media stream.
--
-- 'mediaStreamId', 'mediaStream_mediaStreamId' - A unique identifier for the media stream.
--
-- 'mediaStreamName', 'mediaStream_mediaStreamName' - A name that helps you distinguish one media stream from another.
--
-- 'fmt', 'mediaStream_fmt' - The format type number (sometimes referred to as RTP payload type) of
-- the media stream. MediaConnect assigns this value to the media stream.
-- For ST 2110 JPEG XS outputs, you need to provide this value to the
-- receiver.
newMediaStream ::
  -- | 'mediaStreamType'
  MediaStreamType ->
  -- | 'mediaStreamId'
  Prelude.Int ->
  -- | 'mediaStreamName'
  Prelude.Text ->
  -- | 'fmt'
  Prelude.Int ->
  MediaStream
newMediaStream
  pMediaStreamType_
  pMediaStreamId_
  pMediaStreamName_
  pFmt_ =
    MediaStream'
      { attributes = Prelude.Nothing,
        clockRate = Prelude.Nothing,
        description = Prelude.Nothing,
        videoFormat = Prelude.Nothing,
        mediaStreamType = pMediaStreamType_,
        mediaStreamId = pMediaStreamId_,
        mediaStreamName = pMediaStreamName_,
        fmt = pFmt_
      }

-- | Attributes that are related to the media stream.
mediaStream_attributes :: Lens.Lens' MediaStream (Prelude.Maybe MediaStreamAttributes)
mediaStream_attributes = Lens.lens (\MediaStream' {attributes} -> attributes) (\s@MediaStream' {} a -> s {attributes = a} :: MediaStream)

-- | The sample rate for the stream. This value is measured in Hz.
mediaStream_clockRate :: Lens.Lens' MediaStream (Prelude.Maybe Prelude.Int)
mediaStream_clockRate = Lens.lens (\MediaStream' {clockRate} -> clockRate) (\s@MediaStream' {} a -> s {clockRate = a} :: MediaStream)

-- | A description that can help you quickly identify what your media stream
-- is used for.
mediaStream_description :: Lens.Lens' MediaStream (Prelude.Maybe Prelude.Text)
mediaStream_description = Lens.lens (\MediaStream' {description} -> description) (\s@MediaStream' {} a -> s {description = a} :: MediaStream)

-- | The resolution of the video.
mediaStream_videoFormat :: Lens.Lens' MediaStream (Prelude.Maybe Prelude.Text)
mediaStream_videoFormat = Lens.lens (\MediaStream' {videoFormat} -> videoFormat) (\s@MediaStream' {} a -> s {videoFormat = a} :: MediaStream)

-- | The type of media stream.
mediaStream_mediaStreamType :: Lens.Lens' MediaStream MediaStreamType
mediaStream_mediaStreamType = Lens.lens (\MediaStream' {mediaStreamType} -> mediaStreamType) (\s@MediaStream' {} a -> s {mediaStreamType = a} :: MediaStream)

-- | A unique identifier for the media stream.
mediaStream_mediaStreamId :: Lens.Lens' MediaStream Prelude.Int
mediaStream_mediaStreamId = Lens.lens (\MediaStream' {mediaStreamId} -> mediaStreamId) (\s@MediaStream' {} a -> s {mediaStreamId = a} :: MediaStream)

-- | A name that helps you distinguish one media stream from another.
mediaStream_mediaStreamName :: Lens.Lens' MediaStream Prelude.Text
mediaStream_mediaStreamName = Lens.lens (\MediaStream' {mediaStreamName} -> mediaStreamName) (\s@MediaStream' {} a -> s {mediaStreamName = a} :: MediaStream)

-- | The format type number (sometimes referred to as RTP payload type) of
-- the media stream. MediaConnect assigns this value to the media stream.
-- For ST 2110 JPEG XS outputs, you need to provide this value to the
-- receiver.
mediaStream_fmt :: Lens.Lens' MediaStream Prelude.Int
mediaStream_fmt = Lens.lens (\MediaStream' {fmt} -> fmt) (\s@MediaStream' {} a -> s {fmt = a} :: MediaStream)

instance Data.FromJSON MediaStream where
  parseJSON =
    Data.withObject
      "MediaStream"
      ( \x ->
          MediaStream'
            Prelude.<$> (x Data..:? "attributes")
            Prelude.<*> (x Data..:? "clockRate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "videoFormat")
            Prelude.<*> (x Data..: "mediaStreamType")
            Prelude.<*> (x Data..: "mediaStreamId")
            Prelude.<*> (x Data..: "mediaStreamName")
            Prelude.<*> (x Data..: "fmt")
      )

instance Prelude.Hashable MediaStream where
  hashWithSalt _salt MediaStream' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` clockRate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` videoFormat
      `Prelude.hashWithSalt` mediaStreamType
      `Prelude.hashWithSalt` mediaStreamId
      `Prelude.hashWithSalt` mediaStreamName
      `Prelude.hashWithSalt` fmt

instance Prelude.NFData MediaStream where
  rnf MediaStream' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf clockRate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf videoFormat
      `Prelude.seq` Prelude.rnf mediaStreamType
      `Prelude.seq` Prelude.rnf mediaStreamId
      `Prelude.seq` Prelude.rnf mediaStreamName
      `Prelude.seq` Prelude.rnf fmt
