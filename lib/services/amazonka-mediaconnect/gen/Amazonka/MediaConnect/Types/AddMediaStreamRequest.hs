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
-- Module      : Amazonka.MediaConnect.Types.AddMediaStreamRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddMediaStreamRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.MediaStreamAttributesRequest
import Amazonka.MediaConnect.Types.MediaStreamType
import qualified Amazonka.Prelude as Prelude

-- | The media stream that you want to add to the flow.
--
-- /See:/ 'newAddMediaStreamRequest' smart constructor.
data AddMediaStreamRequest = AddMediaStreamRequest'
  { -- | The attributes that you want to assign to the new media stream.
    attributes :: Prelude.Maybe MediaStreamAttributesRequest,
    -- | The sample rate (in Hz) for the stream. If the media stream type is
    -- video or ancillary data, set this value to 90000. If the media stream
    -- type is audio, set this value to either 48000 or 96000.
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
    mediaStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddMediaStreamRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'addMediaStreamRequest_attributes' - The attributes that you want to assign to the new media stream.
--
-- 'clockRate', 'addMediaStreamRequest_clockRate' - The sample rate (in Hz) for the stream. If the media stream type is
-- video or ancillary data, set this value to 90000. If the media stream
-- type is audio, set this value to either 48000 or 96000.
--
-- 'description', 'addMediaStreamRequest_description' - A description that can help you quickly identify what your media stream
-- is used for.
--
-- 'videoFormat', 'addMediaStreamRequest_videoFormat' - The resolution of the video.
--
-- 'mediaStreamType', 'addMediaStreamRequest_mediaStreamType' - The type of media stream.
--
-- 'mediaStreamId', 'addMediaStreamRequest_mediaStreamId' - A unique identifier for the media stream.
--
-- 'mediaStreamName', 'addMediaStreamRequest_mediaStreamName' - A name that helps you distinguish one media stream from another.
newAddMediaStreamRequest ::
  -- | 'mediaStreamType'
  MediaStreamType ->
  -- | 'mediaStreamId'
  Prelude.Int ->
  -- | 'mediaStreamName'
  Prelude.Text ->
  AddMediaStreamRequest
newAddMediaStreamRequest
  pMediaStreamType_
  pMediaStreamId_
  pMediaStreamName_ =
    AddMediaStreamRequest'
      { attributes =
          Prelude.Nothing,
        clockRate = Prelude.Nothing,
        description = Prelude.Nothing,
        videoFormat = Prelude.Nothing,
        mediaStreamType = pMediaStreamType_,
        mediaStreamId = pMediaStreamId_,
        mediaStreamName = pMediaStreamName_
      }

-- | The attributes that you want to assign to the new media stream.
addMediaStreamRequest_attributes :: Lens.Lens' AddMediaStreamRequest (Prelude.Maybe MediaStreamAttributesRequest)
addMediaStreamRequest_attributes = Lens.lens (\AddMediaStreamRequest' {attributes} -> attributes) (\s@AddMediaStreamRequest' {} a -> s {attributes = a} :: AddMediaStreamRequest)

-- | The sample rate (in Hz) for the stream. If the media stream type is
-- video or ancillary data, set this value to 90000. If the media stream
-- type is audio, set this value to either 48000 or 96000.
addMediaStreamRequest_clockRate :: Lens.Lens' AddMediaStreamRequest (Prelude.Maybe Prelude.Int)
addMediaStreamRequest_clockRate = Lens.lens (\AddMediaStreamRequest' {clockRate} -> clockRate) (\s@AddMediaStreamRequest' {} a -> s {clockRate = a} :: AddMediaStreamRequest)

-- | A description that can help you quickly identify what your media stream
-- is used for.
addMediaStreamRequest_description :: Lens.Lens' AddMediaStreamRequest (Prelude.Maybe Prelude.Text)
addMediaStreamRequest_description = Lens.lens (\AddMediaStreamRequest' {description} -> description) (\s@AddMediaStreamRequest' {} a -> s {description = a} :: AddMediaStreamRequest)

-- | The resolution of the video.
addMediaStreamRequest_videoFormat :: Lens.Lens' AddMediaStreamRequest (Prelude.Maybe Prelude.Text)
addMediaStreamRequest_videoFormat = Lens.lens (\AddMediaStreamRequest' {videoFormat} -> videoFormat) (\s@AddMediaStreamRequest' {} a -> s {videoFormat = a} :: AddMediaStreamRequest)

-- | The type of media stream.
addMediaStreamRequest_mediaStreamType :: Lens.Lens' AddMediaStreamRequest MediaStreamType
addMediaStreamRequest_mediaStreamType = Lens.lens (\AddMediaStreamRequest' {mediaStreamType} -> mediaStreamType) (\s@AddMediaStreamRequest' {} a -> s {mediaStreamType = a} :: AddMediaStreamRequest)

-- | A unique identifier for the media stream.
addMediaStreamRequest_mediaStreamId :: Lens.Lens' AddMediaStreamRequest Prelude.Int
addMediaStreamRequest_mediaStreamId = Lens.lens (\AddMediaStreamRequest' {mediaStreamId} -> mediaStreamId) (\s@AddMediaStreamRequest' {} a -> s {mediaStreamId = a} :: AddMediaStreamRequest)

-- | A name that helps you distinguish one media stream from another.
addMediaStreamRequest_mediaStreamName :: Lens.Lens' AddMediaStreamRequest Prelude.Text
addMediaStreamRequest_mediaStreamName = Lens.lens (\AddMediaStreamRequest' {mediaStreamName} -> mediaStreamName) (\s@AddMediaStreamRequest' {} a -> s {mediaStreamName = a} :: AddMediaStreamRequest)

instance Prelude.Hashable AddMediaStreamRequest where
  hashWithSalt _salt AddMediaStreamRequest' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` clockRate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` videoFormat
      `Prelude.hashWithSalt` mediaStreamType
      `Prelude.hashWithSalt` mediaStreamId
      `Prelude.hashWithSalt` mediaStreamName

instance Prelude.NFData AddMediaStreamRequest where
  rnf AddMediaStreamRequest' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf clockRate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf videoFormat
      `Prelude.seq` Prelude.rnf mediaStreamType
      `Prelude.seq` Prelude.rnf mediaStreamId
      `Prelude.seq` Prelude.rnf mediaStreamName

instance Data.ToJSON AddMediaStreamRequest where
  toJSON AddMediaStreamRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("clockRate" Data..=) Prelude.<$> clockRate,
            ("description" Data..=) Prelude.<$> description,
            ("videoFormat" Data..=) Prelude.<$> videoFormat,
            Prelude.Just
              ("mediaStreamType" Data..= mediaStreamType),
            Prelude.Just ("mediaStreamId" Data..= mediaStreamId),
            Prelude.Just
              ("mediaStreamName" Data..= mediaStreamName)
          ]
      )
