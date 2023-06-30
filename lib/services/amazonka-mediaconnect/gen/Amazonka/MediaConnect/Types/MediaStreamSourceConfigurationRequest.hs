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
-- Module      : Amazonka.MediaConnect.Types.MediaStreamSourceConfigurationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.MediaStreamSourceConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.EncodingName
import Amazonka.MediaConnect.Types.InputConfigurationRequest
import qualified Amazonka.Prelude as Prelude

-- | The definition of a media stream that you want to associate with the
-- source.
--
-- /See:/ 'newMediaStreamSourceConfigurationRequest' smart constructor.
data MediaStreamSourceConfigurationRequest = MediaStreamSourceConfigurationRequest'
  { -- | The transport parameters that you want to associate with the media
    -- stream.
    inputConfigurations :: Prelude.Maybe [InputConfigurationRequest],
    -- | The name of the media stream.
    mediaStreamName :: Prelude.Text,
    -- | The format you want to use to encode the data. For ancillary data
    -- streams, set the encoding name to smpte291. For audio streams, set the
    -- encoding name to pcm. For video, 2110 streams, set the encoding name to
    -- raw. For video, JPEG XS streams, set the encoding name to jxsv.
    encodingName :: EncodingName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaStreamSourceConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputConfigurations', 'mediaStreamSourceConfigurationRequest_inputConfigurations' - The transport parameters that you want to associate with the media
-- stream.
--
-- 'mediaStreamName', 'mediaStreamSourceConfigurationRequest_mediaStreamName' - The name of the media stream.
--
-- 'encodingName', 'mediaStreamSourceConfigurationRequest_encodingName' - The format you want to use to encode the data. For ancillary data
-- streams, set the encoding name to smpte291. For audio streams, set the
-- encoding name to pcm. For video, 2110 streams, set the encoding name to
-- raw. For video, JPEG XS streams, set the encoding name to jxsv.
newMediaStreamSourceConfigurationRequest ::
  -- | 'mediaStreamName'
  Prelude.Text ->
  -- | 'encodingName'
  EncodingName ->
  MediaStreamSourceConfigurationRequest
newMediaStreamSourceConfigurationRequest
  pMediaStreamName_
  pEncodingName_ =
    MediaStreamSourceConfigurationRequest'
      { inputConfigurations =
          Prelude.Nothing,
        mediaStreamName = pMediaStreamName_,
        encodingName = pEncodingName_
      }

-- | The transport parameters that you want to associate with the media
-- stream.
mediaStreamSourceConfigurationRequest_inputConfigurations :: Lens.Lens' MediaStreamSourceConfigurationRequest (Prelude.Maybe [InputConfigurationRequest])
mediaStreamSourceConfigurationRequest_inputConfigurations = Lens.lens (\MediaStreamSourceConfigurationRequest' {inputConfigurations} -> inputConfigurations) (\s@MediaStreamSourceConfigurationRequest' {} a -> s {inputConfigurations = a} :: MediaStreamSourceConfigurationRequest) Prelude.. Lens.mapping Lens.coerced

-- | The name of the media stream.
mediaStreamSourceConfigurationRequest_mediaStreamName :: Lens.Lens' MediaStreamSourceConfigurationRequest Prelude.Text
mediaStreamSourceConfigurationRequest_mediaStreamName = Lens.lens (\MediaStreamSourceConfigurationRequest' {mediaStreamName} -> mediaStreamName) (\s@MediaStreamSourceConfigurationRequest' {} a -> s {mediaStreamName = a} :: MediaStreamSourceConfigurationRequest)

-- | The format you want to use to encode the data. For ancillary data
-- streams, set the encoding name to smpte291. For audio streams, set the
-- encoding name to pcm. For video, 2110 streams, set the encoding name to
-- raw. For video, JPEG XS streams, set the encoding name to jxsv.
mediaStreamSourceConfigurationRequest_encodingName :: Lens.Lens' MediaStreamSourceConfigurationRequest EncodingName
mediaStreamSourceConfigurationRequest_encodingName = Lens.lens (\MediaStreamSourceConfigurationRequest' {encodingName} -> encodingName) (\s@MediaStreamSourceConfigurationRequest' {} a -> s {encodingName = a} :: MediaStreamSourceConfigurationRequest)

instance
  Prelude.Hashable
    MediaStreamSourceConfigurationRequest
  where
  hashWithSalt
    _salt
    MediaStreamSourceConfigurationRequest' {..} =
      _salt
        `Prelude.hashWithSalt` inputConfigurations
        `Prelude.hashWithSalt` mediaStreamName
        `Prelude.hashWithSalt` encodingName

instance
  Prelude.NFData
    MediaStreamSourceConfigurationRequest
  where
  rnf MediaStreamSourceConfigurationRequest' {..} =
    Prelude.rnf inputConfigurations
      `Prelude.seq` Prelude.rnf mediaStreamName
      `Prelude.seq` Prelude.rnf encodingName

instance
  Data.ToJSON
    MediaStreamSourceConfigurationRequest
  where
  toJSON MediaStreamSourceConfigurationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputConfigurations" Data..=)
              Prelude.<$> inputConfigurations,
            Prelude.Just
              ("mediaStreamName" Data..= mediaStreamName),
            Prelude.Just ("encodingName" Data..= encodingName)
          ]
      )
