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
-- Module      : Network.AWS.MediaConnect.Types.MediaStreamOutputConfigurationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConnect.Types.MediaStreamOutputConfigurationRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConnect.Types.DestinationConfigurationRequest
import Network.AWS.MediaConnect.Types.EncodingName
import Network.AWS.MediaConnect.Types.EncodingParametersRequest
import qualified Network.AWS.Prelude as Prelude

-- | The media stream that you want to associate with the output, and the
-- parameters for that association.
--
-- /See:/ 'newMediaStreamOutputConfigurationRequest' smart constructor.
data MediaStreamOutputConfigurationRequest = MediaStreamOutputConfigurationRequest'
  { -- | The transport parameters that you want to associate with the media
    -- stream.
    destinationConfigurations :: Prelude.Maybe [DestinationConfigurationRequest],
    -- | A collection of parameters that determine how MediaConnect will convert
    -- the content. These fields only apply to outputs on flows that have a CDI
    -- source.
    encodingParameters :: Prelude.Maybe EncodingParametersRequest,
    -- | The name of the media stream that is associated with the output.
    mediaStreamName :: Prelude.Text,
    -- | The format that will be used to encode the data. For ancillary data
    -- streams, set the encoding name to smpte291. For audio streams, set the
    -- encoding name to pcm. For video, 2110 streams, set the encoding name to
    -- raw. For video, JPEG XS streams, set the encoding name to jxsv.
    encodingName :: EncodingName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaStreamOutputConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationConfigurations', 'mediaStreamOutputConfigurationRequest_destinationConfigurations' - The transport parameters that you want to associate with the media
-- stream.
--
-- 'encodingParameters', 'mediaStreamOutputConfigurationRequest_encodingParameters' - A collection of parameters that determine how MediaConnect will convert
-- the content. These fields only apply to outputs on flows that have a CDI
-- source.
--
-- 'mediaStreamName', 'mediaStreamOutputConfigurationRequest_mediaStreamName' - The name of the media stream that is associated with the output.
--
-- 'encodingName', 'mediaStreamOutputConfigurationRequest_encodingName' - The format that will be used to encode the data. For ancillary data
-- streams, set the encoding name to smpte291. For audio streams, set the
-- encoding name to pcm. For video, 2110 streams, set the encoding name to
-- raw. For video, JPEG XS streams, set the encoding name to jxsv.
newMediaStreamOutputConfigurationRequest ::
  -- | 'mediaStreamName'
  Prelude.Text ->
  -- | 'encodingName'
  EncodingName ->
  MediaStreamOutputConfigurationRequest
newMediaStreamOutputConfigurationRequest
  pMediaStreamName_
  pEncodingName_ =
    MediaStreamOutputConfigurationRequest'
      { destinationConfigurations =
          Prelude.Nothing,
        encodingParameters = Prelude.Nothing,
        mediaStreamName = pMediaStreamName_,
        encodingName = pEncodingName_
      }

-- | The transport parameters that you want to associate with the media
-- stream.
mediaStreamOutputConfigurationRequest_destinationConfigurations :: Lens.Lens' MediaStreamOutputConfigurationRequest (Prelude.Maybe [DestinationConfigurationRequest])
mediaStreamOutputConfigurationRequest_destinationConfigurations = Lens.lens (\MediaStreamOutputConfigurationRequest' {destinationConfigurations} -> destinationConfigurations) (\s@MediaStreamOutputConfigurationRequest' {} a -> s {destinationConfigurations = a} :: MediaStreamOutputConfigurationRequest) Prelude.. Lens.mapping Lens.coerced

-- | A collection of parameters that determine how MediaConnect will convert
-- the content. These fields only apply to outputs on flows that have a CDI
-- source.
mediaStreamOutputConfigurationRequest_encodingParameters :: Lens.Lens' MediaStreamOutputConfigurationRequest (Prelude.Maybe EncodingParametersRequest)
mediaStreamOutputConfigurationRequest_encodingParameters = Lens.lens (\MediaStreamOutputConfigurationRequest' {encodingParameters} -> encodingParameters) (\s@MediaStreamOutputConfigurationRequest' {} a -> s {encodingParameters = a} :: MediaStreamOutputConfigurationRequest)

-- | The name of the media stream that is associated with the output.
mediaStreamOutputConfigurationRequest_mediaStreamName :: Lens.Lens' MediaStreamOutputConfigurationRequest Prelude.Text
mediaStreamOutputConfigurationRequest_mediaStreamName = Lens.lens (\MediaStreamOutputConfigurationRequest' {mediaStreamName} -> mediaStreamName) (\s@MediaStreamOutputConfigurationRequest' {} a -> s {mediaStreamName = a} :: MediaStreamOutputConfigurationRequest)

-- | The format that will be used to encode the data. For ancillary data
-- streams, set the encoding name to smpte291. For audio streams, set the
-- encoding name to pcm. For video, 2110 streams, set the encoding name to
-- raw. For video, JPEG XS streams, set the encoding name to jxsv.
mediaStreamOutputConfigurationRequest_encodingName :: Lens.Lens' MediaStreamOutputConfigurationRequest EncodingName
mediaStreamOutputConfigurationRequest_encodingName = Lens.lens (\MediaStreamOutputConfigurationRequest' {encodingName} -> encodingName) (\s@MediaStreamOutputConfigurationRequest' {} a -> s {encodingName = a} :: MediaStreamOutputConfigurationRequest)

instance
  Prelude.Hashable
    MediaStreamOutputConfigurationRequest

instance
  Prelude.NFData
    MediaStreamOutputConfigurationRequest

instance
  Core.ToJSON
    MediaStreamOutputConfigurationRequest
  where
  toJSON MediaStreamOutputConfigurationRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("destinationConfigurations" Core..=)
              Prelude.<$> destinationConfigurations,
            ("encodingParameters" Core..=)
              Prelude.<$> encodingParameters,
            Prelude.Just
              ("mediaStreamName" Core..= mediaStreamName),
            Prelude.Just ("encodingName" Core..= encodingName)
          ]
      )
