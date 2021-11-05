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
-- Module      : Network.AWS.MediaConnect.Types.MediaStreamOutputConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConnect.Types.MediaStreamOutputConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConnect.Types.DestinationConfiguration
import Network.AWS.MediaConnect.Types.EncodingName
import Network.AWS.MediaConnect.Types.EncodingParameters
import qualified Network.AWS.Prelude as Prelude

-- | The media stream that is associated with the output, and the parameters
-- for that association.
--
-- /See:/ 'newMediaStreamOutputConfiguration' smart constructor.
data MediaStreamOutputConfiguration = MediaStreamOutputConfiguration'
  { -- | The transport parameters that are associated with each outbound media
    -- stream.
    destinationConfigurations :: Prelude.Maybe [DestinationConfiguration],
    -- | Encoding parameters
    encodingParameters :: Prelude.Maybe EncodingParameters,
    -- | The name of the media stream.
    mediaStreamName :: Prelude.Text,
    -- | The format that was used to encode the data. For ancillary data streams,
    -- set the encoding name to smpte291. For audio streams, set the encoding
    -- name to pcm. For video, 2110 streams, set the encoding name to raw. For
    -- video, JPEG XS streams, set the encoding name to jxsv.
    encodingName :: EncodingName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaStreamOutputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationConfigurations', 'mediaStreamOutputConfiguration_destinationConfigurations' - The transport parameters that are associated with each outbound media
-- stream.
--
-- 'encodingParameters', 'mediaStreamOutputConfiguration_encodingParameters' - Encoding parameters
--
-- 'mediaStreamName', 'mediaStreamOutputConfiguration_mediaStreamName' - The name of the media stream.
--
-- 'encodingName', 'mediaStreamOutputConfiguration_encodingName' - The format that was used to encode the data. For ancillary data streams,
-- set the encoding name to smpte291. For audio streams, set the encoding
-- name to pcm. For video, 2110 streams, set the encoding name to raw. For
-- video, JPEG XS streams, set the encoding name to jxsv.
newMediaStreamOutputConfiguration ::
  -- | 'mediaStreamName'
  Prelude.Text ->
  -- | 'encodingName'
  EncodingName ->
  MediaStreamOutputConfiguration
newMediaStreamOutputConfiguration
  pMediaStreamName_
  pEncodingName_ =
    MediaStreamOutputConfiguration'
      { destinationConfigurations =
          Prelude.Nothing,
        encodingParameters = Prelude.Nothing,
        mediaStreamName = pMediaStreamName_,
        encodingName = pEncodingName_
      }

-- | The transport parameters that are associated with each outbound media
-- stream.
mediaStreamOutputConfiguration_destinationConfigurations :: Lens.Lens' MediaStreamOutputConfiguration (Prelude.Maybe [DestinationConfiguration])
mediaStreamOutputConfiguration_destinationConfigurations = Lens.lens (\MediaStreamOutputConfiguration' {destinationConfigurations} -> destinationConfigurations) (\s@MediaStreamOutputConfiguration' {} a -> s {destinationConfigurations = a} :: MediaStreamOutputConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Encoding parameters
mediaStreamOutputConfiguration_encodingParameters :: Lens.Lens' MediaStreamOutputConfiguration (Prelude.Maybe EncodingParameters)
mediaStreamOutputConfiguration_encodingParameters = Lens.lens (\MediaStreamOutputConfiguration' {encodingParameters} -> encodingParameters) (\s@MediaStreamOutputConfiguration' {} a -> s {encodingParameters = a} :: MediaStreamOutputConfiguration)

-- | The name of the media stream.
mediaStreamOutputConfiguration_mediaStreamName :: Lens.Lens' MediaStreamOutputConfiguration Prelude.Text
mediaStreamOutputConfiguration_mediaStreamName = Lens.lens (\MediaStreamOutputConfiguration' {mediaStreamName} -> mediaStreamName) (\s@MediaStreamOutputConfiguration' {} a -> s {mediaStreamName = a} :: MediaStreamOutputConfiguration)

-- | The format that was used to encode the data. For ancillary data streams,
-- set the encoding name to smpte291. For audio streams, set the encoding
-- name to pcm. For video, 2110 streams, set the encoding name to raw. For
-- video, JPEG XS streams, set the encoding name to jxsv.
mediaStreamOutputConfiguration_encodingName :: Lens.Lens' MediaStreamOutputConfiguration EncodingName
mediaStreamOutputConfiguration_encodingName = Lens.lens (\MediaStreamOutputConfiguration' {encodingName} -> encodingName) (\s@MediaStreamOutputConfiguration' {} a -> s {encodingName = a} :: MediaStreamOutputConfiguration)

instance Core.FromJSON MediaStreamOutputConfiguration where
  parseJSON =
    Core.withObject
      "MediaStreamOutputConfiguration"
      ( \x ->
          MediaStreamOutputConfiguration'
            Prelude.<$> ( x Core..:? "destinationConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "encodingParameters")
            Prelude.<*> (x Core..: "mediaStreamName")
            Prelude.<*> (x Core..: "encodingName")
      )

instance
  Prelude.Hashable
    MediaStreamOutputConfiguration

instance
  Prelude.NFData
    MediaStreamOutputConfiguration
