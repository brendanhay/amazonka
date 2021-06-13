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
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DestinationSettings
import Network.AWS.MediaConvert.Types.MsSmoothAdditionalManifest
import Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
import Network.AWS.MediaConvert.Types.MsSmoothEncryptionSettings
import Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to MS_SMOOTH_GROUP_SETTINGS.
--
-- /See:/ 'newMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { -- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
    -- format for the server and client manifest. Valid options are utf8 and
    -- utf16.
    manifestEncoding :: Prelude.Maybe MsSmoothManifestEncoding,
    -- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes
    -- in seconds. Fragment length must be compatible with GOP size and frame
    -- rate.
    fragmentLength :: Prelude.Maybe Prelude.Natural,
    -- | By default, the service creates one .ism Microsoft Smooth Streaming
    -- manifest for each Microsoft Smooth Streaming output group in your job.
    -- This default manifest references every output in the output group. To
    -- create additional manifests that reference a subset of the outputs in
    -- the output group, specify a list of them here.
    additionalManifests :: Prelude.Maybe [MsSmoothAdditionalManifest],
    -- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
    -- specify the value SpekeKeyProvider.
    encryption :: Prelude.Maybe MsSmoothEncryptionSettings,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Prelude.Maybe DestinationSettings,
    -- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
    -- across a Microsoft Smooth output group into a single audio stream.
    audioDeduplication :: Prelude.Maybe MsSmoothAudioDeduplication
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MsSmoothGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestEncoding', 'msSmoothGroupSettings_manifestEncoding' - Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
-- format for the server and client manifest. Valid options are utf8 and
-- utf16.
--
-- 'fragmentLength', 'msSmoothGroupSettings_fragmentLength' - Use Fragment length (FragmentLength) to specify the mp4 fragment sizes
-- in seconds. Fragment length must be compatible with GOP size and frame
-- rate.
--
-- 'additionalManifests', 'msSmoothGroupSettings_additionalManifests' - By default, the service creates one .ism Microsoft Smooth Streaming
-- manifest for each Microsoft Smooth Streaming output group in your job.
-- This default manifest references every output in the output group. To
-- create additional manifests that reference a subset of the outputs in
-- the output group, specify a list of them here.
--
-- 'encryption', 'msSmoothGroupSettings_encryption' - If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
-- specify the value SpekeKeyProvider.
--
-- 'destination', 'msSmoothGroupSettings_destination' - Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
--
-- 'destinationSettings', 'msSmoothGroupSettings_destinationSettings' - Settings associated with the destination. Will vary based on the type of
-- destination
--
-- 'audioDeduplication', 'msSmoothGroupSettings_audioDeduplication' - COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
-- across a Microsoft Smooth output group into a single audio stream.
newMsSmoothGroupSettings ::
  MsSmoothGroupSettings
newMsSmoothGroupSettings =
  MsSmoothGroupSettings'
    { manifestEncoding =
        Prelude.Nothing,
      fragmentLength = Prelude.Nothing,
      additionalManifests = Prelude.Nothing,
      encryption = Prelude.Nothing,
      destination = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      audioDeduplication = Prelude.Nothing
    }

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
-- format for the server and client manifest. Valid options are utf8 and
-- utf16.
msSmoothGroupSettings_manifestEncoding :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe MsSmoothManifestEncoding)
msSmoothGroupSettings_manifestEncoding = Lens.lens (\MsSmoothGroupSettings' {manifestEncoding} -> manifestEncoding) (\s@MsSmoothGroupSettings' {} a -> s {manifestEncoding = a} :: MsSmoothGroupSettings)

-- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes
-- in seconds. Fragment length must be compatible with GOP size and frame
-- rate.
msSmoothGroupSettings_fragmentLength :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_fragmentLength = Lens.lens (\MsSmoothGroupSettings' {fragmentLength} -> fragmentLength) (\s@MsSmoothGroupSettings' {} a -> s {fragmentLength = a} :: MsSmoothGroupSettings)

-- | By default, the service creates one .ism Microsoft Smooth Streaming
-- manifest for each Microsoft Smooth Streaming output group in your job.
-- This default manifest references every output in the output group. To
-- create additional manifests that reference a subset of the outputs in
-- the output group, specify a list of them here.
msSmoothGroupSettings_additionalManifests :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe [MsSmoothAdditionalManifest])
msSmoothGroupSettings_additionalManifests = Lens.lens (\MsSmoothGroupSettings' {additionalManifests} -> additionalManifests) (\s@MsSmoothGroupSettings' {} a -> s {additionalManifests = a} :: MsSmoothGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
-- specify the value SpekeKeyProvider.
msSmoothGroupSettings_encryption :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe MsSmoothEncryptionSettings)
msSmoothGroupSettings_encryption = Lens.lens (\MsSmoothGroupSettings' {encryption} -> encryption) (\s@MsSmoothGroupSettings' {} a -> s {encryption = a} :: MsSmoothGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
msSmoothGroupSettings_destination :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Text)
msSmoothGroupSettings_destination = Lens.lens (\MsSmoothGroupSettings' {destination} -> destination) (\s@MsSmoothGroupSettings' {} a -> s {destination = a} :: MsSmoothGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
msSmoothGroupSettings_destinationSettings :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe DestinationSettings)
msSmoothGroupSettings_destinationSettings = Lens.lens (\MsSmoothGroupSettings' {destinationSettings} -> destinationSettings) (\s@MsSmoothGroupSettings' {} a -> s {destinationSettings = a} :: MsSmoothGroupSettings)

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
-- across a Microsoft Smooth output group into a single audio stream.
msSmoothGroupSettings_audioDeduplication :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe MsSmoothAudioDeduplication)
msSmoothGroupSettings_audioDeduplication = Lens.lens (\MsSmoothGroupSettings' {audioDeduplication} -> audioDeduplication) (\s@MsSmoothGroupSettings' {} a -> s {audioDeduplication = a} :: MsSmoothGroupSettings)

instance Core.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Core.withObject
      "MsSmoothGroupSettings"
      ( \x ->
          MsSmoothGroupSettings'
            Prelude.<$> (x Core..:? "manifestEncoding")
            Prelude.<*> (x Core..:? "fragmentLength")
            Prelude.<*> ( x Core..:? "additionalManifests"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "destinationSettings")
            Prelude.<*> (x Core..:? "audioDeduplication")
      )

instance Prelude.Hashable MsSmoothGroupSettings

instance Prelude.NFData MsSmoothGroupSettings

instance Core.ToJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("manifestEncoding" Core..=)
              Prelude.<$> manifestEncoding,
            ("fragmentLength" Core..=)
              Prelude.<$> fragmentLength,
            ("additionalManifests" Core..=)
              Prelude.<$> additionalManifests,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("destination" Core..=) Prelude.<$> destination,
            ("destinationSettings" Core..=)
              Prelude.<$> destinationSettings,
            ("audioDeduplication" Core..=)
              Prelude.<$> audioDeduplication
          ]
      )
