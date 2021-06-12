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

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to MS_SMOOTH_GROUP_SETTINGS.
--
-- /See:/ 'newMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { -- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
    -- format for the server and client manifest. Valid options are utf8 and
    -- utf16.
    manifestEncoding :: Core.Maybe MsSmoothManifestEncoding,
    -- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes
    -- in seconds. Fragment length must be compatible with GOP size and frame
    -- rate.
    fragmentLength :: Core.Maybe Core.Natural,
    -- | By default, the service creates one .ism Microsoft Smooth Streaming
    -- manifest for each Microsoft Smooth Streaming output group in your job.
    -- This default manifest references every output in the output group. To
    -- create additional manifests that reference a subset of the outputs in
    -- the output group, specify a list of them here.
    additionalManifests :: Core.Maybe [MsSmoothAdditionalManifest],
    -- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
    -- specify the value SpekeKeyProvider.
    encryption :: Core.Maybe MsSmoothEncryptionSettings,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Core.Maybe Core.Text,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Core.Maybe DestinationSettings,
    -- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
    -- across a Microsoft Smooth output group into a single audio stream.
    audioDeduplication :: Core.Maybe MsSmoothAudioDeduplication
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      fragmentLength = Core.Nothing,
      additionalManifests = Core.Nothing,
      encryption = Core.Nothing,
      destination = Core.Nothing,
      destinationSettings = Core.Nothing,
      audioDeduplication = Core.Nothing
    }

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
-- format for the server and client manifest. Valid options are utf8 and
-- utf16.
msSmoothGroupSettings_manifestEncoding :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe MsSmoothManifestEncoding)
msSmoothGroupSettings_manifestEncoding = Lens.lens (\MsSmoothGroupSettings' {manifestEncoding} -> manifestEncoding) (\s@MsSmoothGroupSettings' {} a -> s {manifestEncoding = a} :: MsSmoothGroupSettings)

-- | Use Fragment length (FragmentLength) to specify the mp4 fragment sizes
-- in seconds. Fragment length must be compatible with GOP size and frame
-- rate.
msSmoothGroupSettings_fragmentLength :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Natural)
msSmoothGroupSettings_fragmentLength = Lens.lens (\MsSmoothGroupSettings' {fragmentLength} -> fragmentLength) (\s@MsSmoothGroupSettings' {} a -> s {fragmentLength = a} :: MsSmoothGroupSettings)

-- | By default, the service creates one .ism Microsoft Smooth Streaming
-- manifest for each Microsoft Smooth Streaming output group in your job.
-- This default manifest references every output in the output group. To
-- create additional manifests that reference a subset of the outputs in
-- the output group, specify a list of them here.
msSmoothGroupSettings_additionalManifests :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe [MsSmoothAdditionalManifest])
msSmoothGroupSettings_additionalManifests = Lens.lens (\MsSmoothGroupSettings' {additionalManifests} -> additionalManifests) (\s@MsSmoothGroupSettings' {} a -> s {additionalManifests = a} :: MsSmoothGroupSettings) Core.. Lens.mapping Lens._Coerce

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
-- specify the value SpekeKeyProvider.
msSmoothGroupSettings_encryption :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe MsSmoothEncryptionSettings)
msSmoothGroupSettings_encryption = Lens.lens (\MsSmoothGroupSettings' {encryption} -> encryption) (\s@MsSmoothGroupSettings' {} a -> s {encryption = a} :: MsSmoothGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
msSmoothGroupSettings_destination :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe Core.Text)
msSmoothGroupSettings_destination = Lens.lens (\MsSmoothGroupSettings' {destination} -> destination) (\s@MsSmoothGroupSettings' {} a -> s {destination = a} :: MsSmoothGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
msSmoothGroupSettings_destinationSettings :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe DestinationSettings)
msSmoothGroupSettings_destinationSettings = Lens.lens (\MsSmoothGroupSettings' {destinationSettings} -> destinationSettings) (\s@MsSmoothGroupSettings' {} a -> s {destinationSettings = a} :: MsSmoothGroupSettings)

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
-- across a Microsoft Smooth output group into a single audio stream.
msSmoothGroupSettings_audioDeduplication :: Lens.Lens' MsSmoothGroupSettings (Core.Maybe MsSmoothAudioDeduplication)
msSmoothGroupSettings_audioDeduplication = Lens.lens (\MsSmoothGroupSettings' {audioDeduplication} -> audioDeduplication) (\s@MsSmoothGroupSettings' {} a -> s {audioDeduplication = a} :: MsSmoothGroupSettings)

instance Core.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Core.withObject
      "MsSmoothGroupSettings"
      ( \x ->
          MsSmoothGroupSettings'
            Core.<$> (x Core..:? "manifestEncoding")
            Core.<*> (x Core..:? "fragmentLength")
            Core.<*> ( x Core..:? "additionalManifests"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "encryption")
            Core.<*> (x Core..:? "destination")
            Core.<*> (x Core..:? "destinationSettings")
            Core.<*> (x Core..:? "audioDeduplication")
      )

instance Core.Hashable MsSmoothGroupSettings

instance Core.NFData MsSmoothGroupSettings

instance Core.ToJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("manifestEncoding" Core..=)
              Core.<$> manifestEncoding,
            ("fragmentLength" Core..=) Core.<$> fragmentLength,
            ("additionalManifests" Core..=)
              Core.<$> additionalManifests,
            ("encryption" Core..=) Core.<$> encryption,
            ("destination" Core..=) Core.<$> destination,
            ("destinationSettings" Core..=)
              Core.<$> destinationSettings,
            ("audioDeduplication" Core..=)
              Core.<$> audioDeduplication
          ]
      )
