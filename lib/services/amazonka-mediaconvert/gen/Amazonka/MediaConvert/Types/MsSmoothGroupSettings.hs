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
-- Module      : Amazonka.MediaConvert.Types.MsSmoothGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MsSmoothGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.DestinationSettings
import Amazonka.MediaConvert.Types.MsSmoothAdditionalManifest
import Amazonka.MediaConvert.Types.MsSmoothAudioDeduplication
import Amazonka.MediaConvert.Types.MsSmoothEncryptionSettings
import Amazonka.MediaConvert.Types.MsSmoothFragmentLengthControl
import Amazonka.MediaConvert.Types.MsSmoothManifestEncoding
import qualified Amazonka.Prelude as Prelude

-- | Settings related to your Microsoft Smooth Streaming output package. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to MS_SMOOTH_GROUP_SETTINGS.
--
-- /See:/ 'newMsSmoothGroupSettings' smart constructor.
data MsSmoothGroupSettings = MsSmoothGroupSettings'
  { -- | By default, the service creates one .ism Microsoft Smooth Streaming
    -- manifest for each Microsoft Smooth Streaming output group in your job.
    -- This default manifest references every output in the output group. To
    -- create additional manifests that reference a subset of the outputs in
    -- the output group, specify a list of them here.
    additionalManifests :: Prelude.Maybe [MsSmoothAdditionalManifest],
    -- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
    -- across a Microsoft Smooth output group into a single audio stream.
    audioDeduplication :: Prelude.Maybe MsSmoothAudioDeduplication,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Prelude.Maybe DestinationSettings,
    -- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
    -- specify the value SpekeKeyProvider.
    encryption :: Prelude.Maybe MsSmoothEncryptionSettings,
    -- | Specify how you want MediaConvert to determine the fragment length.
    -- Choose Exact (EXACT) to have the encoder use the exact length that you
    -- specify with the setting Fragment length (FragmentLength). This might
    -- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
    -- the encoder round up the segment lengths to match the next GOP boundary.
    fragmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Specify how you want MediaConvert to determine the fragment length.
    -- Choose Exact (EXACT) to have the encoder use the exact length that you
    -- specify with the setting Fragment length (FragmentLength). This might
    -- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
    -- the encoder round up the segment lengths to match the next GOP boundary.
    fragmentLengthControl :: Prelude.Maybe MsSmoothFragmentLengthControl,
    -- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
    -- format for the server and client manifest. Valid options are utf8 and
    -- utf16.
    manifestEncoding :: Prelude.Maybe MsSmoothManifestEncoding
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
-- 'additionalManifests', 'msSmoothGroupSettings_additionalManifests' - By default, the service creates one .ism Microsoft Smooth Streaming
-- manifest for each Microsoft Smooth Streaming output group in your job.
-- This default manifest references every output in the output group. To
-- create additional manifests that reference a subset of the outputs in
-- the output group, specify a list of them here.
--
-- 'audioDeduplication', 'msSmoothGroupSettings_audioDeduplication' - COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
-- across a Microsoft Smooth output group into a single audio stream.
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
-- 'encryption', 'msSmoothGroupSettings_encryption' - If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
-- specify the value SpekeKeyProvider.
--
-- 'fragmentLength', 'msSmoothGroupSettings_fragmentLength' - Specify how you want MediaConvert to determine the fragment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Fragment length (FragmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
--
-- 'fragmentLengthControl', 'msSmoothGroupSettings_fragmentLengthControl' - Specify how you want MediaConvert to determine the fragment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Fragment length (FragmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
--
-- 'manifestEncoding', 'msSmoothGroupSettings_manifestEncoding' - Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
-- format for the server and client manifest. Valid options are utf8 and
-- utf16.
newMsSmoothGroupSettings ::
  MsSmoothGroupSettings
newMsSmoothGroupSettings =
  MsSmoothGroupSettings'
    { additionalManifests =
        Prelude.Nothing,
      audioDeduplication = Prelude.Nothing,
      destination = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      encryption = Prelude.Nothing,
      fragmentLength = Prelude.Nothing,
      fragmentLengthControl = Prelude.Nothing,
      manifestEncoding = Prelude.Nothing
    }

-- | By default, the service creates one .ism Microsoft Smooth Streaming
-- manifest for each Microsoft Smooth Streaming output group in your job.
-- This default manifest references every output in the output group. To
-- create additional manifests that reference a subset of the outputs in
-- the output group, specify a list of them here.
msSmoothGroupSettings_additionalManifests :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe [MsSmoothAdditionalManifest])
msSmoothGroupSettings_additionalManifests = Lens.lens (\MsSmoothGroupSettings' {additionalManifests} -> additionalManifests) (\s@MsSmoothGroupSettings' {} a -> s {additionalManifests = a} :: MsSmoothGroupSettings) Prelude.. Lens.mapping Lens.coerced

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
-- across a Microsoft Smooth output group into a single audio stream.
msSmoothGroupSettings_audioDeduplication :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe MsSmoothAudioDeduplication)
msSmoothGroupSettings_audioDeduplication = Lens.lens (\MsSmoothGroupSettings' {audioDeduplication} -> audioDeduplication) (\s@MsSmoothGroupSettings' {} a -> s {audioDeduplication = a} :: MsSmoothGroupSettings)

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

-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to
-- specify the value SpekeKeyProvider.
msSmoothGroupSettings_encryption :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe MsSmoothEncryptionSettings)
msSmoothGroupSettings_encryption = Lens.lens (\MsSmoothGroupSettings' {encryption} -> encryption) (\s@MsSmoothGroupSettings' {} a -> s {encryption = a} :: MsSmoothGroupSettings)

-- | Specify how you want MediaConvert to determine the fragment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Fragment length (FragmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
msSmoothGroupSettings_fragmentLength :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe Prelude.Natural)
msSmoothGroupSettings_fragmentLength = Lens.lens (\MsSmoothGroupSettings' {fragmentLength} -> fragmentLength) (\s@MsSmoothGroupSettings' {} a -> s {fragmentLength = a} :: MsSmoothGroupSettings)

-- | Specify how you want MediaConvert to determine the fragment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Fragment length (FragmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
msSmoothGroupSettings_fragmentLengthControl :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe MsSmoothFragmentLengthControl)
msSmoothGroupSettings_fragmentLengthControl = Lens.lens (\MsSmoothGroupSettings' {fragmentLengthControl} -> fragmentLengthControl) (\s@MsSmoothGroupSettings' {} a -> s {fragmentLengthControl = a} :: MsSmoothGroupSettings)

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
-- format for the server and client manifest. Valid options are utf8 and
-- utf16.
msSmoothGroupSettings_manifestEncoding :: Lens.Lens' MsSmoothGroupSettings (Prelude.Maybe MsSmoothManifestEncoding)
msSmoothGroupSettings_manifestEncoding = Lens.lens (\MsSmoothGroupSettings' {manifestEncoding} -> manifestEncoding) (\s@MsSmoothGroupSettings' {} a -> s {manifestEncoding = a} :: MsSmoothGroupSettings)

instance Data.FromJSON MsSmoothGroupSettings where
  parseJSON =
    Data.withObject
      "MsSmoothGroupSettings"
      ( \x ->
          MsSmoothGroupSettings'
            Prelude.<$> ( x
                            Data..:? "additionalManifests"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "audioDeduplication")
            Prelude.<*> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "destinationSettings")
            Prelude.<*> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "fragmentLength")
            Prelude.<*> (x Data..:? "fragmentLengthControl")
            Prelude.<*> (x Data..:? "manifestEncoding")
      )

instance Prelude.Hashable MsSmoothGroupSettings where
  hashWithSalt _salt MsSmoothGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` additionalManifests
      `Prelude.hashWithSalt` audioDeduplication
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationSettings
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` fragmentLength
      `Prelude.hashWithSalt` fragmentLengthControl
      `Prelude.hashWithSalt` manifestEncoding

instance Prelude.NFData MsSmoothGroupSettings where
  rnf MsSmoothGroupSettings' {..} =
    Prelude.rnf additionalManifests
      `Prelude.seq` Prelude.rnf audioDeduplication
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationSettings
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf fragmentLength
      `Prelude.seq` Prelude.rnf fragmentLengthControl
      `Prelude.seq` Prelude.rnf manifestEncoding

instance Data.ToJSON MsSmoothGroupSettings where
  toJSON MsSmoothGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalManifests" Data..=)
              Prelude.<$> additionalManifests,
            ("audioDeduplication" Data..=)
              Prelude.<$> audioDeduplication,
            ("destination" Data..=) Prelude.<$> destination,
            ("destinationSettings" Data..=)
              Prelude.<$> destinationSettings,
            ("encryption" Data..=) Prelude.<$> encryption,
            ("fragmentLength" Data..=)
              Prelude.<$> fragmentLength,
            ("fragmentLengthControl" Data..=)
              Prelude.<$> fragmentLengthControl,
            ("manifestEncoding" Data..=)
              Prelude.<$> manifestEncoding
          ]
      )
