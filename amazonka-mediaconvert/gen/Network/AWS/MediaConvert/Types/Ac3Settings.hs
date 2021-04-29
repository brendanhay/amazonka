{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.Ac3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3Settings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Ac3BitstreamMode
import Network.AWS.MediaConvert.Types.Ac3CodingMode
import Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Network.AWS.MediaConvert.Types.Ac3LfeFilter
import Network.AWS.MediaConvert.Types.Ac3MetadataControl
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
--
-- /See:/ 'newAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital, dialnorm will be passed through.
    dialnorm :: Prelude.Maybe Prelude.Natural,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Prelude.Maybe Ac3CodingMode,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
    -- Only valid with 3_2_LFE coding mode.
    lfeFilter :: Prelude.Maybe Ac3LfeFilter,
    -- | If set to FILM_STANDARD, adds dynamic range compression signaling to the
    -- output bitstream as defined in the Dolby Digital specification.
    dynamicRangeCompressionProfile :: Prelude.Maybe Ac3DynamicRangeCompressionProfile,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
    -- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Prelude.Maybe Ac3BitstreamMode,
    -- | Specify the average bitrate in bits per second. Valid bitrates depend on
    -- the coding mode.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
    -- DD+, or DolbyE decoder that supplied this audio data. If audio was not
    -- supplied from one of these streams, then the static metadata settings
    -- will be used.
    metadataControl :: Prelude.Maybe Ac3MetadataControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Ac3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dialnorm', 'ac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
--
-- 'codingMode', 'ac3Settings_codingMode' - Dolby Digital coding mode. Determines number of channels.
--
-- 'lfeFilter', 'ac3Settings_lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
--
-- 'dynamicRangeCompressionProfile', 'ac3Settings_dynamicRangeCompressionProfile' - If set to FILM_STANDARD, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
--
-- 'sampleRate', 'ac3Settings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- 'bitstreamMode', 'ac3Settings_bitstreamMode' - Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'bitrate', 'ac3Settings_bitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
--
-- 'metadataControl', 'ac3Settings_metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
newAc3Settings ::
  Ac3Settings
newAc3Settings =
  Ac3Settings'
    { dialnorm = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      lfeFilter = Prelude.Nothing,
      dynamicRangeCompressionProfile = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      bitstreamMode = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      metadataControl = Prelude.Nothing
    }

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
ac3Settings_dialnorm :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_dialnorm = Lens.lens (\Ac3Settings' {dialnorm} -> dialnorm) (\s@Ac3Settings' {} a -> s {dialnorm = a} :: Ac3Settings)

-- | Dolby Digital coding mode. Determines number of channels.
ac3Settings_codingMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3CodingMode)
ac3Settings_codingMode = Lens.lens (\Ac3Settings' {codingMode} -> codingMode) (\s@Ac3Settings' {} a -> s {codingMode = a} :: Ac3Settings)

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
ac3Settings_lfeFilter :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3LfeFilter)
ac3Settings_lfeFilter = Lens.lens (\Ac3Settings' {lfeFilter} -> lfeFilter) (\s@Ac3Settings' {} a -> s {lfeFilter = a} :: Ac3Settings)

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the
-- output bitstream as defined in the Dolby Digital specification.
ac3Settings_dynamicRangeCompressionProfile :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DynamicRangeCompressionProfile)
ac3Settings_dynamicRangeCompressionProfile = Lens.lens (\Ac3Settings' {dynamicRangeCompressionProfile} -> dynamicRangeCompressionProfile) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionProfile = a} :: Ac3Settings)

-- | This value is always 48000. It represents the sample rate in Hz.
ac3Settings_sampleRate :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_sampleRate = Lens.lens (\Ac3Settings' {sampleRate} -> sampleRate) (\s@Ac3Settings' {} a -> s {sampleRate = a} :: Ac3Settings)

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
ac3Settings_bitstreamMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3BitstreamMode)
ac3Settings_bitstreamMode = Lens.lens (\Ac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Ac3Settings' {} a -> s {bitstreamMode = a} :: Ac3Settings)

-- | Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
ac3Settings_bitrate :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_bitrate = Lens.lens (\Ac3Settings' {bitrate} -> bitrate) (\s@Ac3Settings' {} a -> s {bitrate = a} :: Ac3Settings)

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
ac3Settings_metadataControl :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3MetadataControl)
ac3Settings_metadataControl = Lens.lens (\Ac3Settings' {metadataControl} -> metadataControl) (\s@Ac3Settings' {} a -> s {metadataControl = a} :: Ac3Settings)

instance Prelude.FromJSON Ac3Settings where
  parseJSON =
    Prelude.withObject
      "Ac3Settings"
      ( \x ->
          Ac3Settings'
            Prelude.<$> (x Prelude..:? "dialnorm")
            Prelude.<*> (x Prelude..:? "codingMode")
            Prelude.<*> (x Prelude..:? "lfeFilter")
            Prelude.<*> (x Prelude..:? "dynamicRangeCompressionProfile")
            Prelude.<*> (x Prelude..:? "sampleRate")
            Prelude.<*> (x Prelude..:? "bitstreamMode")
            Prelude.<*> (x Prelude..:? "bitrate")
            Prelude.<*> (x Prelude..:? "metadataControl")
      )

instance Prelude.Hashable Ac3Settings

instance Prelude.NFData Ac3Settings

instance Prelude.ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("dialnorm" Prelude..=) Prelude.<$> dialnorm,
            ("codingMode" Prelude..=) Prelude.<$> codingMode,
            ("lfeFilter" Prelude..=) Prelude.<$> lfeFilter,
            ("dynamicRangeCompressionProfile" Prelude..=)
              Prelude.<$> dynamicRangeCompressionProfile,
            ("sampleRate" Prelude..=) Prelude.<$> sampleRate,
            ("bitstreamMode" Prelude..=)
              Prelude.<$> bitstreamMode,
            ("bitrate" Prelude..=) Prelude.<$> bitrate,
            ("metadataControl" Prelude..=)
              Prelude.<$> metadataControl
          ]
      )
