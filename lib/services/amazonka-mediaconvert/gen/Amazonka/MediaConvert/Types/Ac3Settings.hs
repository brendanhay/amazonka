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
-- Module      : Amazonka.MediaConvert.Types.Ac3Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Ac3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Ac3BitstreamMode
import Amazonka.MediaConvert.Types.Ac3CodingMode
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionLine
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
import Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionRf
import Amazonka.MediaConvert.Types.Ac3LfeFilter
import Amazonka.MediaConvert.Types.Ac3MetadataControl
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
--
-- /See:/ 'newAc3Settings' smart constructor.
data Ac3Settings = Ac3Settings'
  { -- | Specify the average bitrate in bits per second. The bitrate that you
    -- specify must be a multiple of 8000 within the allowed minimum and
    -- maximum values. Leave blank to use the default bitrate for the coding
    -- mode you select according ETSI TS 102 366. Valid bitrates for coding
    -- mode 1\/0: Default: 96000. Minimum: 64000. Maximum: 128000. Valid
    -- bitrates for coding mode 1\/1: Default: 192000. Minimum: 128000.
    -- Maximum: 384000. Valid bitrates for coding mode 2\/0: Default: 192000.
    -- Minimum: 128000. Maximum: 384000. Valid bitrates for coding mode 3\/2
    -- with FLE: Default: 384000. Minimum: 384000. Maximum: 640000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
    -- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Prelude.Maybe Ac3BitstreamMode,
    -- | Dolby Digital coding mode. Determines number of channels.
    codingMode :: Prelude.Maybe Ac3CodingMode,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital, dialnorm will be passed through.
    dialnorm :: Prelude.Maybe Prelude.Natural,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the line operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionLine :: Prelude.Maybe Ac3DynamicRangeCompressionLine,
    -- | When you want to add Dolby dynamic range compression (DRC) signaling to
    -- your output stream, we recommend that you use the mode-specific settings
    -- instead of Dynamic range compression profile
    -- (DynamicRangeCompressionProfile). The mode-specific settings are Dynamic
    -- range compression profile, line mode (dynamicRangeCompressionLine) and
    -- Dynamic range compression profile, RF mode (dynamicRangeCompressionRf).
    -- Note that when you specify values for all three settings, MediaConvert
    -- ignores the value of this setting in favor of the mode-specific
    -- settings. If you do use this setting instead of the mode-specific
    -- settings, choose None (NONE) to leave out DRC signaling. Keep the
    -- default Film standard (FILM_STANDARD) to set the profile to Dolby\'s
    -- film standard profile for all operating modes.
    dynamicRangeCompressionProfile :: Prelude.Maybe Ac3DynamicRangeCompressionProfile,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the RF operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionRf :: Prelude.Maybe Ac3DynamicRangeCompressionRf,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
    -- Only valid with 3_2_LFE coding mode.
    lfeFilter :: Prelude.Maybe Ac3LfeFilter,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
    -- DD+, or DolbyE decoder that supplied this audio data. If audio was not
    -- supplied from one of these streams, then the static metadata settings
    -- will be used.
    metadataControl :: Prelude.Maybe Ac3MetadataControl,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ac3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitrate', 'ac3Settings_bitrate' - Specify the average bitrate in bits per second. The bitrate that you
-- specify must be a multiple of 8000 within the allowed minimum and
-- maximum values. Leave blank to use the default bitrate for the coding
-- mode you select according ETSI TS 102 366. Valid bitrates for coding
-- mode 1\/0: Default: 96000. Minimum: 64000. Maximum: 128000. Valid
-- bitrates for coding mode 1\/1: Default: 192000. Minimum: 128000.
-- Maximum: 384000. Valid bitrates for coding mode 2\/0: Default: 192000.
-- Minimum: 128000. Maximum: 384000. Valid bitrates for coding mode 3\/2
-- with FLE: Default: 384000. Minimum: 384000. Maximum: 640000.
--
-- 'bitstreamMode', 'ac3Settings_bitstreamMode' - Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'codingMode', 'ac3Settings_codingMode' - Dolby Digital coding mode. Determines number of channels.
--
-- 'dialnorm', 'ac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
--
-- 'dynamicRangeCompressionLine', 'ac3Settings_dynamicRangeCompressionLine' - Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the line operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
--
-- 'dynamicRangeCompressionProfile', 'ac3Settings_dynamicRangeCompressionProfile' - When you want to add Dolby dynamic range compression (DRC) signaling to
-- your output stream, we recommend that you use the mode-specific settings
-- instead of Dynamic range compression profile
-- (DynamicRangeCompressionProfile). The mode-specific settings are Dynamic
-- range compression profile, line mode (dynamicRangeCompressionLine) and
-- Dynamic range compression profile, RF mode (dynamicRangeCompressionRf).
-- Note that when you specify values for all three settings, MediaConvert
-- ignores the value of this setting in favor of the mode-specific
-- settings. If you do use this setting instead of the mode-specific
-- settings, choose None (NONE) to leave out DRC signaling. Keep the
-- default Film standard (FILM_STANDARD) to set the profile to Dolby\'s
-- film standard profile for all operating modes.
--
-- 'dynamicRangeCompressionRf', 'ac3Settings_dynamicRangeCompressionRf' - Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the RF operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
--
-- 'lfeFilter', 'ac3Settings_lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
--
-- 'metadataControl', 'ac3Settings_metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
--
-- 'sampleRate', 'ac3Settings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
newAc3Settings ::
  Ac3Settings
newAc3Settings =
  Ac3Settings'
    { bitrate = Prelude.Nothing,
      bitstreamMode = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      dialnorm = Prelude.Nothing,
      dynamicRangeCompressionLine = Prelude.Nothing,
      dynamicRangeCompressionProfile = Prelude.Nothing,
      dynamicRangeCompressionRf = Prelude.Nothing,
      lfeFilter = Prelude.Nothing,
      metadataControl = Prelude.Nothing,
      sampleRate = Prelude.Nothing
    }

-- | Specify the average bitrate in bits per second. The bitrate that you
-- specify must be a multiple of 8000 within the allowed minimum and
-- maximum values. Leave blank to use the default bitrate for the coding
-- mode you select according ETSI TS 102 366. Valid bitrates for coding
-- mode 1\/0: Default: 96000. Minimum: 64000. Maximum: 128000. Valid
-- bitrates for coding mode 1\/1: Default: 192000. Minimum: 128000.
-- Maximum: 384000. Valid bitrates for coding mode 2\/0: Default: 192000.
-- Minimum: 128000. Maximum: 384000. Valid bitrates for coding mode 3\/2
-- with FLE: Default: 384000. Minimum: 384000. Maximum: 640000.
ac3Settings_bitrate :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_bitrate = Lens.lens (\Ac3Settings' {bitrate} -> bitrate) (\s@Ac3Settings' {} a -> s {bitrate = a} :: Ac3Settings)

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits.
-- For more information about the AC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
ac3Settings_bitstreamMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3BitstreamMode)
ac3Settings_bitstreamMode = Lens.lens (\Ac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Ac3Settings' {} a -> s {bitstreamMode = a} :: Ac3Settings)

-- | Dolby Digital coding mode. Determines number of channels.
ac3Settings_codingMode :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3CodingMode)
ac3Settings_codingMode = Lens.lens (\Ac3Settings' {codingMode} -> codingMode) (\s@Ac3Settings' {} a -> s {codingMode = a} :: Ac3Settings)

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital, dialnorm will be passed through.
ac3Settings_dialnorm :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_dialnorm = Lens.lens (\Ac3Settings' {dialnorm} -> dialnorm) (\s@Ac3Settings' {} a -> s {dialnorm = a} :: Ac3Settings)

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the line operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
ac3Settings_dynamicRangeCompressionLine :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DynamicRangeCompressionLine)
ac3Settings_dynamicRangeCompressionLine = Lens.lens (\Ac3Settings' {dynamicRangeCompressionLine} -> dynamicRangeCompressionLine) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionLine = a} :: Ac3Settings)

-- | When you want to add Dolby dynamic range compression (DRC) signaling to
-- your output stream, we recommend that you use the mode-specific settings
-- instead of Dynamic range compression profile
-- (DynamicRangeCompressionProfile). The mode-specific settings are Dynamic
-- range compression profile, line mode (dynamicRangeCompressionLine) and
-- Dynamic range compression profile, RF mode (dynamicRangeCompressionRf).
-- Note that when you specify values for all three settings, MediaConvert
-- ignores the value of this setting in favor of the mode-specific
-- settings. If you do use this setting instead of the mode-specific
-- settings, choose None (NONE) to leave out DRC signaling. Keep the
-- default Film standard (FILM_STANDARD) to set the profile to Dolby\'s
-- film standard profile for all operating modes.
ac3Settings_dynamicRangeCompressionProfile :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DynamicRangeCompressionProfile)
ac3Settings_dynamicRangeCompressionProfile = Lens.lens (\Ac3Settings' {dynamicRangeCompressionProfile} -> dynamicRangeCompressionProfile) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionProfile = a} :: Ac3Settings)

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the RF operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
ac3Settings_dynamicRangeCompressionRf :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3DynamicRangeCompressionRf)
ac3Settings_dynamicRangeCompressionRf = Lens.lens (\Ac3Settings' {dynamicRangeCompressionRf} -> dynamicRangeCompressionRf) (\s@Ac3Settings' {} a -> s {dynamicRangeCompressionRf = a} :: Ac3Settings)

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
ac3Settings_lfeFilter :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3LfeFilter)
ac3Settings_lfeFilter = Lens.lens (\Ac3Settings' {lfeFilter} -> lfeFilter) (\s@Ac3Settings' {} a -> s {lfeFilter = a} :: Ac3Settings)

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
ac3Settings_metadataControl :: Lens.Lens' Ac3Settings (Prelude.Maybe Ac3MetadataControl)
ac3Settings_metadataControl = Lens.lens (\Ac3Settings' {metadataControl} -> metadataControl) (\s@Ac3Settings' {} a -> s {metadataControl = a} :: Ac3Settings)

-- | This value is always 48000. It represents the sample rate in Hz.
ac3Settings_sampleRate :: Lens.Lens' Ac3Settings (Prelude.Maybe Prelude.Natural)
ac3Settings_sampleRate = Lens.lens (\Ac3Settings' {sampleRate} -> sampleRate) (\s@Ac3Settings' {} a -> s {sampleRate = a} :: Ac3Settings)

instance Data.FromJSON Ac3Settings where
  parseJSON =
    Data.withObject
      "Ac3Settings"
      ( \x ->
          Ac3Settings'
            Prelude.<$> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "bitstreamMode")
            Prelude.<*> (x Data..:? "codingMode")
            Prelude.<*> (x Data..:? "dialnorm")
            Prelude.<*> (x Data..:? "dynamicRangeCompressionLine")
            Prelude.<*> (x Data..:? "dynamicRangeCompressionProfile")
            Prelude.<*> (x Data..:? "dynamicRangeCompressionRf")
            Prelude.<*> (x Data..:? "lfeFilter")
            Prelude.<*> (x Data..:? "metadataControl")
            Prelude.<*> (x Data..:? "sampleRate")
      )

instance Prelude.Hashable Ac3Settings where
  hashWithSalt _salt Ac3Settings' {..} =
    _salt
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` bitstreamMode
      `Prelude.hashWithSalt` codingMode
      `Prelude.hashWithSalt` dialnorm
      `Prelude.hashWithSalt` dynamicRangeCompressionLine
      `Prelude.hashWithSalt` dynamicRangeCompressionProfile
      `Prelude.hashWithSalt` dynamicRangeCompressionRf
      `Prelude.hashWithSalt` lfeFilter
      `Prelude.hashWithSalt` metadataControl
      `Prelude.hashWithSalt` sampleRate

instance Prelude.NFData Ac3Settings where
  rnf Ac3Settings' {..} =
    Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf bitstreamMode
      `Prelude.seq` Prelude.rnf codingMode
      `Prelude.seq` Prelude.rnf dialnorm
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionLine
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionProfile
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionRf
      `Prelude.seq` Prelude.rnf lfeFilter
      `Prelude.seq` Prelude.rnf metadataControl
      `Prelude.seq` Prelude.rnf sampleRate

instance Data.ToJSON Ac3Settings where
  toJSON Ac3Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bitrate" Data..=) Prelude.<$> bitrate,
            ("bitstreamMode" Data..=) Prelude.<$> bitstreamMode,
            ("codingMode" Data..=) Prelude.<$> codingMode,
            ("dialnorm" Data..=) Prelude.<$> dialnorm,
            ("dynamicRangeCompressionLine" Data..=)
              Prelude.<$> dynamicRangeCompressionLine,
            ("dynamicRangeCompressionProfile" Data..=)
              Prelude.<$> dynamicRangeCompressionProfile,
            ("dynamicRangeCompressionRf" Data..=)
              Prelude.<$> dynamicRangeCompressionRf,
            ("lfeFilter" Data..=) Prelude.<$> lfeFilter,
            ("metadataControl" Data..=)
              Prelude.<$> metadataControl,
            ("sampleRate" Data..=) Prelude.<$> sampleRate
          ]
      )
