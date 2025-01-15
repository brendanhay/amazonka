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
-- Module      : Amazonka.MediaConvert.Types.Eac3Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Eac3AttenuationControl
import Amazonka.MediaConvert.Types.Eac3BitstreamMode
import Amazonka.MediaConvert.Types.Eac3CodingMode
import Amazonka.MediaConvert.Types.Eac3DcFilter
import Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionLine
import Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionRf
import Amazonka.MediaConvert.Types.Eac3LfeControl
import Amazonka.MediaConvert.Types.Eac3LfeFilter
import Amazonka.MediaConvert.Types.Eac3MetadataControl
import Amazonka.MediaConvert.Types.Eac3PassthroughControl
import Amazonka.MediaConvert.Types.Eac3PhaseControl
import Amazonka.MediaConvert.Types.Eac3StereoDownmix
import Amazonka.MediaConvert.Types.Eac3SurroundExMode
import Amazonka.MediaConvert.Types.Eac3SurroundMode
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
--
-- /See:/ 'newEac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { -- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
    -- channels. Only used for 3\/2 coding mode.
    attenuationControl :: Prelude.Maybe Eac3AttenuationControl,
    -- | Specify the average bitrate in bits per second. The bitrate that you
    -- specify must be a multiple of 8000 within the allowed minimum and
    -- maximum values. Leave blank to use the default bitrate for the coding
    -- mode you select according ETSI TS 102 366. Valid bitrates for coding
    -- mode 1\/0: Default: 96000. Minimum: 32000. Maximum: 3024000. Valid
    -- bitrates for coding mode 2\/0: Default: 192000. Minimum: 96000. Maximum:
    -- 3024000. Valid bitrates for coding mode 3\/2: Default: 384000. Minimum:
    -- 192000. Maximum: 3024000.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
    -- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Prelude.Maybe Eac3BitstreamMode,
    -- | Dolby Digital Plus coding mode. Determines number of channels.
    codingMode :: Prelude.Maybe Eac3CodingMode,
    -- | Activates a DC highpass filter for all input channels.
    dcFilter :: Prelude.Maybe Eac3DcFilter,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital Plus, dialnorm will be passed through.
    dialnorm :: Prelude.Maybe Prelude.Natural,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the line operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionLine :: Prelude.Maybe Eac3DynamicRangeCompressionLine,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the RF operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionRf :: Prelude.Maybe Eac3DynamicRangeCompressionRf,
    -- | When encoding 3\/2 audio, controls whether the LFE channel is enabled
    lfeControl :: Prelude.Maybe Eac3LfeControl,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
    -- Only valid with 3_2_LFE coding mode.
    lfeFilter :: Prelude.Maybe Eac3LfeFilter,
    -- | Specify a value for the following Dolby Digital Plus setting: Left
    -- only\/Right only center mix (Lo\/Ro center). MediaConvert uses this
    -- value for downmixing. How the service uses this value depends on the
    -- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
    -- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
    -- mutes the channel. This setting applies only if you keep the default
    -- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
    -- mode (Eac3CodingMode). If you choose a different value for Coding mode,
    -- the service ignores Left only\/Right only center (loRoCenterMixLevel).
    loRoCenterMixLevel :: Prelude.Maybe Prelude.Double,
    -- | Specify a value for the following Dolby Digital Plus setting: Left
    -- only\/Right only (Lo\/Ro surround). MediaConvert uses this value for
    -- downmixing. How the service uses this value depends on the value that
    -- you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5,
    -- -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting
    -- applies only if you keep the default value of 3\/2 - L, R, C, Ls, Rs
    -- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
    -- choose a different value for Coding mode, the service ignores Left
    -- only\/Right only surround (loRoSurroundMixLevel).
    loRoSurroundMixLevel :: Prelude.Maybe Prelude.Double,
    -- | Specify a value for the following Dolby Digital Plus setting: Left
    -- total\/Right total center mix (Lt\/Rt center). MediaConvert uses this
    -- value for downmixing. How the service uses this value depends on the
    -- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
    -- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
    -- mutes the channel. This setting applies only if you keep the default
    -- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
    -- mode (Eac3CodingMode). If you choose a different value for Coding mode,
    -- the service ignores Left total\/Right total center (ltRtCenterMixLevel).
    ltRtCenterMixLevel :: Prelude.Maybe Prelude.Double,
    -- | Specify a value for the following Dolby Digital Plus setting: Left
    -- total\/Right total surround mix (Lt\/Rt surround). MediaConvert uses
    -- this value for downmixing. How the service uses this value depends on
    -- the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
    -- values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the
    -- channel. This setting applies only if you keep the default value of 3\/2
    -- - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode
    -- (Eac3CodingMode). If you choose a different value for Coding mode, the
    -- service ignores Left total\/Right total surround (ltRtSurroundMixLevel).
    ltRtSurroundMixLevel :: Prelude.Maybe Prelude.Double,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
    -- DD+, or DolbyE decoder that supplied this audio data. If audio was not
    -- supplied from one of these streams, then the static metadata settings
    -- will be used.
    metadataControl :: Prelude.Maybe Eac3MetadataControl,
    -- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
    -- is present on the input. this detection is dynamic over the life of the
    -- transcode. Inputs that alternate between DD+ and non-DD+ content will
    -- have a consistent DD+ output as the system alternates between
    -- passthrough and encoding.
    passthroughControl :: Prelude.Maybe Eac3PassthroughControl,
    -- | Controls the amount of phase-shift applied to the surround channels.
    -- Only used for 3\/2 coding mode.
    phaseControl :: Prelude.Maybe Eac3PhaseControl,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Choose how the service does stereo downmixing. This setting only applies
    -- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
    -- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
    -- choose a different value for Coding mode, the service ignores Stereo
    -- downmix (Eac3StereoDownmix).
    stereoDownmix :: Prelude.Maybe Eac3StereoDownmix,
    -- | When encoding 3\/2 audio, sets whether an extra center back surround
    -- channel is matrix encoded into the left and right surround channels.
    surroundExMode :: Prelude.Maybe Eac3SurroundExMode,
    -- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
    -- into the two channels.
    surroundMode :: Prelude.Maybe Eac3SurroundMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Eac3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attenuationControl', 'eac3Settings_attenuationControl' - If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
--
-- 'bitrate', 'eac3Settings_bitrate' - Specify the average bitrate in bits per second. The bitrate that you
-- specify must be a multiple of 8000 within the allowed minimum and
-- maximum values. Leave blank to use the default bitrate for the coding
-- mode you select according ETSI TS 102 366. Valid bitrates for coding
-- mode 1\/0: Default: 96000. Minimum: 32000. Maximum: 3024000. Valid
-- bitrates for coding mode 2\/0: Default: 192000. Minimum: 96000. Maximum:
-- 3024000. Valid bitrates for coding mode 3\/2: Default: 384000. Minimum:
-- 192000. Maximum: 3024000.
--
-- 'bitstreamMode', 'eac3Settings_bitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'codingMode', 'eac3Settings_codingMode' - Dolby Digital Plus coding mode. Determines number of channels.
--
-- 'dcFilter', 'eac3Settings_dcFilter' - Activates a DC highpass filter for all input channels.
--
-- 'dialnorm', 'eac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
--
-- 'dynamicRangeCompressionLine', 'eac3Settings_dynamicRangeCompressionLine' - Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the line operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
--
-- 'dynamicRangeCompressionRf', 'eac3Settings_dynamicRangeCompressionRf' - Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the RF operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
--
-- 'lfeControl', 'eac3Settings_lfeControl' - When encoding 3\/2 audio, controls whether the LFE channel is enabled
--
-- 'lfeFilter', 'eac3Settings_lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
--
-- 'loRoCenterMixLevel', 'eac3Settings_loRoCenterMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left
-- only\/Right only center mix (Lo\/Ro center). MediaConvert uses this
-- value for downmixing. How the service uses this value depends on the
-- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
-- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
-- mutes the channel. This setting applies only if you keep the default
-- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
-- mode (Eac3CodingMode). If you choose a different value for Coding mode,
-- the service ignores Left only\/Right only center (loRoCenterMixLevel).
--
-- 'loRoSurroundMixLevel', 'eac3Settings_loRoSurroundMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left
-- only\/Right only (Lo\/Ro surround). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5,
-- -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting
-- applies only if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Left
-- only\/Right only surround (loRoSurroundMixLevel).
--
-- 'ltRtCenterMixLevel', 'eac3Settings_ltRtCenterMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left
-- total\/Right total center mix (Lt\/Rt center). MediaConvert uses this
-- value for downmixing. How the service uses this value depends on the
-- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
-- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
-- mutes the channel. This setting applies only if you keep the default
-- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
-- mode (Eac3CodingMode). If you choose a different value for Coding mode,
-- the service ignores Left total\/Right total center (ltRtCenterMixLevel).
--
-- 'ltRtSurroundMixLevel', 'eac3Settings_ltRtSurroundMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left
-- total\/Right total surround mix (Lt\/Rt surround). MediaConvert uses
-- this value for downmixing. How the service uses this value depends on
-- the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
-- values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the
-- channel. This setting applies only if you keep the default value of 3\/2
-- - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode
-- (Eac3CodingMode). If you choose a different value for Coding mode, the
-- service ignores Left total\/Right total surround (ltRtSurroundMixLevel).
--
-- 'metadataControl', 'eac3Settings_metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
--
-- 'passthroughControl', 'eac3Settings_passthroughControl' - When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
-- is present on the input. this detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
--
-- 'phaseControl', 'eac3Settings_phaseControl' - Controls the amount of phase-shift applied to the surround channels.
-- Only used for 3\/2 coding mode.
--
-- 'sampleRate', 'eac3Settings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- 'stereoDownmix', 'eac3Settings_stereoDownmix' - Choose how the service does stereo downmixing. This setting only applies
-- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Stereo
-- downmix (Eac3StereoDownmix).
--
-- 'surroundExMode', 'eac3Settings_surroundExMode' - When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
--
-- 'surroundMode', 'eac3Settings_surroundMode' - When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
newEac3Settings ::
  Eac3Settings
newEac3Settings =
  Eac3Settings'
    { attenuationControl = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      bitstreamMode = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      dcFilter = Prelude.Nothing,
      dialnorm = Prelude.Nothing,
      dynamicRangeCompressionLine = Prelude.Nothing,
      dynamicRangeCompressionRf = Prelude.Nothing,
      lfeControl = Prelude.Nothing,
      lfeFilter = Prelude.Nothing,
      loRoCenterMixLevel = Prelude.Nothing,
      loRoSurroundMixLevel = Prelude.Nothing,
      ltRtCenterMixLevel = Prelude.Nothing,
      ltRtSurroundMixLevel = Prelude.Nothing,
      metadataControl = Prelude.Nothing,
      passthroughControl = Prelude.Nothing,
      phaseControl = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      stereoDownmix = Prelude.Nothing,
      surroundExMode = Prelude.Nothing,
      surroundMode = Prelude.Nothing
    }

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
eac3Settings_attenuationControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3AttenuationControl)
eac3Settings_attenuationControl = Lens.lens (\Eac3Settings' {attenuationControl} -> attenuationControl) (\s@Eac3Settings' {} a -> s {attenuationControl = a} :: Eac3Settings)

-- | Specify the average bitrate in bits per second. The bitrate that you
-- specify must be a multiple of 8000 within the allowed minimum and
-- maximum values. Leave blank to use the default bitrate for the coding
-- mode you select according ETSI TS 102 366. Valid bitrates for coding
-- mode 1\/0: Default: 96000. Minimum: 32000. Maximum: 3024000. Valid
-- bitrates for coding mode 2\/0: Default: 192000. Minimum: 96000. Maximum:
-- 3024000. Valid bitrates for coding mode 3\/2: Default: 384000. Minimum:
-- 192000. Maximum: 3024000.
eac3Settings_bitrate :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_bitrate = Lens.lens (\Eac3Settings' {bitrate} -> bitrate) (\s@Eac3Settings' {} a -> s {bitrate = a} :: Eac3Settings)

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
eac3Settings_bitstreamMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3BitstreamMode)
eac3Settings_bitstreamMode = Lens.lens (\Eac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Eac3Settings' {} a -> s {bitstreamMode = a} :: Eac3Settings)

-- | Dolby Digital Plus coding mode. Determines number of channels.
eac3Settings_codingMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3CodingMode)
eac3Settings_codingMode = Lens.lens (\Eac3Settings' {codingMode} -> codingMode) (\s@Eac3Settings' {} a -> s {codingMode = a} :: Eac3Settings)

-- | Activates a DC highpass filter for all input channels.
eac3Settings_dcFilter :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3DcFilter)
eac3Settings_dcFilter = Lens.lens (\Eac3Settings' {dcFilter} -> dcFilter) (\s@Eac3Settings' {} a -> s {dcFilter = a} :: Eac3Settings)

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
eac3Settings_dialnorm :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_dialnorm = Lens.lens (\Eac3Settings' {dialnorm} -> dialnorm) (\s@Eac3Settings' {} a -> s {dialnorm = a} :: Eac3Settings)

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the line operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
eac3Settings_dynamicRangeCompressionLine :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3DynamicRangeCompressionLine)
eac3Settings_dynamicRangeCompressionLine = Lens.lens (\Eac3Settings' {dynamicRangeCompressionLine} -> dynamicRangeCompressionLine) (\s@Eac3Settings' {} a -> s {dynamicRangeCompressionLine = a} :: Eac3Settings)

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the RF operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
eac3Settings_dynamicRangeCompressionRf :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3DynamicRangeCompressionRf)
eac3Settings_dynamicRangeCompressionRf = Lens.lens (\Eac3Settings' {dynamicRangeCompressionRf} -> dynamicRangeCompressionRf) (\s@Eac3Settings' {} a -> s {dynamicRangeCompressionRf = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, controls whether the LFE channel is enabled
eac3Settings_lfeControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3LfeControl)
eac3Settings_lfeControl = Lens.lens (\Eac3Settings' {lfeControl} -> lfeControl) (\s@Eac3Settings' {} a -> s {lfeControl = a} :: Eac3Settings)

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
eac3Settings_lfeFilter :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3LfeFilter)
eac3Settings_lfeFilter = Lens.lens (\Eac3Settings' {lfeFilter} -> lfeFilter) (\s@Eac3Settings' {} a -> s {lfeFilter = a} :: Eac3Settings)

-- | Specify a value for the following Dolby Digital Plus setting: Left
-- only\/Right only center mix (Lo\/Ro center). MediaConvert uses this
-- value for downmixing. How the service uses this value depends on the
-- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
-- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
-- mutes the channel. This setting applies only if you keep the default
-- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
-- mode (Eac3CodingMode). If you choose a different value for Coding mode,
-- the service ignores Left only\/Right only center (loRoCenterMixLevel).
eac3Settings_loRoCenterMixLevel :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Double)
eac3Settings_loRoCenterMixLevel = Lens.lens (\Eac3Settings' {loRoCenterMixLevel} -> loRoCenterMixLevel) (\s@Eac3Settings' {} a -> s {loRoCenterMixLevel = a} :: Eac3Settings)

-- | Specify a value for the following Dolby Digital Plus setting: Left
-- only\/Right only (Lo\/Ro surround). MediaConvert uses this value for
-- downmixing. How the service uses this value depends on the value that
-- you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5,
-- -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting
-- applies only if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Left
-- only\/Right only surround (loRoSurroundMixLevel).
eac3Settings_loRoSurroundMixLevel :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Double)
eac3Settings_loRoSurroundMixLevel = Lens.lens (\Eac3Settings' {loRoSurroundMixLevel} -> loRoSurroundMixLevel) (\s@Eac3Settings' {} a -> s {loRoSurroundMixLevel = a} :: Eac3Settings)

-- | Specify a value for the following Dolby Digital Plus setting: Left
-- total\/Right total center mix (Lt\/Rt center). MediaConvert uses this
-- value for downmixing. How the service uses this value depends on the
-- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
-- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
-- mutes the channel. This setting applies only if you keep the default
-- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
-- mode (Eac3CodingMode). If you choose a different value for Coding mode,
-- the service ignores Left total\/Right total center (ltRtCenterMixLevel).
eac3Settings_ltRtCenterMixLevel :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Double)
eac3Settings_ltRtCenterMixLevel = Lens.lens (\Eac3Settings' {ltRtCenterMixLevel} -> ltRtCenterMixLevel) (\s@Eac3Settings' {} a -> s {ltRtCenterMixLevel = a} :: Eac3Settings)

-- | Specify a value for the following Dolby Digital Plus setting: Left
-- total\/Right total surround mix (Lt\/Rt surround). MediaConvert uses
-- this value for downmixing. How the service uses this value depends on
-- the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
-- values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the
-- channel. This setting applies only if you keep the default value of 3\/2
-- - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode
-- (Eac3CodingMode). If you choose a different value for Coding mode, the
-- service ignores Left total\/Right total surround (ltRtSurroundMixLevel).
eac3Settings_ltRtSurroundMixLevel :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Double)
eac3Settings_ltRtSurroundMixLevel = Lens.lens (\Eac3Settings' {ltRtSurroundMixLevel} -> ltRtSurroundMixLevel) (\s@Eac3Settings' {} a -> s {ltRtSurroundMixLevel = a} :: Eac3Settings)

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
eac3Settings_metadataControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3MetadataControl)
eac3Settings_metadataControl = Lens.lens (\Eac3Settings' {metadataControl} -> metadataControl) (\s@Eac3Settings' {} a -> s {metadataControl = a} :: Eac3Settings)

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
-- is present on the input. this detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
eac3Settings_passthroughControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3PassthroughControl)
eac3Settings_passthroughControl = Lens.lens (\Eac3Settings' {passthroughControl} -> passthroughControl) (\s@Eac3Settings' {} a -> s {passthroughControl = a} :: Eac3Settings)

-- | Controls the amount of phase-shift applied to the surround channels.
-- Only used for 3\/2 coding mode.
eac3Settings_phaseControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3PhaseControl)
eac3Settings_phaseControl = Lens.lens (\Eac3Settings' {phaseControl} -> phaseControl) (\s@Eac3Settings' {} a -> s {phaseControl = a} :: Eac3Settings)

-- | This value is always 48000. It represents the sample rate in Hz.
eac3Settings_sampleRate :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_sampleRate = Lens.lens (\Eac3Settings' {sampleRate} -> sampleRate) (\s@Eac3Settings' {} a -> s {sampleRate = a} :: Eac3Settings)

-- | Choose how the service does stereo downmixing. This setting only applies
-- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Stereo
-- downmix (Eac3StereoDownmix).
eac3Settings_stereoDownmix :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3StereoDownmix)
eac3Settings_stereoDownmix = Lens.lens (\Eac3Settings' {stereoDownmix} -> stereoDownmix) (\s@Eac3Settings' {} a -> s {stereoDownmix = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
eac3Settings_surroundExMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3SurroundExMode)
eac3Settings_surroundExMode = Lens.lens (\Eac3Settings' {surroundExMode} -> surroundExMode) (\s@Eac3Settings' {} a -> s {surroundExMode = a} :: Eac3Settings)

-- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
eac3Settings_surroundMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3SurroundMode)
eac3Settings_surroundMode = Lens.lens (\Eac3Settings' {surroundMode} -> surroundMode) (\s@Eac3Settings' {} a -> s {surroundMode = a} :: Eac3Settings)

instance Data.FromJSON Eac3Settings where
  parseJSON =
    Data.withObject
      "Eac3Settings"
      ( \x ->
          Eac3Settings'
            Prelude.<$> (x Data..:? "attenuationControl")
            Prelude.<*> (x Data..:? "bitrate")
            Prelude.<*> (x Data..:? "bitstreamMode")
            Prelude.<*> (x Data..:? "codingMode")
            Prelude.<*> (x Data..:? "dcFilter")
            Prelude.<*> (x Data..:? "dialnorm")
            Prelude.<*> (x Data..:? "dynamicRangeCompressionLine")
            Prelude.<*> (x Data..:? "dynamicRangeCompressionRf")
            Prelude.<*> (x Data..:? "lfeControl")
            Prelude.<*> (x Data..:? "lfeFilter")
            Prelude.<*> (x Data..:? "loRoCenterMixLevel")
            Prelude.<*> (x Data..:? "loRoSurroundMixLevel")
            Prelude.<*> (x Data..:? "ltRtCenterMixLevel")
            Prelude.<*> (x Data..:? "ltRtSurroundMixLevel")
            Prelude.<*> (x Data..:? "metadataControl")
            Prelude.<*> (x Data..:? "passthroughControl")
            Prelude.<*> (x Data..:? "phaseControl")
            Prelude.<*> (x Data..:? "sampleRate")
            Prelude.<*> (x Data..:? "stereoDownmix")
            Prelude.<*> (x Data..:? "surroundExMode")
            Prelude.<*> (x Data..:? "surroundMode")
      )

instance Prelude.Hashable Eac3Settings where
  hashWithSalt _salt Eac3Settings' {..} =
    _salt
      `Prelude.hashWithSalt` attenuationControl
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` bitstreamMode
      `Prelude.hashWithSalt` codingMode
      `Prelude.hashWithSalt` dcFilter
      `Prelude.hashWithSalt` dialnorm
      `Prelude.hashWithSalt` dynamicRangeCompressionLine
      `Prelude.hashWithSalt` dynamicRangeCompressionRf
      `Prelude.hashWithSalt` lfeControl
      `Prelude.hashWithSalt` lfeFilter
      `Prelude.hashWithSalt` loRoCenterMixLevel
      `Prelude.hashWithSalt` loRoSurroundMixLevel
      `Prelude.hashWithSalt` ltRtCenterMixLevel
      `Prelude.hashWithSalt` ltRtSurroundMixLevel
      `Prelude.hashWithSalt` metadataControl
      `Prelude.hashWithSalt` passthroughControl
      `Prelude.hashWithSalt` phaseControl
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` stereoDownmix
      `Prelude.hashWithSalt` surroundExMode
      `Prelude.hashWithSalt` surroundMode

instance Prelude.NFData Eac3Settings where
  rnf Eac3Settings' {..} =
    Prelude.rnf attenuationControl `Prelude.seq`
      Prelude.rnf bitrate `Prelude.seq`
        Prelude.rnf bitstreamMode `Prelude.seq`
          Prelude.rnf codingMode `Prelude.seq`
            Prelude.rnf dcFilter `Prelude.seq`
              Prelude.rnf dialnorm `Prelude.seq`
                Prelude.rnf dynamicRangeCompressionLine `Prelude.seq`
                  Prelude.rnf dynamicRangeCompressionRf `Prelude.seq`
                    Prelude.rnf lfeControl `Prelude.seq`
                      Prelude.rnf lfeFilter `Prelude.seq`
                        Prelude.rnf loRoCenterMixLevel `Prelude.seq`
                          Prelude.rnf loRoSurroundMixLevel `Prelude.seq`
                            Prelude.rnf ltRtCenterMixLevel `Prelude.seq`
                              Prelude.rnf ltRtSurroundMixLevel `Prelude.seq`
                                Prelude.rnf metadataControl `Prelude.seq`
                                  Prelude.rnf passthroughControl `Prelude.seq`
                                    Prelude.rnf phaseControl `Prelude.seq`
                                      Prelude.rnf sampleRate `Prelude.seq`
                                        Prelude.rnf stereoDownmix `Prelude.seq`
                                          Prelude.rnf surroundExMode `Prelude.seq`
                                            Prelude.rnf surroundMode

instance Data.ToJSON Eac3Settings where
  toJSON Eac3Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attenuationControl" Data..=)
              Prelude.<$> attenuationControl,
            ("bitrate" Data..=) Prelude.<$> bitrate,
            ("bitstreamMode" Data..=) Prelude.<$> bitstreamMode,
            ("codingMode" Data..=) Prelude.<$> codingMode,
            ("dcFilter" Data..=) Prelude.<$> dcFilter,
            ("dialnorm" Data..=) Prelude.<$> dialnorm,
            ("dynamicRangeCompressionLine" Data..=)
              Prelude.<$> dynamicRangeCompressionLine,
            ("dynamicRangeCompressionRf" Data..=)
              Prelude.<$> dynamicRangeCompressionRf,
            ("lfeControl" Data..=) Prelude.<$> lfeControl,
            ("lfeFilter" Data..=) Prelude.<$> lfeFilter,
            ("loRoCenterMixLevel" Data..=)
              Prelude.<$> loRoCenterMixLevel,
            ("loRoSurroundMixLevel" Data..=)
              Prelude.<$> loRoSurroundMixLevel,
            ("ltRtCenterMixLevel" Data..=)
              Prelude.<$> ltRtCenterMixLevel,
            ("ltRtSurroundMixLevel" Data..=)
              Prelude.<$> ltRtSurroundMixLevel,
            ("metadataControl" Data..=)
              Prelude.<$> metadataControl,
            ("passthroughControl" Data..=)
              Prelude.<$> passthroughControl,
            ("phaseControl" Data..=) Prelude.<$> phaseControl,
            ("sampleRate" Data..=) Prelude.<$> sampleRate,
            ("stereoDownmix" Data..=) Prelude.<$> stereoDownmix,
            ("surroundExMode" Data..=)
              Prelude.<$> surroundExMode,
            ("surroundMode" Data..=) Prelude.<$> surroundMode
          ]
      )
