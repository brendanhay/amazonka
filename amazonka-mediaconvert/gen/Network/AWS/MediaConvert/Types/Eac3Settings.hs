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
-- Module      : Network.AWS.MediaConvert.Types.Eac3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3Settings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Eac3AttenuationControl
import Network.AWS.MediaConvert.Types.Eac3BitstreamMode
import Network.AWS.MediaConvert.Types.Eac3CodingMode
import Network.AWS.MediaConvert.Types.Eac3DcFilter
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3LfeControl
import Network.AWS.MediaConvert.Types.Eac3LfeFilter
import Network.AWS.MediaConvert.Types.Eac3MetadataControl
import Network.AWS.MediaConvert.Types.Eac3PassthroughControl
import Network.AWS.MediaConvert.Types.Eac3PhaseControl
import Network.AWS.MediaConvert.Types.Eac3StereoDownmix
import Network.AWS.MediaConvert.Types.Eac3SurroundExMode
import Network.AWS.MediaConvert.Types.Eac3SurroundMode
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
--
-- /See:/ 'newEac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { -- | Specify a value for the following Dolby Digital Plus setting: Left
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
    -- total\/Right total center mix (Lt\/Rt center). MediaConvert uses this
    -- value for downmixing. How the service uses this value depends on the
    -- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
    -- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
    -- mutes the channel. This setting applies only if you keep the default
    -- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
    -- mode (Eac3CodingMode). If you choose a different value for Coding mode,
    -- the service ignores Left total\/Right total center (ltRtCenterMixLevel).
    ltRtCenterMixLevel :: Prelude.Maybe Prelude.Double,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital Plus, dialnorm will be passed through.
    dialnorm :: Prelude.Maybe Prelude.Natural,
    -- | Dolby Digital Plus coding mode. Determines number of channels.
    codingMode :: Prelude.Maybe Eac3CodingMode,
    -- | When encoding 3\/2 audio, controls whether the LFE channel is enabled
    lfeControl :: Prelude.Maybe Eac3LfeControl,
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
    -- total\/Right total surround mix (Lt\/Rt surround). MediaConvert uses
    -- this value for downmixing. How the service uses this value depends on
    -- the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
    -- values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the
    -- channel. This setting applies only if you keep the default value of 3\/2
    -- - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode
    -- (Eac3CodingMode). If you choose a different value for Coding mode, the
    -- service ignores Left total\/Right total surround (ltRtSurroundMixLevel).
    ltRtSurroundMixLevel :: Prelude.Maybe Prelude.Double,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
    -- Only valid with 3_2_LFE coding mode.
    lfeFilter :: Prelude.Maybe Eac3LfeFilter,
    -- | Activates a DC highpass filter for all input channels.
    dcFilter :: Prelude.Maybe Eac3DcFilter,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Choose how the service does stereo downmixing. This setting only applies
    -- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
    -- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
    -- choose a different value for Coding mode, the service ignores Stereo
    -- downmix (Eac3StereoDownmix).
    stereoDownmix :: Prelude.Maybe Eac3StereoDownmix,
    -- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
    -- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Prelude.Maybe Eac3BitstreamMode,
    -- | When encoding 3\/2 audio, sets whether an extra center back surround
    -- channel is matrix encoded into the left and right surround channels.
    surroundExMode :: Prelude.Maybe Eac3SurroundExMode,
    -- | Controls the amount of phase-shift applied to the surround channels.
    -- Only used for 3\/2 coding mode.
    phaseControl :: Prelude.Maybe Eac3PhaseControl,
    -- | Specify how the service limits the audio dynamic range when compressing
    -- the audio.
    dynamicRangeCompressionRf :: Prelude.Maybe Eac3DynamicRangeCompressionRf,
    -- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
    -- is present on the input. this detection is dynamic over the life of the
    -- transcode. Inputs that alternate between DD+ and non-DD+ content will
    -- have a consistent DD+ output as the system alternates between
    -- passthrough and encoding.
    passthroughControl :: Prelude.Maybe Eac3PassthroughControl,
    -- | Specify the average bitrate in bits per second. Valid bitrates depend on
    -- the coding mode.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
    -- channels. Only used for 3\/2 coding mode.
    attenuationControl :: Prelude.Maybe Eac3AttenuationControl,
    -- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
    -- into the two channels.
    surroundMode :: Prelude.Maybe Eac3SurroundMode,
    -- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
    -- DD+, or DolbyE decoder that supplied this audio data. If audio was not
    -- supplied from one of these streams, then the static metadata settings
    -- will be used.
    metadataControl :: Prelude.Maybe Eac3MetadataControl,
    -- | Specify the absolute peak level for a signal with dynamic range
    -- compression.
    dynamicRangeCompressionLine :: Prelude.Maybe Eac3DynamicRangeCompressionLine
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
-- 'dialnorm', 'eac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
--
-- 'codingMode', 'eac3Settings_codingMode' - Dolby Digital Plus coding mode. Determines number of channels.
--
-- 'lfeControl', 'eac3Settings_lfeControl' - When encoding 3\/2 audio, controls whether the LFE channel is enabled
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
-- 'lfeFilter', 'eac3Settings_lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
--
-- 'dcFilter', 'eac3Settings_dcFilter' - Activates a DC highpass filter for all input channels.
--
-- 'sampleRate', 'eac3Settings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- 'stereoDownmix', 'eac3Settings_stereoDownmix' - Choose how the service does stereo downmixing. This setting only applies
-- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Stereo
-- downmix (Eac3StereoDownmix).
--
-- 'bitstreamMode', 'eac3Settings_bitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'surroundExMode', 'eac3Settings_surroundExMode' - When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
--
-- 'phaseControl', 'eac3Settings_phaseControl' - Controls the amount of phase-shift applied to the surround channels.
-- Only used for 3\/2 coding mode.
--
-- 'dynamicRangeCompressionRf', 'eac3Settings_dynamicRangeCompressionRf' - Specify how the service limits the audio dynamic range when compressing
-- the audio.
--
-- 'passthroughControl', 'eac3Settings_passthroughControl' - When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
-- is present on the input. this detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
--
-- 'bitrate', 'eac3Settings_bitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
--
-- 'attenuationControl', 'eac3Settings_attenuationControl' - If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
--
-- 'surroundMode', 'eac3Settings_surroundMode' - When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
--
-- 'metadataControl', 'eac3Settings_metadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
--
-- 'dynamicRangeCompressionLine', 'eac3Settings_dynamicRangeCompressionLine' - Specify the absolute peak level for a signal with dynamic range
-- compression.
newEac3Settings ::
  Eac3Settings
newEac3Settings =
  Eac3Settings'
    { loRoCenterMixLevel = Prelude.Nothing,
      ltRtCenterMixLevel = Prelude.Nothing,
      dialnorm = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      lfeControl = Prelude.Nothing,
      loRoSurroundMixLevel = Prelude.Nothing,
      ltRtSurroundMixLevel = Prelude.Nothing,
      lfeFilter = Prelude.Nothing,
      dcFilter = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      stereoDownmix = Prelude.Nothing,
      bitstreamMode = Prelude.Nothing,
      surroundExMode = Prelude.Nothing,
      phaseControl = Prelude.Nothing,
      dynamicRangeCompressionRf = Prelude.Nothing,
      passthroughControl = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      attenuationControl = Prelude.Nothing,
      surroundMode = Prelude.Nothing,
      metadataControl = Prelude.Nothing,
      dynamicRangeCompressionLine = Prelude.Nothing
    }

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

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
eac3Settings_dialnorm :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_dialnorm = Lens.lens (\Eac3Settings' {dialnorm} -> dialnorm) (\s@Eac3Settings' {} a -> s {dialnorm = a} :: Eac3Settings)

-- | Dolby Digital Plus coding mode. Determines number of channels.
eac3Settings_codingMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3CodingMode)
eac3Settings_codingMode = Lens.lens (\Eac3Settings' {codingMode} -> codingMode) (\s@Eac3Settings' {} a -> s {codingMode = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, controls whether the LFE channel is enabled
eac3Settings_lfeControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3LfeControl)
eac3Settings_lfeControl = Lens.lens (\Eac3Settings' {lfeControl} -> lfeControl) (\s@Eac3Settings' {} a -> s {lfeControl = a} :: Eac3Settings)

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

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
eac3Settings_lfeFilter :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3LfeFilter)
eac3Settings_lfeFilter = Lens.lens (\Eac3Settings' {lfeFilter} -> lfeFilter) (\s@Eac3Settings' {} a -> s {lfeFilter = a} :: Eac3Settings)

-- | Activates a DC highpass filter for all input channels.
eac3Settings_dcFilter :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3DcFilter)
eac3Settings_dcFilter = Lens.lens (\Eac3Settings' {dcFilter} -> dcFilter) (\s@Eac3Settings' {} a -> s {dcFilter = a} :: Eac3Settings)

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

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
eac3Settings_bitstreamMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3BitstreamMode)
eac3Settings_bitstreamMode = Lens.lens (\Eac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Eac3Settings' {} a -> s {bitstreamMode = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
eac3Settings_surroundExMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3SurroundExMode)
eac3Settings_surroundExMode = Lens.lens (\Eac3Settings' {surroundExMode} -> surroundExMode) (\s@Eac3Settings' {} a -> s {surroundExMode = a} :: Eac3Settings)

-- | Controls the amount of phase-shift applied to the surround channels.
-- Only used for 3\/2 coding mode.
eac3Settings_phaseControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3PhaseControl)
eac3Settings_phaseControl = Lens.lens (\Eac3Settings' {phaseControl} -> phaseControl) (\s@Eac3Settings' {} a -> s {phaseControl = a} :: Eac3Settings)

-- | Specify how the service limits the audio dynamic range when compressing
-- the audio.
eac3Settings_dynamicRangeCompressionRf :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3DynamicRangeCompressionRf)
eac3Settings_dynamicRangeCompressionRf = Lens.lens (\Eac3Settings' {dynamicRangeCompressionRf} -> dynamicRangeCompressionRf) (\s@Eac3Settings' {} a -> s {dynamicRangeCompressionRf = a} :: Eac3Settings)

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
-- is present on the input. this detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
eac3Settings_passthroughControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3PassthroughControl)
eac3Settings_passthroughControl = Lens.lens (\Eac3Settings' {passthroughControl} -> passthroughControl) (\s@Eac3Settings' {} a -> s {passthroughControl = a} :: Eac3Settings)

-- | Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
eac3Settings_bitrate :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_bitrate = Lens.lens (\Eac3Settings' {bitrate} -> bitrate) (\s@Eac3Settings' {} a -> s {bitrate = a} :: Eac3Settings)

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
eac3Settings_attenuationControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3AttenuationControl)
eac3Settings_attenuationControl = Lens.lens (\Eac3Settings' {attenuationControl} -> attenuationControl) (\s@Eac3Settings' {} a -> s {attenuationControl = a} :: Eac3Settings)

-- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
eac3Settings_surroundMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3SurroundMode)
eac3Settings_surroundMode = Lens.lens (\Eac3Settings' {surroundMode} -> surroundMode) (\s@Eac3Settings' {} a -> s {surroundMode = a} :: Eac3Settings)

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD,
-- DD+, or DolbyE decoder that supplied this audio data. If audio was not
-- supplied from one of these streams, then the static metadata settings
-- will be used.
eac3Settings_metadataControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3MetadataControl)
eac3Settings_metadataControl = Lens.lens (\Eac3Settings' {metadataControl} -> metadataControl) (\s@Eac3Settings' {} a -> s {metadataControl = a} :: Eac3Settings)

-- | Specify the absolute peak level for a signal with dynamic range
-- compression.
eac3Settings_dynamicRangeCompressionLine :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3DynamicRangeCompressionLine)
eac3Settings_dynamicRangeCompressionLine = Lens.lens (\Eac3Settings' {dynamicRangeCompressionLine} -> dynamicRangeCompressionLine) (\s@Eac3Settings' {} a -> s {dynamicRangeCompressionLine = a} :: Eac3Settings)

instance Core.FromJSON Eac3Settings where
  parseJSON =
    Core.withObject
      "Eac3Settings"
      ( \x ->
          Eac3Settings'
            Prelude.<$> (x Core..:? "loRoCenterMixLevel")
            Prelude.<*> (x Core..:? "ltRtCenterMixLevel")
            Prelude.<*> (x Core..:? "dialnorm")
            Prelude.<*> (x Core..:? "codingMode")
            Prelude.<*> (x Core..:? "lfeControl")
            Prelude.<*> (x Core..:? "loRoSurroundMixLevel")
            Prelude.<*> (x Core..:? "ltRtSurroundMixLevel")
            Prelude.<*> (x Core..:? "lfeFilter")
            Prelude.<*> (x Core..:? "dcFilter")
            Prelude.<*> (x Core..:? "sampleRate")
            Prelude.<*> (x Core..:? "stereoDownmix")
            Prelude.<*> (x Core..:? "bitstreamMode")
            Prelude.<*> (x Core..:? "surroundExMode")
            Prelude.<*> (x Core..:? "phaseControl")
            Prelude.<*> (x Core..:? "dynamicRangeCompressionRf")
            Prelude.<*> (x Core..:? "passthroughControl")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "attenuationControl")
            Prelude.<*> (x Core..:? "surroundMode")
            Prelude.<*> (x Core..:? "metadataControl")
            Prelude.<*> (x Core..:? "dynamicRangeCompressionLine")
      )

instance Prelude.Hashable Eac3Settings

instance Prelude.NFData Eac3Settings

instance Core.ToJSON Eac3Settings where
  toJSON Eac3Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("loRoCenterMixLevel" Core..=)
              Prelude.<$> loRoCenterMixLevel,
            ("ltRtCenterMixLevel" Core..=)
              Prelude.<$> ltRtCenterMixLevel,
            ("dialnorm" Core..=) Prelude.<$> dialnorm,
            ("codingMode" Core..=) Prelude.<$> codingMode,
            ("lfeControl" Core..=) Prelude.<$> lfeControl,
            ("loRoSurroundMixLevel" Core..=)
              Prelude.<$> loRoSurroundMixLevel,
            ("ltRtSurroundMixLevel" Core..=)
              Prelude.<$> ltRtSurroundMixLevel,
            ("lfeFilter" Core..=) Prelude.<$> lfeFilter,
            ("dcFilter" Core..=) Prelude.<$> dcFilter,
            ("sampleRate" Core..=) Prelude.<$> sampleRate,
            ("stereoDownmix" Core..=) Prelude.<$> stereoDownmix,
            ("bitstreamMode" Core..=) Prelude.<$> bitstreamMode,
            ("surroundExMode" Core..=)
              Prelude.<$> surroundExMode,
            ("phaseControl" Core..=) Prelude.<$> phaseControl,
            ("dynamicRangeCompressionRf" Core..=)
              Prelude.<$> dynamicRangeCompressionRf,
            ("passthroughControl" Core..=)
              Prelude.<$> passthroughControl,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("attenuationControl" Core..=)
              Prelude.<$> attenuationControl,
            ("surroundMode" Core..=) Prelude.<$> surroundMode,
            ("metadataControl" Core..=)
              Prelude.<$> metadataControl,
            ("dynamicRangeCompressionLine" Core..=)
              Prelude.<$> dynamicRangeCompressionLine
          ]
      )
