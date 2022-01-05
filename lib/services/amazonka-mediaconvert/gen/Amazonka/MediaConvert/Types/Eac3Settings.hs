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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | Choose how the service does stereo downmixing. This setting only applies
    -- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
    -- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
    -- choose a different value for Coding mode, the service ignores Stereo
    -- downmix (Eac3StereoDownmix).
    stereoDownmix :: Prelude.Maybe Eac3StereoDownmix,
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
    -- total\/Right total center mix (Lt\/Rt center). MediaConvert uses this
    -- value for downmixing. How the service uses this value depends on the
    -- value that you choose for Stereo downmix (Eac3StereoDownmix). Valid
    -- values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60
    -- mutes the channel. This setting applies only if you keep the default
    -- value of 3\/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding
    -- mode (Eac3CodingMode). If you choose a different value for Coding mode,
    -- the service ignores Left total\/Right total center (ltRtCenterMixLevel).
    ltRtCenterMixLevel :: Prelude.Maybe Prelude.Double,
    -- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
    -- Only valid with 3_2_LFE coding mode.
    lfeFilter :: Prelude.Maybe Eac3LfeFilter,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the line operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionLine :: Prelude.Maybe Eac3DynamicRangeCompressionLine,
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
    -- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
    -- into the two channels.
    surroundMode :: Prelude.Maybe Eac3SurroundMode,
    -- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
    -- channels. Only used for 3\/2 coding mode.
    attenuationControl :: Prelude.Maybe Eac3AttenuationControl,
    -- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
    -- is present on the input. this detection is dynamic over the life of the
    -- transcode. Inputs that alternate between DD+ and non-DD+ content will
    -- have a consistent DD+ output as the system alternates between
    -- passthrough and encoding.
    passthroughControl :: Prelude.Maybe Eac3PassthroughControl,
    -- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
    -- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
    -- (Annex E).
    bitstreamMode :: Prelude.Maybe Eac3BitstreamMode,
    -- | When encoding 3\/2 audio, controls whether the LFE channel is enabled
    lfeControl :: Prelude.Maybe Eac3LfeControl,
    -- | Choose the Dolby Digital dynamic range control (DRC) profile that
    -- MediaConvert uses when encoding the metadata in the Dolby Digital stream
    -- for the RF operating mode. Related setting: When you use this setting,
    -- MediaConvert ignores any value you provide for Dynamic range compression
    -- profile (DynamicRangeCompressionProfile). For information about the
    -- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
    -- Control chapter of the Dolby Metadata Guide at
    -- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
    dynamicRangeCompressionRf :: Prelude.Maybe Eac3DynamicRangeCompressionRf,
    -- | Dolby Digital Plus coding mode. Determines number of channels.
    codingMode :: Prelude.Maybe Eac3CodingMode,
    -- | This value is always 48000. It represents the sample rate in Hz.
    sampleRate :: Prelude.Maybe Prelude.Natural,
    -- | Activates a DC highpass filter for all input channels.
    dcFilter :: Prelude.Maybe Eac3DcFilter,
    -- | Specify the average bitrate in bits per second. Valid bitrates depend on
    -- the coding mode.
    bitrate :: Prelude.Maybe Prelude.Natural,
    -- | Controls the amount of phase-shift applied to the surround channels.
    -- Only used for 3\/2 coding mode.
    phaseControl :: Prelude.Maybe Eac3PhaseControl,
    -- | When encoding 3\/2 audio, sets whether an extra center back surround
    -- channel is matrix encoded into the left and right surround channels.
    surroundExMode :: Prelude.Maybe Eac3SurroundExMode,
    -- | Sets the dialnorm for the output. If blank and input audio is Dolby
    -- Digital Plus, dialnorm will be passed through.
    dialnorm :: Prelude.Maybe Prelude.Natural
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
-- 'stereoDownmix', 'eac3Settings_stereoDownmix' - Choose how the service does stereo downmixing. This setting only applies
-- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Stereo
-- downmix (Eac3StereoDownmix).
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
-- 'lfeFilter', 'eac3Settings_lfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
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
-- 'surroundMode', 'eac3Settings_surroundMode' - When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
--
-- 'attenuationControl', 'eac3Settings_attenuationControl' - If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
--
-- 'passthroughControl', 'eac3Settings_passthroughControl' - When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
-- is present on the input. this detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
--
-- 'bitstreamMode', 'eac3Settings_bitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
--
-- 'lfeControl', 'eac3Settings_lfeControl' - When encoding 3\/2 audio, controls whether the LFE channel is enabled
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
-- 'codingMode', 'eac3Settings_codingMode' - Dolby Digital Plus coding mode. Determines number of channels.
--
-- 'sampleRate', 'eac3Settings_sampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- 'dcFilter', 'eac3Settings_dcFilter' - Activates a DC highpass filter for all input channels.
--
-- 'bitrate', 'eac3Settings_bitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
--
-- 'phaseControl', 'eac3Settings_phaseControl' - Controls the amount of phase-shift applied to the surround channels.
-- Only used for 3\/2 coding mode.
--
-- 'surroundExMode', 'eac3Settings_surroundExMode' - When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
--
-- 'dialnorm', 'eac3Settings_dialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
newEac3Settings ::
  Eac3Settings
newEac3Settings =
  Eac3Settings'
    { stereoDownmix = Prelude.Nothing,
      loRoCenterMixLevel = Prelude.Nothing,
      ltRtCenterMixLevel = Prelude.Nothing,
      lfeFilter = Prelude.Nothing,
      dynamicRangeCompressionLine = Prelude.Nothing,
      ltRtSurroundMixLevel = Prelude.Nothing,
      metadataControl = Prelude.Nothing,
      loRoSurroundMixLevel = Prelude.Nothing,
      surroundMode = Prelude.Nothing,
      attenuationControl = Prelude.Nothing,
      passthroughControl = Prelude.Nothing,
      bitstreamMode = Prelude.Nothing,
      lfeControl = Prelude.Nothing,
      dynamicRangeCompressionRf = Prelude.Nothing,
      codingMode = Prelude.Nothing,
      sampleRate = Prelude.Nothing,
      dcFilter = Prelude.Nothing,
      bitrate = Prelude.Nothing,
      phaseControl = Prelude.Nothing,
      surroundExMode = Prelude.Nothing,
      dialnorm = Prelude.Nothing
    }

-- | Choose how the service does stereo downmixing. This setting only applies
-- if you keep the default value of 3\/2 - L, R, C, Ls, Rs
-- (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you
-- choose a different value for Coding mode, the service ignores Stereo
-- downmix (Eac3StereoDownmix).
eac3Settings_stereoDownmix :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3StereoDownmix)
eac3Settings_stereoDownmix = Lens.lens (\Eac3Settings' {stereoDownmix} -> stereoDownmix) (\s@Eac3Settings' {} a -> s {stereoDownmix = a} :: Eac3Settings)

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

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
eac3Settings_lfeFilter :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3LfeFilter)
eac3Settings_lfeFilter = Lens.lens (\Eac3Settings' {lfeFilter} -> lfeFilter) (\s@Eac3Settings' {} a -> s {lfeFilter = a} :: Eac3Settings)

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

-- | When encoding 2\/0 audio, sets whether Dolby Surround is matrix encoded
-- into the two channels.
eac3Settings_surroundMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3SurroundMode)
eac3Settings_surroundMode = Lens.lens (\Eac3Settings' {surroundMode} -> surroundMode) (\s@Eac3Settings' {} a -> s {surroundMode = a} :: Eac3Settings)

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround
-- channels. Only used for 3\/2 coding mode.
eac3Settings_attenuationControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3AttenuationControl)
eac3Settings_attenuationControl = Lens.lens (\Eac3Settings' {attenuationControl} -> attenuationControl) (\s@Eac3Settings' {} a -> s {attenuationControl = a} :: Eac3Settings)

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
-- is present on the input. this detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
eac3Settings_passthroughControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3PassthroughControl)
eac3Settings_passthroughControl = Lens.lens (\Eac3Settings' {passthroughControl} -> passthroughControl) (\s@Eac3Settings' {} a -> s {passthroughControl = a} :: Eac3Settings)

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits.
-- For more information about the EAC3 bitstream mode, see ATSC A\/52-2012
-- (Annex E).
eac3Settings_bitstreamMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3BitstreamMode)
eac3Settings_bitstreamMode = Lens.lens (\Eac3Settings' {bitstreamMode} -> bitstreamMode) (\s@Eac3Settings' {} a -> s {bitstreamMode = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, controls whether the LFE channel is enabled
eac3Settings_lfeControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3LfeControl)
eac3Settings_lfeControl = Lens.lens (\Eac3Settings' {lfeControl} -> lfeControl) (\s@Eac3Settings' {} a -> s {lfeControl = a} :: Eac3Settings)

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

-- | Dolby Digital Plus coding mode. Determines number of channels.
eac3Settings_codingMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3CodingMode)
eac3Settings_codingMode = Lens.lens (\Eac3Settings' {codingMode} -> codingMode) (\s@Eac3Settings' {} a -> s {codingMode = a} :: Eac3Settings)

-- | This value is always 48000. It represents the sample rate in Hz.
eac3Settings_sampleRate :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_sampleRate = Lens.lens (\Eac3Settings' {sampleRate} -> sampleRate) (\s@Eac3Settings' {} a -> s {sampleRate = a} :: Eac3Settings)

-- | Activates a DC highpass filter for all input channels.
eac3Settings_dcFilter :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3DcFilter)
eac3Settings_dcFilter = Lens.lens (\Eac3Settings' {dcFilter} -> dcFilter) (\s@Eac3Settings' {} a -> s {dcFilter = a} :: Eac3Settings)

-- | Specify the average bitrate in bits per second. Valid bitrates depend on
-- the coding mode.
eac3Settings_bitrate :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_bitrate = Lens.lens (\Eac3Settings' {bitrate} -> bitrate) (\s@Eac3Settings' {} a -> s {bitrate = a} :: Eac3Settings)

-- | Controls the amount of phase-shift applied to the surround channels.
-- Only used for 3\/2 coding mode.
eac3Settings_phaseControl :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3PhaseControl)
eac3Settings_phaseControl = Lens.lens (\Eac3Settings' {phaseControl} -> phaseControl) (\s@Eac3Settings' {} a -> s {phaseControl = a} :: Eac3Settings)

-- | When encoding 3\/2 audio, sets whether an extra center back surround
-- channel is matrix encoded into the left and right surround channels.
eac3Settings_surroundExMode :: Lens.Lens' Eac3Settings (Prelude.Maybe Eac3SurroundExMode)
eac3Settings_surroundExMode = Lens.lens (\Eac3Settings' {surroundExMode} -> surroundExMode) (\s@Eac3Settings' {} a -> s {surroundExMode = a} :: Eac3Settings)

-- | Sets the dialnorm for the output. If blank and input audio is Dolby
-- Digital Plus, dialnorm will be passed through.
eac3Settings_dialnorm :: Lens.Lens' Eac3Settings (Prelude.Maybe Prelude.Natural)
eac3Settings_dialnorm = Lens.lens (\Eac3Settings' {dialnorm} -> dialnorm) (\s@Eac3Settings' {} a -> s {dialnorm = a} :: Eac3Settings)

instance Core.FromJSON Eac3Settings where
  parseJSON =
    Core.withObject
      "Eac3Settings"
      ( \x ->
          Eac3Settings'
            Prelude.<$> (x Core..:? "stereoDownmix")
            Prelude.<*> (x Core..:? "loRoCenterMixLevel")
            Prelude.<*> (x Core..:? "ltRtCenterMixLevel")
            Prelude.<*> (x Core..:? "lfeFilter")
            Prelude.<*> (x Core..:? "dynamicRangeCompressionLine")
            Prelude.<*> (x Core..:? "ltRtSurroundMixLevel")
            Prelude.<*> (x Core..:? "metadataControl")
            Prelude.<*> (x Core..:? "loRoSurroundMixLevel")
            Prelude.<*> (x Core..:? "surroundMode")
            Prelude.<*> (x Core..:? "attenuationControl")
            Prelude.<*> (x Core..:? "passthroughControl")
            Prelude.<*> (x Core..:? "bitstreamMode")
            Prelude.<*> (x Core..:? "lfeControl")
            Prelude.<*> (x Core..:? "dynamicRangeCompressionRf")
            Prelude.<*> (x Core..:? "codingMode")
            Prelude.<*> (x Core..:? "sampleRate")
            Prelude.<*> (x Core..:? "dcFilter")
            Prelude.<*> (x Core..:? "bitrate")
            Prelude.<*> (x Core..:? "phaseControl")
            Prelude.<*> (x Core..:? "surroundExMode")
            Prelude.<*> (x Core..:? "dialnorm")
      )

instance Prelude.Hashable Eac3Settings where
  hashWithSalt _salt Eac3Settings' {..} =
    _salt `Prelude.hashWithSalt` stereoDownmix
      `Prelude.hashWithSalt` loRoCenterMixLevel
      `Prelude.hashWithSalt` ltRtCenterMixLevel
      `Prelude.hashWithSalt` lfeFilter
      `Prelude.hashWithSalt` dynamicRangeCompressionLine
      `Prelude.hashWithSalt` ltRtSurroundMixLevel
      `Prelude.hashWithSalt` metadataControl
      `Prelude.hashWithSalt` loRoSurroundMixLevel
      `Prelude.hashWithSalt` surroundMode
      `Prelude.hashWithSalt` attenuationControl
      `Prelude.hashWithSalt` passthroughControl
      `Prelude.hashWithSalt` bitstreamMode
      `Prelude.hashWithSalt` lfeControl
      `Prelude.hashWithSalt` dynamicRangeCompressionRf
      `Prelude.hashWithSalt` codingMode
      `Prelude.hashWithSalt` sampleRate
      `Prelude.hashWithSalt` dcFilter
      `Prelude.hashWithSalt` bitrate
      `Prelude.hashWithSalt` phaseControl
      `Prelude.hashWithSalt` surroundExMode
      `Prelude.hashWithSalt` dialnorm

instance Prelude.NFData Eac3Settings where
  rnf Eac3Settings' {..} =
    Prelude.rnf stereoDownmix
      `Prelude.seq` Prelude.rnf loRoCenterMixLevel
      `Prelude.seq` Prelude.rnf ltRtCenterMixLevel
      `Prelude.seq` Prelude.rnf lfeFilter
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionLine
      `Prelude.seq` Prelude.rnf ltRtSurroundMixLevel
      `Prelude.seq` Prelude.rnf metadataControl
      `Prelude.seq` Prelude.rnf loRoSurroundMixLevel
      `Prelude.seq` Prelude.rnf surroundMode
      `Prelude.seq` Prelude.rnf attenuationControl
      `Prelude.seq` Prelude.rnf passthroughControl
      `Prelude.seq` Prelude.rnf bitstreamMode
      `Prelude.seq` Prelude.rnf lfeControl
      `Prelude.seq` Prelude.rnf dynamicRangeCompressionRf
      `Prelude.seq` Prelude.rnf codingMode
      `Prelude.seq` Prelude.rnf sampleRate
      `Prelude.seq` Prelude.rnf dcFilter
      `Prelude.seq` Prelude.rnf bitrate
      `Prelude.seq` Prelude.rnf phaseControl
      `Prelude.seq` Prelude.rnf surroundExMode
      `Prelude.seq` Prelude.rnf dialnorm

instance Core.ToJSON Eac3Settings where
  toJSON Eac3Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stereoDownmix" Core..=) Prelude.<$> stereoDownmix,
            ("loRoCenterMixLevel" Core..=)
              Prelude.<$> loRoCenterMixLevel,
            ("ltRtCenterMixLevel" Core..=)
              Prelude.<$> ltRtCenterMixLevel,
            ("lfeFilter" Core..=) Prelude.<$> lfeFilter,
            ("dynamicRangeCompressionLine" Core..=)
              Prelude.<$> dynamicRangeCompressionLine,
            ("ltRtSurroundMixLevel" Core..=)
              Prelude.<$> ltRtSurroundMixLevel,
            ("metadataControl" Core..=)
              Prelude.<$> metadataControl,
            ("loRoSurroundMixLevel" Core..=)
              Prelude.<$> loRoSurroundMixLevel,
            ("surroundMode" Core..=) Prelude.<$> surroundMode,
            ("attenuationControl" Core..=)
              Prelude.<$> attenuationControl,
            ("passthroughControl" Core..=)
              Prelude.<$> passthroughControl,
            ("bitstreamMode" Core..=) Prelude.<$> bitstreamMode,
            ("lfeControl" Core..=) Prelude.<$> lfeControl,
            ("dynamicRangeCompressionRf" Core..=)
              Prelude.<$> dynamicRangeCompressionRf,
            ("codingMode" Core..=) Prelude.<$> codingMode,
            ("sampleRate" Core..=) Prelude.<$> sampleRate,
            ("dcFilter" Core..=) Prelude.<$> dcFilter,
            ("bitrate" Core..=) Prelude.<$> bitrate,
            ("phaseControl" Core..=) Prelude.<$> phaseControl,
            ("surroundExMode" Core..=)
              Prelude.<$> surroundExMode,
            ("dialnorm" Core..=) Prelude.<$> dialnorm
          ]
      )
